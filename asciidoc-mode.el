;;; asciidoc-mode.el --- Major mode for AsciiDoc markup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/asciidoc-mode
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: text, asciidoc, languages, tree-sitter

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A tree-sitter-based major mode for editing AsciiDoc files.
;;
;; This mode uses two tree-sitter parsers from
;; <https://github.com/cathaysia/tree-sitter-asciidoc>:
;;
;; - `asciidoc' for block-level structure (sections, lists, blocks)
;; - `asciidoc-inline' for inline formatting (bold, italic, links)
;;
;; Both parsers operate on the full buffer independently, similar to
;; the dual-parser pattern used by `markdown-ts-mode'.
;;
;; Features:
;; - Syntax highlighting for headings, inline markup, blocks, lists,
;;   attributes, admonitions, macros, and more
;; - Imenu support for section navigation
;; - Outline integration for folding
;; - Cross-reference and link navigation, including cross-file Antora and
;;   relative xref targets (RET, mouse, C-c C-o, or xref)
;; - Comment support (// line comments)
;;
;; Quick start:
;;   (asciidoc-install-grammars)   ; one-time setup
;;   ;; then open any .adoc file

;;; Code:

(require 'treesit)
(require 'subr-x)
(require 'outline)
(require 'xref)
(require 'cl-lib)
(require 'flymake)

;;; Customization

(defgroup asciidoc nil
  "Support for AsciiDoc markup."
  :group 'text
  :link '(url-link "https://github.com/bbatsov/asciidoc-mode"))

(defcustom asciidoc-fontify-code-blocks-natively 5000
  "Whether to fontify source blocks using the language's major mode.
When non-nil, the body of a `[source,LANG]' block is fontified with
LANG's major mode (the same highlighting that mode would apply).  An
integer value only fontifies blocks whose body is at most that many
characters, to avoid performance problems on very large blocks; a value
of t fontifies all blocks regardless of size.  When nil, source block
bodies keep the plain `font-lock-string-face' used for all verbatim
blocks."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "All blocks" t)
                 (integer :tag "Up to N characters"))
  :package-version '(asciidoc-mode . "0.3.0"))

(defcustom asciidoc-code-lang-modes
  '(("C" . c-mode)
    ("cpp" . c++-mode)
    ("C++" . c++-mode)
    ("bash" . sh-mode)
    ("shell" . sh-mode)
    ("elisp" . emacs-lisp-mode)
    ("json" . (js-json-mode json-ts-mode json-mode))
    ("ocaml" . (neocaml-mode tuareg-mode caml-mode))
    ("sqlite" . sql-mode))
  "Alist mapping AsciiDoc source languages to major modes.
Used by native source block fontification when the major mode cannot be
derived from the language name as LANG-mode.  The key is the language
string as it appears in the block (e.g. the `ruby' in `[source,ruby]').
The value is either a single major mode or a list of candidate modes
tried in order, the first defined one being used -- so a language can map
to a preferred mode with fallbacks.  For example `ocaml' maps to
`neocaml-mode', then `tuareg-mode', then `caml-mode'."
  :type '(alist :key-type string
                :value-type (choice (function :tag "Major mode")
                                    (repeat (function :tag "Major mode"))))
  :package-version '(asciidoc-mode . "0.3.0"))

(defcustom asciidoc-fontify-code-block-default-mode 'prog-mode
  "Fallback major mode for native source block fontification.
Used when a block has no language, or no major mode can be found for its
language.  The default, `prog-mode', applies no highlighting."
  :type 'function
  :package-version '(asciidoc-mode . "0.3.0"))

;;; Version

(defconst asciidoc-mode-version "0.4.0"
  "The current version of `asciidoc-mode'.")

;;; Grammar recipes

(defvar asciidoc-grammar-recipes
  '((asciidoc
     "https://github.com/cathaysia/tree-sitter-asciidoc"
     nil "tree-sitter-asciidoc/src")
    (asciidoc-inline
     "https://github.com/cathaysia/tree-sitter-asciidoc"
     nil "tree-sitter-asciidoc_inline/src"))
  "Tree-sitter grammar recipes for AsciiDoc.
Each entry has the form (LANG URL REVISION SOURCE-DIR CC C++).")

;;;###autoload
(defun asciidoc-install-grammars ()
  "Install the tree-sitter grammars needed by `asciidoc-mode'."
  (interactive)
  (let ((treesit-language-source-alist asciidoc-grammar-recipes))
    (dolist (recipe asciidoc-grammar-recipes)
      (let ((lang (car recipe)))
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar for %s..." lang)
          (treesit-install-language-grammar lang)
          (message "Installing tree-sitter grammar for %s...done" lang))))))

(defun asciidoc--ensure-grammars ()
  "Return non-nil if both AsciiDoc grammars are available."
  (and (treesit-available-p)
       (treesit-language-available-p 'asciidoc)
       (treesit-language-available-p 'asciidoc-inline)))

;;; Faces

(defface asciidoc-document-title-face
  '((t :inherit outline-1))
  "Face for AsciiDoc document title (= Title)."
  :group 'asciidoc)

(defface asciidoc-title-1-face
  '((t :inherit outline-2))
  "Face for AsciiDoc level-1 section title (== Title)."
  :group 'asciidoc)

(defface asciidoc-title-2-face
  '((t :inherit outline-3))
  "Face for AsciiDoc level-2 section title (=== Title)."
  :group 'asciidoc)

(defface asciidoc-title-3-face
  '((t :inherit outline-4))
  "Face for AsciiDoc level-3 section title (==== Title)."
  :group 'asciidoc)

(defface asciidoc-title-4-face
  '((t :inherit outline-5))
  "Face for AsciiDoc level-4 section title (===== Title)."
  :group 'asciidoc)

(defface asciidoc-title-5-face
  '((t :inherit outline-6))
  "Face for AsciiDoc level-5 section title (====== Title)."
  :group 'asciidoc)

(defface asciidoc-link-face
  '((t :inherit link))
  "Face for links and link text (autolinks, URL labels)."
  :group 'asciidoc)

(defface asciidoc-link-mouse-face
  '((t :inherit highlight :underline t))
  "Face used to highlight a navigable reference under the mouse pointer."
  :group 'asciidoc)

(defface asciidoc-cross-reference-face
  '((t :inherit link))
  "Face for internal cross-references (e.g. <<id>>).
Inherits `link' (like the `org-mode' and `markdown-mode' link faces)
since cross-references are navigable."
  :group 'asciidoc)

(defface asciidoc-anchor-face
  '((t :inherit shadow :overline t))
  "Face for anchor definitions (e.g. [[id]] and [#id]).
An anchor is a landmark rather than something you act on, so it fades
like other markup (matching `adoc-mode'); the overline keeps it findable."
  :group 'asciidoc)

(defface asciidoc-superscript-face
  '((t :height 0.8))
  "Face for superscript text (e.g. ^text^)."
  :group 'asciidoc)

(defface asciidoc-subscript-face
  '((t :height 0.8))
  "Face for subscript text (e.g. ~text~)."
  :group 'asciidoc)

(defface asciidoc-url-face
  '((t :inherit font-lock-string-face))
  "Face for URL and email targets (autolinks and standalone addresses).
Link text inside `[...]' uses `asciidoc-link-face' instead."
  :group 'asciidoc)

(defface asciidoc-metadata-key-face
  '((t :inherit font-lock-variable-name-face))
  "Face for document attribute names (the `author' in `:author: Jane')."
  :group 'asciidoc)

(defface asciidoc-metadata-value-face
  '((t :inherit font-lock-string-face))
  "Face for document attribute values (the `Jane' in `:author: Jane')."
  :group 'asciidoc)

(defface asciidoc-footnote-marker-face
  '((t :inherit font-lock-function-call-face))
  "Face for the `footnote'/`footnoteref' macro name."
  :group 'asciidoc)

(defface asciidoc-footnote-text-face
  '((t :inherit font-lock-comment-face))
  "Face for footnote body text.
Footnote bodies are secondary text, so they recede into the comment face,
matching `adoc-mode' and `markdown-mode'."
  :group 'asciidoc)

(defface asciidoc-code-face
  '((t :inherit (font-lock-string-face fixed-pitch)))
  "Face for inline monospace text and listing block bodies.
Inherits `fixed-pitch' so code renders in a monospaced font even when
the buffer uses a variable-pitch default."
  :group 'asciidoc)

(defface asciidoc-highlight-face
  '((t :inherit highlight))
  "Face for highlighted/marked spans (e.g. `#text#').
Asciidoctor renders a hash-delimited span with the default `highlight'
style (typically a yellow background)."
  :group 'asciidoc)

(defface asciidoc-strike-through-face
  '((t :strike-through t))
  "Face for strike-through role spans (`[.line-through]#text#')."
  :group 'asciidoc)

(defface asciidoc-underline-face
  '((t :underline t))
  "Face for underline role spans (`[.underline]#text#')."
  :group 'asciidoc)

(defface asciidoc-overline-face
  '((t :overline t))
  "Face for overline role spans (`[.overline]#text#')."
  :group 'asciidoc)

(defface asciidoc-markup-face
  '((t :inherit shadow))
  "Face for structural markup that should fade into the background.
Used for block delimiters (`----', `====', ...) and list markers (`*',
`1.', `::').  Inherits `shadow' so the prose stands out, matching
`adoc-mode' and the convention in `markdown-mode'."
  :group 'asciidoc)

;; Admonition faces.  Each paragraph admonition (`NOTE:', `TIP:', ...) gets a
;; color-coded label and a subtle whole-block background, like Asciidoctor's
;; rendered callouts.  Backgrounds use `:extend t' so they fill the line, and
;; carry light/dark variants since a fixed tint can't suit both.

(defface asciidoc-admonition-note-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the `NOTE:' admonition label." :group 'asciidoc)

(defface asciidoc-admonition-note-face
  '((((background light)) :background "#ddeaff" :extend t)
    (((background dark))  :background "#1b2638" :extend t))
  "Background face for the body of a `NOTE:' admonition." :group 'asciidoc)

(defface asciidoc-admonition-tip-label-face
  '((t :inherit success :weight bold))
  "Face for the `TIP:' admonition label." :group 'asciidoc)

(defface asciidoc-admonition-tip-face
  '((((background light)) :background "#defae4" :extend t)
    (((background dark))  :background "#16291c" :extend t))
  "Background face for the body of a `TIP:' admonition." :group 'asciidoc)

(defface asciidoc-admonition-important-label-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for the `IMPORTANT:' admonition label." :group 'asciidoc)

(defface asciidoc-admonition-important-face
  '((((background light)) :background "#f0e6ff" :extend t)
    (((background dark))  :background "#241a33" :extend t))
  "Background face for the body of an `IMPORTANT:' admonition."
  :group 'asciidoc)

(defface asciidoc-admonition-caution-label-face
  '((t :inherit warning :weight bold))
  "Face for the `CAUTION:' admonition label." :group 'asciidoc)

(defface asciidoc-admonition-caution-face
  '((((background light)) :background "#fff1e0" :extend t)
    (((background dark))  :background "#332617" :extend t))
  "Background face for the body of a `CAUTION:' admonition." :group 'asciidoc)

(defface asciidoc-admonition-warning-label-face
  '((t :inherit error :weight bold))
  "Face for the `WARNING:' admonition label." :group 'asciidoc)

(defface asciidoc-admonition-warning-face
  '((((background light)) :background "#ffe5e8" :extend t)
    (((background dark))  :background "#331a1f" :extend t))
  "Background face for the body of a `WARNING:' admonition." :group 'asciidoc)

(defcustom asciidoc-superscript-raise 0.4
  "How far to raise superscript text, as a fraction of line height.
Applied as a `display' \\='(raise ...) property on top of
`asciidoc-superscript-face'."
  :type 'number
  :group 'asciidoc
  :package-version '(asciidoc-mode . "0.4.0"))

(defcustom asciidoc-subscript-raise -0.25
  "How far to lower subscript text, as a fraction of line height.
Applied as a `display' \\='(raise ...) property on top of
`asciidoc-subscript-face'."
  :type 'number
  :group 'asciidoc
  :package-version '(asciidoc-mode . "0.4.0"))

;;; Font-lock

(defun asciidoc--fontify-raised-span (node override start end &rest _)
  "Fontify a `superscript'/`subscript' NODE: face it and raise or lower it.
The single-character `^'/`~' delimiters are left on the baseline; only the
inner content is shifted via a `display' \\='(raise ...) property.  START and
END bound the region being fontified; OVERRIDE is passed through from the
font-lock rule."
  (let* ((subp (equal (treesit-node-type node) "subscript"))
         (face (if subp 'asciidoc-subscript-face 'asciidoc-superscript-face))
         (raise (if subp asciidoc-subscript-raise asciidoc-superscript-raise))
         ;; The node spans the whole `^x^'/`~x~'; skip the one-character
         ;; opening and closing delimiters so only the content floats.
         (beg (max start (1+ (treesit-node-start node))))
         (fin (min end (1- (treesit-node-end node)))))
    (when (< beg fin)
      (treesit-fontify-with-override beg fin face override)
      (put-text-property beg fin 'display (list 'raise raise)))))

(defcustom asciidoc-role-face-alist
  '(("line-through" . asciidoc-strike-through-face)
    ("underline"    . asciidoc-underline-face)
    ("overline"     . asciidoc-overline-face))
  "Alist mapping AsciiDoc role names to faces for custom-style spans.
Recognised inside `[.role]#text#' (and the unconstrained `##' variant).
A span whose role is not listed keeps the default face."
  :type '(alist :key-type (string :tag "Role name")
                :value-type (face :tag "Face"))
  :group 'asciidoc
  :package-version '(asciidoc-mode . "0.4.0"))

(defun asciidoc--fontify-roled-span (node override start end &rest _)
  "Style the custom-style span NODE according to its role.
A role listed in `asciidoc-role-face-alist' (e.g. `underline') applies its
face to the span text.  An unrecognised role leaves the text unstyled,
since the span reuses the `highlight' node and would otherwise inherit the
mark face.  START, END and OVERRIDE come from the font-lock rule."
  (let* ((parent (treesit-node-parent node))
         (face (seq-some
                (lambda (role)
                  (cdr (assoc (treesit-node-text role t)
                              asciidoc-role-face-alist)))
                (treesit-filter-child
                 parent
                 (lambda (c) (equal (treesit-node-type c) "role")))))
         (beg (max start (treesit-node-start node)))
         (fin (min end (treesit-node-end node))))
    (when (< beg fin)
      (treesit-fontify-with-override beg fin (or face 'default) override))))

(defvar-keymap asciidoc-reference-map
  :doc "Keymap active on the text of a navigable reference.
Applied as a `keymap' text property by `asciidoc--fontify-reference'."
  "RET" #'asciidoc-follow-reference-at-point
  "<mouse-2>" #'asciidoc-follow-reference-at-point)

(defun asciidoc--fontify-reference (node _override start end &rest _)
  "Make the navigable reference NODE clickable.
Adds the keymap, mouse highlight and tooltip; the link/cross-reference
faces are applied by their own rules.  Non-navigable inline macros (e.g.
`image:', `kbd:') are skipped.  START and END bound the fontified region."
  (when (or (member (treesit-node-type node) '("xref" "autolink"))
            (member (asciidoc--node-field node "macro_name")
                    '("link" "mailto" "xref")))
    (let ((beg (max start (treesit-node-start node)))
          (fin (min end (treesit-node-end node))))
      (when (< beg fin)
        (add-text-properties
         beg fin
         (list 'keymap asciidoc-reference-map
               'mouse-face 'asciidoc-link-mouse-face
               'follow-link t
               'help-echo "mouse-1/RET: follow reference"))))))

(defvar asciidoc--font-lock-settings
  (treesit-font-lock-rules
   ;; Block-level rules (asciidoc parser)
   ;; Use :override t so block-level faces win over spurious inline
   ;; emphasis nodes (the inline parser misreads `*' list markers as
   ;; emphasis delimiters).
   :language 'asciidoc
   :override t
   :feature 'comment
   '((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :language 'asciidoc
   :override t
   :feature 'title
   ;; The grammar nests the header's `document_attr' entries inside
   ;; `document_title', so face only the title's marker and its own line --
   ;; not the whole node, which would bleed the title face onto the
   ;; `:name: value' attribute lines below the title.
   '((document_title (title_h0_marker) @asciidoc-document-title-face)
     (document_title (line) @asciidoc-document-title-face)
     (title1) @asciidoc-title-1-face
     (title2) @asciidoc-title-2-face
     (title3) @asciidoc-title-3-face
     (title4) @asciidoc-title-4-face
     (title5) @asciidoc-title-5-face)

   :language 'asciidoc
   :override t
   :feature 'block
   '((listing_block_body) @asciidoc-code-face
     (literal_block_body) @font-lock-string-face
     (ident_block_line) @font-lock-string-face
     (block_title) @font-lock-type-face
     (breaks) @font-lock-comment-delimiter-face
     (quoted_md_block) @font-lock-doc-face)

   :language 'asciidoc
   :override t
   :feature 'delimiter
   '((listing_block_start_marker) @asciidoc-markup-face
     (listing_block_end_marker) @asciidoc-markup-face
     (literal_block_marker) @asciidoc-markup-face
     (passthrough_block_marker) @asciidoc-markup-face
     (open_block_marker) @asciidoc-markup-face
     (quoted_block_start_marker) @asciidoc-markup-face
     (quoted_block_end_marker) @asciidoc-markup-face
     (delimited_block_start_marker) @asciidoc-markup-face
     (delimited_block_end_marker) @asciidoc-markup-face)

   :language 'asciidoc
   :override t
   :feature 'table
   '((table_block_marker) @asciidoc-markup-face
     (csv_table_block_marker) @asciidoc-markup-face
     (dsv_table_block_marker) @asciidoc-markup-face
     (table_cell_attr) @font-lock-preprocessor-face)

   :language 'asciidoc
   :override t
   :feature 'list
   ;; List markers fade like other structural markup.  Tokens that carry
   ;; meaning rather than just structure keep a distinct face: the checkbox
   ;; state (`[x]'/`[ ]') and callout numbers (`<1>').
   '((ordered_list_marker) @asciidoc-markup-face
     (unordered_list_marker) @asciidoc-markup-face
     (checked_list_marker_checked) @font-lock-constant-face
     (checked_list_marker_unchecked) @font-lock-constant-face
     (callout_list_marker) @font-lock-constant-face
     (callout_marker) @font-lock-constant-face
     ;; Description list (`term:: definition'): the term is a label, the
     ;; `::'/`:::' separator a marker like the other list markers.
     (description_list_item (term) @font-lock-keyword-face)
     (description_marker) @asciidoc-markup-face)

   :language 'asciidoc
   :override t
   :feature 'attribute
   '((document_attr (attr_name) @asciidoc-metadata-key-face)
     (document_attr (line) @asciidoc-metadata-value-face)
     (element_attr) @font-lock-preprocessor-face)

   :language 'asciidoc
   :override t
   :feature 'macro
   '((block_macro (block_macro_name) @font-lock-function-call-face)
     (block_macro (target) @font-lock-string-face))

   :language 'asciidoc
   :override t
   :feature 'metadata
   '((author_line) @font-lock-doc-face
     (revision_line) @font-lock-doc-face)

   ;; Inline rules (asciidoc-inline parser)
   :language 'asciidoc-inline
   :feature 'inline-markup
   '((emphasis) @bold
     (ltalic) @italic
     (monospace) @asciidoc-code-face
     (highlight) @asciidoc-highlight-face
     (superscript) @asciidoc--fontify-raised-span
     (subscript) @asciidoc--fontify-raised-span
     (passthrough) @font-lock-string-face)

   ;; A custom-style span (`[.role]#text#') reuses the `highlight' node for
   ;; its content, so the blanket rule above would render it with the mark
   ;; face.  Override it: the role itself is markup (like a block `[.role]'
   ;; attribute list, which uses the preprocessor face), and the span text
   ;; takes the role's own styling (underline, strike-through, ...) when the
   ;; role is known, otherwise stays neutral.
   :language 'asciidoc-inline
   :override t
   :feature 'inline-markup
   '((roled_text (role) @font-lock-preprocessor-face)
     (roled_text (highlight) @asciidoc--fontify-roled-span))

   :language 'asciidoc-inline
   :feature 'inline-link
   '((autolink) @asciidoc--fontify-reference
     (link_url) @asciidoc-url-face
     (email) @asciidoc-url-face
     (xref) @asciidoc-cross-reference-face
     (xref) @asciidoc--fontify-reference
     (uri_label) @asciidoc-link-face)

   :language 'asciidoc-inline
   :feature 'inline-macro
   '((inline_macro (macro_name) @font-lock-function-call-face)
     (inline_macro (target) @font-lock-string-face)
     (inline_macro) @asciidoc--fontify-reference
     (stem_macro) @font-lock-function-call-face
     (footnote (macro_name) @asciidoc-footnote-marker-face)
     (footnote (attr) @asciidoc-footnote-text-face))

   :language 'asciidoc-inline
   :feature 'inline-reference
   '((id_assignment) @asciidoc-anchor-face
     (index_term) @font-lock-doc-face
     (index_term2) @font-lock-doc-face
     (attribute_reference) @font-lock-variable-name-face
     (intrinsic_attributes_pair) @font-lock-escape-face)

   :language 'asciidoc-inline
   :feature 'replacement
   '((replacement) @font-lock-escape-face
     (escaped_sequence) @font-lock-escape-face))
  "Tree-sitter font-lock settings for `asciidoc-mode'.")

;;; Feature list

(defvar asciidoc--treesit-font-lock-feature-list
  '((comment title)
    (block delimiter table list attribute macro metadata)
    (inline-markup inline-link inline-macro inline-reference)
    (replacement))
  "Font-lock feature list for `asciidoc-mode'.")

;;; Admonitions

;; A paragraph admonition (`NOTE: text', possibly wrapping over several
;; lines) is given a color-coded label and a whole-block background, like
;; Asciidoctor's rendered callouts.  The grammar only spans the first line
;; (and emits the label as a zero-width marker), so this is done with a
;; font-lock matcher that finds the `admonition' node via tree-sitter --
;; which keeps a `NOTE:'-looking line inside a code block from matching --
;; and extends the block to the next blank line.  A
;; `font-lock-extend-region-functions' entry re-fontifies the whole block
;; when an edit lands on a continuation line.

(defcustom asciidoc-fontify-admonition-blocks t
  "When non-nil, tint the whole body of a paragraph admonition.
The label is always color-coded; this controls only the background that
spans the admonition's body lines."
  :type 'boolean
  :group 'asciidoc
  :package-version '(asciidoc-mode . "0.4.0"))

(defconst asciidoc--admonition-label-regexp
  "\\(\\(?:NOTE\\|TIP\\|IMPORTANT\\|CAUTION\\|WARNING\\):\\)\\(?: \\|$\\)"
  "Regexp matching a paragraph admonition label at the start of a line.
Group 1 is the `LABEL:' text, without the trailing space.")

(defun asciidoc--admonition-kind (node)
  "Return the kind (\"note\", \"tip\", ...) of an `admonition' NODE."
  (let ((type (treesit-node-type (treesit-node-child node 0))))
    (when (and type (string-prefix-p "admonition_" type))
      (substring type (length "admonition_")))))

(defun asciidoc--admonition-block-end (beg)
  "Return the end of the admonition paragraph starting at BEG.
That is the start of the next blank line, or end of buffer."
  (save-excursion
    (goto-char beg)
    (if (re-search-forward "\n[ \t]*\n" nil t)
        (match-beginning 0)
      (point-max))))

(defun asciidoc--admonition-label-end (beg)
  "Return the end of the `LABEL:' starting at line position BEG, or nil."
  (save-excursion
    (goto-char beg)
    (and (looking-at asciidoc--admonition-label-regexp) (match-end 1))))

(defun asciidoc--fontify-admonitions (limit)
  "Font-lock matcher: give paragraph admonitions a block look up to LIMIT.
Returns nil and applies faces as a side effect."
  (when (treesit-parser-list nil 'asciidoc)
    (save-match-data
      (let ((root (treesit-buffer-root-node 'asciidoc)))
        (pcase-dolist (`(_ . ,node)
                       (treesit-query-capture
                        root '((admonition) @a) (point) limit))
          (when-let* ((kind (asciidoc--admonition-kind node))
                      (label-beg (save-excursion
                                   (goto-char (treesit-node-start node))
                                   (line-beginning-position)))
                      (label-end (asciidoc--admonition-label-end label-beg))
                      (block-end (asciidoc--admonition-block-end label-beg)))
            (when asciidoc-fontify-admonition-blocks
              (font-lock-prepend-text-property
               label-beg block-end 'face
               (intern (format "asciidoc-admonition-%s-face" kind))))
            (font-lock-prepend-text-property
             label-beg label-end 'face
             (intern (format "asciidoc-admonition-%s-label-face" kind)))
            (put-text-property label-beg block-end 'font-lock-multiline t))))))
  nil)

;; Bound dynamically by font-lock around `font-lock-extend-region-functions'.
(defvar font-lock-beg)
(defvar font-lock-end)

(defun asciidoc--font-lock-extend-region ()
  "Extend the font-lock region back to an admonition label above the region.
For `font-lock-extend-region-functions', so editing a continuation line
re-fontifies the whole admonition block."
  (save-excursion
    (goto-char font-lock-beg)
    (let ((para-start (if (re-search-backward "\n[ \t]*\n" nil t)
                          (match-end 0)
                        (point-min))))
      (when (and (< para-start font-lock-beg)
                 (progn (goto-char para-start)
                        (looking-at-p asciidoc--admonition-label-regexp)))
        (setq font-lock-beg para-start)))))

;;; Native source block fontification

;; The block grammar gives us the verbatim body (`listing_block_body' /
;; `literal_block_body') and the preceding `element_attr' holding the raw
;; attribute list (e.g. \"source,ruby\").  We fontify the body with the
;; language's own major mode in a hidden buffer and copy the faces back,
;; the same technique used by `markdown-mode', `org-mode' and `adoc-mode'.

(defun asciidoc--code-block-language (attr-value)
  "Return the source language in ATTR-VALUE, or nil.
ATTR-VALUE is the raw text of a block's attribute list, e.g.
\"source,ruby\".  The language is the positional second attribute, and
only for source blocks (a `source' or empty leading style)."
  (let* ((parts (split-string attr-value "," nil))
         (style (string-trim (car (split-string (or (car parts) "") "%"))))
         (lang (and (cdr parts) (string-trim (nth 1 parts)))))
    (when (and lang
               (not (string-empty-p lang))
               (member style '("source" ""))
               (not (string-search "=" lang)))
      lang)))

(defun asciidoc--code-block-lang-mode (lang)
  "Return the major mode to fontify LANG, or nil if none is available.
Consults `asciidoc-code-lang-modes' (whose value may be a single mode or
a list of candidates tried in order), then LANG-mode, and honors any
`major-mode-remap-alist' entry so tree-sitter modes are used when set."
  (let* ((down (downcase lang))
         (norm (lambda (value) (if (listp value) value (list value))))
         (mode (seq-find
                #'fboundp
                (append
                 (funcall norm (cdr (assoc lang asciidoc-code-lang-modes)))
                 (funcall norm (cdr (assoc down asciidoc-code-lang-modes)))
                 (list (intern (concat lang "-mode"))
                       (intern (concat down "-mode")))))))
    (when mode
      (if (fboundp 'major-mode-remap) (major-mode-remap mode) mode))))

(defun asciidoc--element-attr-value (attr-node)
  "Return the text of ATTR-NODE's `attr_value' child, or nil."
  (when-let* ((value (car (treesit-filter-child
                           attr-node
                           (lambda (n)
                             (equal (treesit-node-type n) "attr_value"))))))
    (treesit-node-text value t)))

(defun asciidoc--fontify-code-block-natively (lang beg end)
  "Fontify the source block body between BEG and END using LANG's mode.
Falls back to `asciidoc-fontify-code-block-default-mode' when LANG has no
available major mode.  Does nothing if the resolved mode is unavailable."
  (let ((lang-mode (or (asciidoc--code-block-lang-mode lang)
                       asciidoc-fontify-code-block-default-mode))
        (dest (current-buffer))
        (string (buffer-substring-no-properties beg end)))
    ;; Skip `asciidoc-mode' itself (e.g. `[source,asciidoc]') -- activating it
    ;; in the scratch buffer would recurse into this fontification.
    (when (and (fboundp lang-mode)
               (not (provided-mode-derived-p lang-mode 'asciidoc-mode)))
      (condition-case nil
          (let ((faces
                 (with-current-buffer
                     (get-buffer-create
                      (format " *asciidoc-code-fontification:%s*" lang-mode))
                   ;; Re-enable modification hooks: when this runs from
                   ;; `jit-lock' they are globally inhibited, which breaks
                   ;; font-lock in the scratch buffer (Bug#25132).
                   (let ((inhibit-modification-hooks nil))
                     (erase-buffer)
                     (insert string))
                   (unless (eq major-mode lang-mode)
                     (funcall lang-mode))
                   (font-lock-ensure)
                   ;; Collect (rel-start rel-end . face) runs.
                   (let ((pos (point-min)) runs)
                     (while (< pos (point-max))
                       (let ((next (or (next-single-property-change
                                        pos 'face nil (point-max))
                                       (point-max)))
                             (face (get-text-property pos 'face)))
                         (when face
                           (push (list (1- pos) (1- next) face) runs))
                         (setq pos next)))
                     (nreverse runs)))))
            ;; Replace the flat string face with the native faces, but only
            ;; when the language mode actually produced some -- otherwise the
            ;; block keeps the verbatim string face.
            (when faces
              (with-silent-modifications
                (put-text-property beg end 'face nil dest)
                (pcase-dolist (`(,rs ,re ,face) faces)
                  (put-text-property (+ beg rs) (+ beg re) 'face face dest)))))
        (error nil)))))

(defun asciidoc--fontify-code-blocks (limit)
  "Font-lock matcher: natively fontify source blocks between point and LIMIT.
Honors `asciidoc-fontify-code-blocks-natively' (including its size cap)
and always returns nil, doing its work as a side effect."
  (when (and asciidoc-fontify-code-blocks-natively
             (treesit-parser-list nil 'asciidoc))
    (save-match-data
      (let ((cap (if (integerp asciidoc-fontify-code-blocks-natively)
                     asciidoc-fontify-code-blocks-natively
                   most-positive-fixnum))
            (root (treesit-buffer-root-node 'asciidoc)))
        (pcase-dolist (`(_ . ,body)
                       (treesit-query-capture
                        root
                        '((listing_block_body) @b (literal_block_body) @b)
                        (point) limit))
          (let* ((block (treesit-node-parent body))
                 (attr (and block (treesit-node-prev-sibling block)))
                 (lang (and attr
                            (equal (treesit-node-type attr) "element_attr")
                            (asciidoc--code-block-language
                             (or (asciidoc--element-attr-value attr) ""))))
                 (beg (treesit-node-start body))
                 (end (treesit-node-end body)))
            (when (and lang (<= (- end beg) cap))
              (asciidoc--fontify-code-block-natively lang beg end)))))))
  nil)

;;; Imenu

(defun asciidoc--imenu-name (node)
  "Return a clean section name for NODE, stripping the leading `=' markers."
  (let ((text (treesit-node-text node t)))
    (if (string-match "^=+\\s-*" text)
        (substring text (match-end 0))
      text)))

(defvar asciidoc--treesit-simple-imenu-settings
  `(("Section" "\\`title[1-5]\\'" nil asciidoc--imenu-name))
  "Imenu settings for `asciidoc-mode'.")

;;; Outline

(defvar asciidoc--outline-predicate
  "\\`\\(?:document_title\\|title[1-5]\\)\\'"
  "Regexp matching title node types for outline integration.")

;;; Heading commands

(defconst asciidoc--heading-regexp "^\\(=\\{1,6\\}\\)[ \t]"
  "Regexp matching a one-line AsciiDoc heading.
Group 1 is the run of leading `=' markers.")

(defun asciidoc--heading-marker-bounds ()
  "Return (BEG . END) of the current line's heading markers, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at asciidoc--heading-regexp)
      (cons (match-beginning 1) (match-end 1)))))

(defun asciidoc-promote-heading ()
  "Promote the heading on the current line by removing one `=' marker."
  (interactive)
  (let ((bounds (asciidoc--heading-marker-bounds)))
    (cond ((null bounds) (user-error "Point is not on a heading"))
          ((<= (- (cdr bounds) (car bounds)) 1)
           (user-error "Already at the topmost heading level"))
          (t (save-excursion (goto-char (car bounds)) (delete-char 1))))))

(defun asciidoc-demote-heading ()
  "Demote the heading on the current line by adding one `=' marker."
  (interactive)
  (let ((bounds (asciidoc--heading-marker-bounds)))
    (cond ((null bounds) (user-error "Point is not on a heading"))
          ((>= (- (cdr bounds) (car bounds)) 6)
           (user-error "Already at the deepest heading level"))
          (t (save-excursion (goto-char (car bounds)) (insert "="))))))

;;; Reference navigation

(defun asciidoc--node-field (node type)
  "Return the text of NODE's first named child of TYPE, or nil."
  (when-let* ((child (car (treesit-filter-child
                           node
                           (lambda (c) (equal (treesit-node-type c) type))
                           t))))
    (treesit-node-text child t)))

(defun asciidoc--doc-attribute (name default)
  "Return document attribute NAME (a `:name: value' line) or DEFAULT.
An attribute set to an empty value yields an empty string, not DEFAULT."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         (concat "^:" (regexp-quote name) ":[ \t]*\\(.*\\)$") nil t)
        (string-trim (match-string-no-properties 1))
      default)))

(defun asciidoc--id-prefix-separator ()
  "Return a cons (PREFIX . SEPARATOR) for section id generation.
Defaults come from the document's `idprefix'/`idseparator' attributes,
falling back to the project conventions: Asciidoctor uses \"_\"/\"_\",
Antora uses \"\"/\"-\" (so `Setup Steps' becomes `setup-steps')."
  (let ((antora (asciidoc--antora-context)))
    (cons (asciidoc--doc-attribute "idprefix" (if antora "" "_"))
          (asciidoc--doc-attribute "idseparator" (if antora "-" "_")))))

(defun asciidoc--section-id (title &optional prefix separator)
  "Return the auto-generated id AsciiDoc derives from section TITLE.
TITLE is downcased, every run of non-alphanumeric characters becomes the
separator, leading and trailing separators are trimmed, and the prefix is
prepended.  PREFIX and SEPARATOR default to `asciidoc--id-prefix-separator'."
  (let* ((ps (unless (and prefix separator) (asciidoc--id-prefix-separator)))
         (prefix (or prefix (car ps)))
         (sep (or separator (cdr ps)))
         (body (replace-regexp-in-string "[^[:alnum:]]+" sep (downcase title))))
    (unless (string-empty-p sep)
      (let ((q (regexp-quote sep)))
        (setq body (replace-regexp-in-string
                    (concat "\\`" q "+\\|" q "+\\'") "" body))))
    (concat prefix body)))

(defun asciidoc--explicit-anchor-position (id)
  "Return the position of an explicit anchor defining ID, or nil.
Matches the inline forms `[[ID]]' and `[[ID,reftext]]' and the shorthand
`[#ID]' (optionally with roles, as in `[#ID.role]')."
  (let ((q (regexp-quote id)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "\\[\\[" q "\\(?:,[^]]*\\)?\\]\\]"
                     "\\|\\[#" q "[].]")
             nil t)
        (match-beginning 0)))))

(defun asciidoc--section-anchor-position (id)
  "Return the position of a section matching ID, or nil.
ID matches a section by its auto-generated id or by its exact title text,
the latter being a natural cross reference such as `<<My Section Title>>'."
  (let ((ps (asciidoc--id-prefix-separator)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "^=+[ \t]+\\(.+?\\)[ \t]*$" nil t)
          (let ((title (match-string-no-properties 1)))
            (when (or (equal title id)
                      (equal (asciidoc--section-id title (car ps) (cdr ps)) id))
              (throw 'found (match-beginning 0)))))
        nil))))

(defun asciidoc--anchor-position (id)
  "Return the buffer position of the anchor defining ID, or nil.
Explicit anchors (`[[ID]]', `[#ID]') take precedence over a section whose
auto-generated id matches ID."
  (or (asciidoc--explicit-anchor-position id)
      (asciidoc--section-anchor-position id)))

;; Cross-file (multi-document) reference resolution.  An Antora page xref
;; like `xref:basics/install.adoc#id[]' resolves its path against the
;; module's `pages/' directory; a generic project resolves against the
;; current file's directory.  Cross-component and versioned Antora targets
;; are out of scope (they live in other repos).

(defun asciidoc--antora-context ()
  "Return Antora coordinates for the current buffer, or nil.
The value is a plist (:root DIR :module NAME :component NAME) when the
file lives under `…/modules/<module>/pages/' beside an `antora.yml'."
  (when (and buffer-file-name
             (string-match "\\(.*\\)/modules/\\([^/]+\\)/pages/"
                           buffer-file-name))
    (let* ((root (match-string 1 buffer-file-name))
           (module (match-string 2 buffer-file-name))
           (descriptor (expand-file-name "antora.yml" root)))
      (when (file-exists-p descriptor)
        (list :root root :module module
              :component (asciidoc--antora-component descriptor))))))

(defun asciidoc--antora-component (descriptor)
  "Return the component name declared in the antora.yml at DESCRIPTOR, or nil."
  (with-temp-buffer
    (insert-file-contents descriptor)
    (when (re-search-forward "^name:[ \t]*\\(.+?\\)[ \t]*$" nil t)
      (match-string-no-properties 1))))

(defun asciidoc--resolve-antora-path (path ctx)
  "Resolve Antora page PATH to an absolute file using context CTX, or nil.
PATH may carry `module:' or `component:module:' coordinates and an
optional `version@' prefix.  Only the current component is resolved."
  (let* ((path (replace-regexp-in-string "\\`[^@]*@" "" path)) ; drop version@
         (parts (split-string path ":"))
         (rest (car (last parts)))
         (coords (butlast parts))
         (component (and (= (length coords) 2) (nth 0 coords)))
         (module (cond ((= (length coords) 2) (nth 1 coords))
                       ((= (length coords) 1) (nth 0 coords))
                       (t (plist-get ctx :module)))))
    (when (or (null component) (equal component (plist-get ctx :component)))
      (asciidoc--ensure-adoc
       (expand-file-name (concat "modules/" module "/pages/" rest)
                         (plist-get ctx :root))))))

(defun asciidoc--ensure-adoc (file)
  "Return FILE, appending `.adoc' when it has no extension."
  (if (file-name-extension file) file (concat file ".adoc")))

(defun asciidoc--resolve-file-target (path)
  "Resolve a cross-document target PATH to an absolute file, or nil."
  (let ((ctx (asciidoc--antora-context)))
    (cond
     (ctx (asciidoc--resolve-antora-path path ctx))
     (buffer-file-name
      (asciidoc--ensure-adoc
       (expand-file-name path (file-name-directory buffer-file-name)))))))

(defun asciidoc--reference-target-file (target)
  "Return the document-path part of TARGET if it points to another file.
TARGET points elsewhere when its path part ends in `.adoc' or contains a
path separator or Antora coordinate; otherwise it is an in-buffer id."
  (let ((path (car (split-string target "#"))))
    (when (and (not (string-empty-p path))
               (or (string-suffix-p ".adoc" path)
                   (string-match-p "[/:]" path)))
      path)))

(defun asciidoc--resolve-reference (target)
  "Resolve cross-reference TARGET to a marker, or nil.
TARGET is an id, a natural title, or a cross-document `path#fragment'.
Cross-document targets open the destination file."
  (let ((file (asciidoc--reference-target-file target)))
    (if (not file)
        (when-let* ((pos (asciidoc--anchor-position target)))
          (copy-marker pos))
      (let ((abs (asciidoc--resolve-file-target file))
            (fragment (cadr (split-string target "#"))))
        (when (and abs (file-readable-p abs))
          (with-current-buffer (find-file-noselect abs)
            (copy-marker (or (and fragment (not (string-empty-p fragment))
                                  (asciidoc--anchor-position fragment))
                             (point-min)))))))))

(defun asciidoc--goto-anchor (target)
  "Follow cross-reference TARGET to its anchor, opening another file if needed.
Signal a `user-error' when TARGET cannot be resolved."
  (let ((marker (asciidoc--resolve-reference target)))
    (unless marker
      (user-error "Cannot resolve cross-reference `%s'" target))
    (push-mark)
    (xref-push-marker-stack)
    (unless (eq (marker-buffer marker) (current-buffer))
      (pop-to-buffer-same-window (marker-buffer marker)))
    (goto-char marker)
    (when (fboundp 'pulse-momentary-highlight-one-line)
      (pulse-momentary-highlight-one-line (point)))))

(defun asciidoc-follow-reference-at-point (&optional event)
  "Follow the cross-reference or link at point.

A cross-reference (`<<id>>' or `xref:id[]') jumps to its anchor and pushes
the mark, so \\[set-mark-command] with a prefix returns.  A URL link, whether
a bare URL or a `link:'/`mailto:' macro, is opened with `browse-url'.

When called from a mouse EVENT, point is first moved to the click."
  (interactive (list last-nonmenu-event))
  (when (and event (mouse-event-p event))
    (mouse-set-point event))
  (let* ((leaf (treesit-node-at (point) 'asciidoc-inline))
         (node (and leaf
                    (treesit-parent-until
                     leaf
                     (lambda (n)
                       (member (treesit-node-type n)
                               '("xref" "autolink" "inline_macro")))
                     t))))
    (pcase (and node (treesit-node-type node))
      ("xref"
       (asciidoc--goto-anchor (asciidoc--node-field node "id")))
      ("autolink"
       (browse-url (treesit-node-text node t)))
      ("inline_macro"
       (let ((name (asciidoc--node-field node "macro_name"))
             (target (asciidoc--node-field node "target")))
         (pcase name
           ("xref" (asciidoc--goto-anchor target))
           ("link" (browse-url target))
           ("mailto" (browse-url (concat "mailto:" target)))
           (_ (user-error "Nothing to follow at point")))))
      (_ (user-error "No reference at point")))))

;;; Xref backend

;; Resolving an AsciiDoc cross-reference to its anchor is exactly what Emacs'
;; `xref' framework expects, so expose it as a backend.  This gives
;; `xref-find-definitions' (M-.) on a `<<id>>'/`xref:id[]' and `xref-go-back'
;; (M-,) to return, with history, for free.

(defun asciidoc--xref-backend ()
  "Return the `xref' backend for `asciidoc-mode'."
  'asciidoc)

(defun asciidoc--xref-id-at-point ()
  "Return the cross-reference id under point, or nil.
Recognized at a `<<id>>' or `xref:id[]' reference, or at an `[[id]]' /
`[#id]' anchor (so xref commands work from the definition too)."
  (let* ((leaf (treesit-node-at (point) 'asciidoc-inline))
         (node (and leaf
                    (treesit-parent-until
                     leaf
                     (lambda (n)
                       (member (treesit-node-type n)
                               '("xref" "inline_macro" "id_assignment")))
                     t))))
    (pcase (and node (treesit-node-type node))
      ("xref" (asciidoc--node-field node "id"))
      ("id_assignment" (asciidoc--node-field node "id"))
      ("inline_macro"
       (when (equal (asciidoc--node-field node "macro_name") "xref")
         (asciidoc--node-field node "target"))))))

(defun asciidoc--all-anchor-ids ()
  "Return the list of all cross-reference targets in the buffer.
Includes explicit anchors (`[[id]]', `[#id]'), auto-generated section ids,
and section titles (for natural cross references like `<<My Title>>')."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\[\\[\\([^],]+\\)\\(?:,[^]]*\\)?\\]\\]\\|\\[#\\([^].]+\\)[].]"
              nil t)
        (push (or (match-string-no-properties 1) (match-string-no-properties 2))
              ids))
      (goto-char (point-min))
      (let ((ps (asciidoc--id-prefix-separator)))
        (while (re-search-forward "^=+[ \t]+\\(.+?\\)[ \t]*$" nil t)
          (let ((title (match-string-no-properties 1)))
            (push (asciidoc--section-id title (car ps) (cdr ps)) ids)
            (push title ids)))))
    (delete-dups (nreverse ids))))

(defconst asciidoc--builtin-attributes
  '("doctitle" "author" "authorinitials" "firstname" "lastname" "email"
    "revnumber" "revdate" "revremark" "version" "doctype" "backend"
    "sectnums" "sectanchors" "toc" "toclevels" "icons" "imagesdir"
    "source-highlighter" "experimental" "idprefix" "idseparator"
    "nofooter" "stem" "tabsize" "leveloffset"
    ;; character-replacement intrinsics
    "sp" "nbsp" "zwsp" "wj" "apos" "quot" "lsquo" "rsquo" "ldquo" "rdquo"
    "deg" "plus" "brvbar" "vbar" "amp" "lt" "gt" "startsb" "endsb"
    "caret" "asterisk" "tilde" "backslash" "backtick" "two-colons"
    "two-semicolons" "cpp" "pp" "blank" "empty")
  "Common built-in document attribute names offered for completion.")

(defconst asciidoc--common-source-languages
  '("asciidoc" "bash" "c" "clojure" "cpp" "csharp" "css" "diff" "dockerfile"
    "elixir" "emacs-lisp" "erlang" "go" "groovy" "haskell" "html" "java"
    "javascript" "json" "kotlin" "lisp" "lua" "make" "markdown" "ocaml"
    "perl" "php" "python" "ruby" "rust" "scala" "scheme" "sh" "shell" "sql"
    "swift" "toml" "typescript" "xml" "yaml")
  "Common source-block language names offered for completion.")

(defun asciidoc--attribute-names ()
  "Return attribute names for completion.
Combines the attributes defined in the buffer with the built-in set."
  (let (names)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^:\\([a-zA-Z0-9_][a-zA-Z0-9_-]*\\):" nil t)
        (push (match-string-no-properties 1) names)))
    (delete-dups (append (nreverse names) asciidoc--builtin-attributes))))

(defun asciidoc--source-languages ()
  "Return source-block language names for completion."
  (delete-dups (append (mapcar #'car asciidoc-code-lang-modes)
                       asciidoc--common-source-languages)))

(defun asciidoc--capf-bounded (opener forbidden table)
  "Return a capf for completion after OPENER up to point.
Search back to OPENER on the current line; return nil when the text
between it and point contains any FORBIDDEN character.  TABLE is the
completion collection."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward opener (line-beginning-position) t)
        (let ((start (match-end 0)))
          (unless (string-match-p forbidden
                                  (buffer-substring-no-properties start end))
            (list start end table :exclusive 'no)))))))

(defun asciidoc--capf-xref ()
  "Complete cross-reference ids inside `<<...>>' or `xref:...['."
  (or (asciidoc--capf-bounded
       "<<" "[>,]"
       (completion-table-dynamic (lambda (_) (asciidoc--all-anchor-ids))))
      (asciidoc--capf-bounded
       "xref:" "[],]"
       (completion-table-dynamic (lambda (_) (asciidoc--all-anchor-ids))))))

(defun asciidoc--capf-attribute ()
  "Complete attribute names inside `{...}'."
  (asciidoc--capf-bounded
   "{" "[}[:space:]]"
   (completion-table-dynamic (lambda (_) (asciidoc--attribute-names)))))

(defun asciidoc--capf-source-language ()
  "Complete the language in a `[source,LANG]' block-attribute line."
  (asciidoc--capf-bounded
   "\\[source,[ \t]*" "[],]"
   (completion-table-dynamic (lambda (_) (asciidoc--source-languages)))))

(defun asciidoc--capf-include ()
  "Complete the file path in an `include::PATH' directive."
  (when buffer-file-name
    (asciidoc--capf-bounded "include::" "\\[" #'completion-file-name-table)))

(defun asciidoc--capf ()
  "Context-aware `completion-at-point' for `asciidoc-mode'.
Completes cross-reference ids in `<<'/`xref:', attribute names in `{',
source-block languages after `[source,', and file paths after
`include::'.  Stays out of the way in ordinary prose."
  (or (asciidoc--capf-xref)
      (asciidoc--capf-attribute)
      (asciidoc--capf-source-language)
      (asciidoc--capf-include)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql asciidoc)))
  "Return the cross-reference id at point for the `asciidoc' backend."
  (asciidoc--xref-id-at-point))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql asciidoc)))
  "Return all anchor ids in the buffer for the `asciidoc' backend."
  (asciidoc--all-anchor-ids))

(cl-defmethod xref-backend-definitions ((_backend (eql asciidoc)) id)
  "Return the definition of ID as `xref' items for the `asciidoc' backend.
ID may be an in-buffer id/title or a cross-document `path#fragment'."
  (when-let* ((marker (asciidoc--resolve-reference id)))
    (list (xref-make id (xref-make-buffer-location
                         (marker-buffer marker) (marker-position marker))))))

(defun asciidoc--xref-item-at (pos)
  "Return an `xref-item' for the reference at POS, summarized by its line."
  (save-excursion
    (goto-char pos)
    (xref-make (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               (xref-make-buffer-location (current-buffer) pos))))

(cl-defmethod xref-backend-references ((_backend (eql asciidoc)) id)
  "Return `<<id>>' and `xref:id[]' usages of ID as `xref' items.
References are resolved within the current buffer."
  (let ((q (regexp-quote id)) items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "<<" q "\\(?:,[^>]*\\)?>>") nil t)
        (push (asciidoc--xref-item-at (match-beginning 0)) items))
      (goto-char (point-min))
      (while (re-search-forward (concat "xref:" q "\\(?:#[^[]*\\)?\\[") nil t)
        (push (asciidoc--xref-item-at (match-beginning 0)) items)))
    (nreverse items)))

;;; Flyspell

(defconst asciidoc--no-spellcheck-node-types
  '("autolink" "xref" "inline_macro" "id_assignment" "monospace" "passthrough")
  "Inline node types whose text `flyspell-mode' should not spell-check.")

(defun asciidoc--flyspell-verify ()
  "Return non-nil if `flyspell-mode' should check the word ending at point.
Skip words inside links, cross-references, anchors, macros, and inline
code: URLs and ids are not prose, and on a navigable reference flyspell's
own mouse keymap would otherwise shadow `asciidoc-follow-reference-at-point'."
  (or (null (treesit-parser-list nil 'asciidoc-inline))
      (let ((node (treesit-node-at (max (point-min) (1- (point)))
                                   'asciidoc-inline)))
        (not (and node
                  (treesit-parent-until
                   node
                   (lambda (n)
                     (member (treesit-node-type n)
                             asciidoc--no-spellcheck-node-types))
                   t))))))

(put 'asciidoc-mode 'flyspell-mode-predicate #'asciidoc--flyspell-verify)

;;; Flymake

(defcustom asciidoc-asciidoctor-command "asciidoctor"
  "Executable used by the Asciidoctor-backed Flymake checker."
  :type 'string
  :group 'asciidoc
  :package-version '(asciidoc-mode . "0.4.0"))

(defcustom asciidoc-asciidoctor-extra-args nil
  "Extra command-line arguments passed to Asciidoctor by the Flymake checker."
  :type '(repeat string)
  :group 'asciidoc
  :package-version '(asciidoc-mode . "0.4.0"))

(defconst asciidoc--flymake-diagnostic-re
  (concat "^asciidoctor: \\(ERROR\\|WARNING\\|DEPRECATED\\): "
          "<stdin>: [Ll]ine \\([0-9]+\\): \\(.*\\)$")
  "Regexp matching an Asciidoctor diagnostic about the document on stdin.
Group 1 is the severity, group 2 the line number, group 3 the message.
Asciidoctor has spelled it both `line' and `Line', hence the alternation.")

(defvar-local asciidoc--flymake-proc nil
  "The most recent Asciidoctor Flymake process for this buffer.")

(defun asciidoc--flymake-parse-output (output source &optional exit-status)
  "Parse Asciidoctor OUTPUT into Flymake diagnostics for buffer SOURCE.
OUTPUT is the combined output of an Asciidoctor run over the buffer's
contents.  When EXIT-STATUS is non-zero and no per-line diagnostics are
found, the first message is reported as a buffer-level error so a fatal
failure is not swallowed."
  (let ((diags '())
        (count 0))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward asciidoc--flymake-diagnostic-re nil t)
        (let* ((type (pcase (match-string 1)
                       ("ERROR" :error)
                       ("WARNING" :warning)
                       (_ :note)))
               (line (string-to-number (match-string 2)))
               (msg (match-string 3))
               (region (with-current-buffer source
                         (flymake-diag-region source line))))
          (when region
            (setq count (1+ count))
            (push (flymake-make-diagnostic source (car region) (cdr region)
                                           type msg)
                  diags))))
      (when (and (zerop count) exit-status (not (zerop exit-status)))
        (goto-char (point-min))
        (when (re-search-forward "^asciidoctor: .+$" nil t)
          (when-let* ((region (with-current-buffer source
                                (flymake-diag-region source 1))))
            (push (flymake-make-diagnostic source (car region) (cdr region)
                                           :error (match-string 0))
                  diags)))))
    (nreverse diags)))

(defun asciidoc-flymake (report-fn &rest _args)
  "An AsciiDoc Flymake backend using Asciidoctor.
Run the current buffer through `asciidoc-asciidoctor-command' and turn its
parser diagnostics into Flymake reports via REPORT-FN.  Add this to
`flymake-diagnostic-functions' (the mode does so automatically)."
  (unless (executable-find asciidoc-asciidoctor-command)
    (error "Cannot find the Asciidoctor executable %S"
           asciidoc-asciidoctor-command))
  (when (process-live-p asciidoc--flymake-proc)
    (kill-process asciidoc--flymake-proc))
  (let ((source (current-buffer))
        (base (expand-file-name default-directory)))
    (save-restriction
      (widen)
      (setq
       asciidoc--flymake-proc
       (make-process
        :name "asciidoc-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *asciidoc-flymake*")
        :command (append (list asciidoc-asciidoctor-command)
                         asciidoc-asciidoctor-extra-args
                         (list "-B" base "-o" null-device "-"))
        :sentinel
        (lambda (proc _event)
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                (if (and (buffer-live-p source)
                         (with-current-buffer source
                           (eq proc asciidoc--flymake-proc)))
                    (funcall report-fn
                             (asciidoc--flymake-parse-output
                              (with-current-buffer (process-buffer proc)
                                (buffer-string))
                              source (process-exit-status proc)))
                  (flymake-log :warning "Canceling obsolete check %s" proc))
              (kill-buffer (process-buffer proc)))))))
      (process-send-region asciidoc--flymake-proc (point-min) (point-max))
      (process-send-eof asciidoc--flymake-proc))))

;;; Mode definition

;;;###autoload
(define-derived-mode asciidoc-mode text-mode "AsciiDoc"
  "Major mode for editing AsciiDoc files, powered by tree-sitter.

Requires two tree-sitter grammars from
<https://github.com/cathaysia/tree-sitter-asciidoc>.
Install them with \\[asciidoc-install-grammars].

\\{asciidoc-mode-map}"
  (setq-local comment-start "// ")
  ;; AsciiDoc line comments only start at the beginning of a line, so
  ;; anchor the skip regexp.  Otherwise the comment-aware filling sees the
  ;; `//' in a URL like `https://...' mid-line and fills the paragraph with
  ;; a bogus `//' prefix.
  (setq-local comment-start-skip "^//+\\s-*")

  ;; Asciidoctor-backed diagnostics, contributed only when the user turns on
  ;; `flymake-mode'.  Independent of tree-sitter, so it works in the
  ;; grammar-less fallback too.
  (add-hook 'flymake-diagnostic-functions #'asciidoc-flymake nil t)

  (when (asciidoc--ensure-grammars)
    ;; Create both parsers over the full buffer.
    ;; Create inline parser first so the block parser ends up first
    ;; in `treesit-parser-list' (used by `treesit-buffer-root-node').
    (treesit-parser-create 'asciidoc-inline)
    (treesit-parser-create 'asciidoc)

    (setq-local treesit-primary-parser
                (car (treesit-parser-list nil 'asciidoc)))

    ;; Restrict the inline parser to actual inline-content ranges.  The
    ;; inline grammar is meant to parse a single inline span; run over the
    ;; whole buffer it misreads block markup (e.g. `*' list markers) as
    ;; emphasis and can produce an error spanning the rest of the buffer,
    ;; which suppresses inline fontification.  Embedding it into the block
    ;; parser via `treesit-range-rules' scopes it to the relevant text.
    ;; The `line' / `table_cell_content' children are captured rather than
    ;; their containers so block markers (`*', `.', `=') stay out of the
    ;; inline ranges.
    (setq-local treesit-range-settings
                (treesit-range-rules
                 :embed 'asciidoc-inline
                 :host 'asciidoc
                 '((paragraph (line) @cap)
                   (admonition (line) @cap)
                   (unordered_list_item (line) @cap)
                   (ordered_list_item (line) @cap)
                   (checked_list_item (line) @cap)
                   (callout_list_item (line) @cap)
                   (description_list_item (line) @cap)
                   (quoted_block (line) @cap)
                   (table_cell (table_cell_content) @cap)
                   (csv_record (table_cell_content) @cap)
                   (dsv_record (table_cell_content) @cap))))
    (setq-local treesit-language-at-point-function
                (lambda (_pos) 'asciidoc))

    ;; Font-lock
    (setq-local treesit-font-lock-settings asciidoc--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                asciidoc--treesit-font-lock-feature-list)
    ;; Some inline rules attach non-face text properties: super/subscript add
    ;; a `display' \='(raise ...), and references add a keymap/mouse affordance.
    ;; Register them so font-lock clears them on refontification, e.g. when an
    ;; edited span no longer needs them.
    (setq-local font-lock-extra-managed-props
                (append '(display keymap mouse-face follow-link help-echo)
                        font-lock-extra-managed-props))

    ;; Admonition blocks span several lines; let font-lock re-fontify the
    ;; whole block when an edit lands inside it.
    (setq-local font-lock-multiline t)
    (add-hook 'font-lock-extend-region-functions
              #'asciidoc--font-lock-extend-region nil t)

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                asciidoc--treesit-simple-imenu-settings)

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                "\\`\\(?:document_title\\|title[1-5]\\)\\'")
    (setq-local treesit-defun-name-function #'asciidoc--imenu-name)
    (setq-local treesit-thing-settings
                `((asciidoc
                   (sentence
                    ,(regexp-opt '("paragraph" "listing_block"
                                   "literal_block" "admonition" "list"
                                   "quoted_md_block" "breaks"
                                   "block_comment" "line_comment"))))))

    ;; Outline
    (setq-local treesit-outline-predicate asciidoc--outline-predicate)

    ;; Cross-reference navigation via `xref' (M-. / M-,).
    (add-hook 'xref-backend-functions #'asciidoc--xref-backend nil t)
    ;; Complete cross-reference ids while typing `<<'.
    (add-hook 'completion-at-point-functions #'asciidoc--capf nil t)

    (treesit-major-mode-setup)

    ;; Enable outline-minor-mode for heading navigation and folding, with
    ;; TAB/S-TAB cycling visibility on heading lines.
    (setq-local outline-minor-mode-cycle t)
    (outline-minor-mode 1))

  ;; Added last: `font-lock-add-keywords' must run after
  ;; `treesit-major-mode-setup' or it suppresses tree-sitter fontification.
  ;; The code-block matcher runs after tree-sitter has applied the verbatim
  ;; string face, replacing it with native faces (see
  ;; `asciidoc--fontify-code-blocks').
  (font-lock-add-keywords nil '((asciidoc--fontify-admonitions)))
  (font-lock-add-keywords nil '((asciidoc--fontify-code-blocks)) 'append))

;;; Keymap

;; Heading-oriented bindings modelled on `org-mode' / `adoc-mode'.
(keymap-set asciidoc-mode-map "M-<left>" #'asciidoc-promote-heading)
(keymap-set asciidoc-mode-map "M-<right>" #'asciidoc-demote-heading)
(keymap-set asciidoc-mode-map "M-<up>" #'outline-move-subtree-up)
(keymap-set asciidoc-mode-map "M-<down>" #'outline-move-subtree-down)
(keymap-set asciidoc-mode-map "C-c C-n" #'outline-next-visible-heading)
(keymap-set asciidoc-mode-map "C-c C-p" #'outline-previous-visible-heading)
(keymap-set asciidoc-mode-map "C-c C-f" #'outline-forward-same-level)
(keymap-set asciidoc-mode-map "C-c C-b" #'outline-backward-same-level)
(keymap-set asciidoc-mode-map "C-c C-u" #'outline-up-heading)
(keymap-set asciidoc-mode-map "C-c C-o" #'asciidoc-follow-reference-at-point)

;;; Menu

(easy-menu-define asciidoc-mode-menu asciidoc-mode-map
  "Menu for `asciidoc-mode'."
  '("AsciiDoc"
    ("Navigation"
     ["Jump to Section..." imenu]
     ["Follow Reference at Point" asciidoc-follow-reference-at-point]
     "---"
     ["Next Section" outline-next-visible-heading]
     ["Previous Section" outline-previous-visible-heading]
     ["Up to Parent Section" outline-up-heading]
     ["Next Section (Same Level)" outline-forward-same-level]
     ["Previous Section (Same Level)" outline-backward-same-level]
     "---"
     ["Forward Sentence" forward-sentence]
     ["Backward Sentence" backward-sentence])
    ("Show & Hide"
     ["Cycle Heading" outline-cycle]
     ["Cycle Buffer" outline-cycle-buffer]
     "---"
     ["Show All" outline-show-all]
     ["Hide Body" outline-hide-body]
     ["Hide Other" outline-hide-other])
    ("Headings"
     ["Promote Heading" asciidoc-promote-heading]
     ["Demote Heading" asciidoc-demote-heading]
     ["Move Subtree Up" outline-move-subtree-up]
     ["Move Subtree Down" outline-move-subtree-down]
     "---"
     ["Mark Subtree" outline-mark-subtree])
    "---"
    ["Comment/Uncomment Region" comment-dwim]
    ["Narrow to Section" narrow-to-defun]
    ["Widen" widen :enable (buffer-narrowed-p)]
    "---"
    ["Install Grammars" asciidoc-install-grammars]
    ["Inspect Mode" treesit-inspect-mode]
    "---"
    ["Show Version" (message "asciidoc-mode %s" asciidoc-mode-version)]
    ["Describe Mode" (describe-function 'asciidoc-mode)]))

;;; Auto-mode-alist

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciidoc-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . asciidoc-mode))

(provide 'asciidoc-mode)
;;; asciidoc-mode.el ends here
