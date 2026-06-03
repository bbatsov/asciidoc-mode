;;; asciidoc-mode.el --- Major mode for AsciiDoc markup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/asciidoc-mode
;; Version: 0.3.0
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
;; - Comment support (// line comments)
;;
;; Quick start:
;;   (asciidoc-install-grammars)   ; one-time setup
;;   ;; then open any .adoc file

;;; Code:

(require 'treesit)
(require 'subr-x)
(require 'outline)

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

(defconst asciidoc-mode-version "0.3.0"
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

(defface asciidoc-cross-reference-face
  '((t :inherit font-lock-constant-face))
  "Face for internal cross-references (e.g. <<id>>)."
  :group 'asciidoc)

(defface asciidoc-anchor-face
  '((t :inherit font-lock-type-face))
  "Face for anchor definitions (e.g. [[id]] and [#id])."
  :group 'asciidoc)

;;; Font-lock

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
   '((listing_block_body) @font-lock-string-face
     (literal_block_body) @font-lock-string-face
     (ident_block_line) @font-lock-string-face
     (block_title) @font-lock-type-face
     (breaks) @font-lock-comment-delimiter-face
     (quoted_md_block) @font-lock-doc-face)

   :language 'asciidoc
   :override t
   :feature 'delimiter
   '((listing_block_start_marker) @font-lock-delimiter-face
     (listing_block_end_marker) @font-lock-delimiter-face
     (literal_block_marker) @font-lock-delimiter-face
     (passthrough_block_marker) @font-lock-delimiter-face
     (open_block_marker) @font-lock-delimiter-face
     (quoted_block_start_marker) @font-lock-delimiter-face
     (quoted_block_end_marker) @font-lock-delimiter-face
     (delimited_block_start_marker) @font-lock-delimiter-face
     (delimited_block_end_marker) @font-lock-delimiter-face)

   :language 'asciidoc
   :override t
   :feature 'table
   '((table_block_marker) @font-lock-delimiter-face
     (table_cell_attr) @font-lock-preprocessor-face)

   :language 'asciidoc
   :override t
   :feature 'list
   '((ordered_list_marker) @font-lock-constant-face
     (unordered_list_marker) @font-lock-constant-face
     (checked_list_marker) @font-lock-constant-face
     (callout_list_marker) @font-lock-constant-face
     (callout_marker) @font-lock-constant-face)

   :language 'asciidoc
   :override t
   :feature 'attribute
   '((document_attr (attr_name) @font-lock-variable-name-face)
     (document_attr (line) @font-lock-string-face)
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
     (monospace) @font-lock-string-face
     (highlight) @font-lock-warning-face
     (passthrough) @font-lock-string-face)

   :language 'asciidoc-inline
   :feature 'inline-link
   '((autolink) @asciidoc-link-face
     (xref) @asciidoc-cross-reference-face
     (uri_label) @asciidoc-link-face)

   :language 'asciidoc-inline
   :feature 'inline-macro
   '((inline_macro (macro_name) @font-lock-function-call-face)
     (inline_macro (target) @font-lock-string-face)
     (stem_macro) @font-lock-function-call-face
     (footnote) @font-lock-doc-face)

   :language 'asciidoc-inline
   :feature 'inline-reference
   '((id_assignment) @asciidoc-anchor-face
     (index_term) @font-lock-doc-face
     (index_term2) @font-lock-doc-face
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

;;; Admonition labels

;; The block grammar consumes a paragraph-style admonition label (e.g.
;; \"NOTE\") without emitting a node for it, so it can't be highlighted
;; via tree-sitter.  A small font-lock keyword fills the gap and, unlike
;; the previous tree-sitter rule, leaves the admonition body to inline
;; fontification instead of overriding it.
(defvar asciidoc--admonition-font-lock-keywords
  '(("^\\(?:NOTE\\|TIP\\|IMPORTANT\\|CAUTION\\|WARNING\\):"
     0 'font-lock-keyword-face t))
  "Font-lock keywords for paragraph-style admonition labels.")

;;; Attribute references

;; The inline grammar doesn't recognize attribute references (`{name}'
;; expands to nothing), so highlight them with a font-lock keyword.  The
;; nil override leaves existing faces (code blocks, strings) untouched, so
;; e.g. a shell `${VAR}' inside a source block is not mistaken for one.
(defvar asciidoc--attribute-reference-font-lock-keywords
  '(("{\\(?:[a-zA-Z0-9_][a-zA-Z0-9_-]*\\)}"
     0 'font-lock-variable-name-face nil))
  "Font-lock keywords for inline attribute references like `{name}'.")

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
                   (quoted_block (line) @cap)
                   (table_cell (table_cell_content) @cap))))
    (setq-local treesit-language-at-point-function
                (lambda (_pos) 'asciidoc))

    ;; Font-lock
    (setq-local treesit-font-lock-settings asciidoc--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                asciidoc--treesit-font-lock-feature-list)

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
  (font-lock-add-keywords nil asciidoc--admonition-font-lock-keywords)
  (font-lock-add-keywords nil asciidoc--attribute-reference-font-lock-keywords)
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

;;; Menu

(easy-menu-define asciidoc-mode-menu asciidoc-mode-map
  "Menu for `asciidoc-mode'."
  '("AsciiDoc"
    ("Navigation"
     ["Jump to Section..." imenu]
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
