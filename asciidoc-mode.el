;;; asciidoc-mode.el --- Major mode for AsciiDoc markup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: https://github.com/bbatsov/asciidoc-mode
;; Version: 0.1.0
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

;;; Customization

(defgroup asciidoc nil
  "Support for AsciiDoc markup."
  :group 'text
  :link '(url-link "https://github.com/bbatsov/asciidoc-mode"))

;;; Version

(defconst asciidoc-mode-version "0.1.0"
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

;;; Font-lock

(defvar asciidoc--font-lock-settings
  (treesit-font-lock-rules
   ;; Block-level rules (asciidoc parser)
   :language 'asciidoc
   :feature 'comment
   '((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face)

   :language 'asciidoc
   :feature 'title
   '((document_title) @asciidoc-document-title-face
     (title1) @asciidoc-title-1-face
     (title2) @asciidoc-title-2-face
     (title3) @asciidoc-title-3-face
     (title4) @asciidoc-title-4-face
     (title5) @asciidoc-title-5-face)

   :language 'asciidoc
   :feature 'block
   '((listing_block_body) @font-lock-string-face
     (literal_block_body) @font-lock-string-face
     (ident_block_line) @font-lock-string-face
     (block_title) @font-lock-type-face
     (breaks) @font-lock-comment-delimiter-face
     (quoted_md_block) @font-lock-doc-face)

   :language 'asciidoc
   :feature 'list
   '((ordered_list_marker) @font-lock-constant-face
     (unordered_list_marker) @font-lock-constant-face
     (checked_list_marker) @font-lock-constant-face
     (callout_list_marker) @font-lock-constant-face
     (callout_marker) @font-lock-constant-face)

   :language 'asciidoc
   :feature 'admonition
   '((admonition) @font-lock-keyword-face)

   :language 'asciidoc
   :feature 'attribute
   '((document_attr (attr_name) @font-lock-variable-name-face)
     (element_attr) @font-lock-preprocessor-face)

   :language 'asciidoc
   :feature 'macro
   '((block_macro (block_macro_name) @font-lock-function-call-face)
     (block_macro (target) @font-lock-string-face))

   :language 'asciidoc
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
   '((autolink) @font-lock-constant-face
     (xref) @font-lock-constant-face
     (uri_label) @link)

   :language 'asciidoc-inline
   :feature 'inline-macro
   '((inline_macro) @font-lock-function-call-face
     (stem_macro) @font-lock-function-call-face
     (footnote) @font-lock-doc-face)

   :language 'asciidoc-inline
   :feature 'inline-reference
   '((id_assignment) @font-lock-preprocessor-face
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
    (block list admonition attribute macro metadata)
    (inline-markup inline-link inline-macro inline-reference)
    (replacement))
  "Font-lock feature list for `asciidoc-mode'.")

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

;;; Mode definition

;;;###autoload
(define-derived-mode asciidoc-mode text-mode "AsciiDoc"
  "Major mode for editing AsciiDoc files, powered by tree-sitter.

Requires two tree-sitter grammars from
<https://github.com/cathaysia/tree-sitter-asciidoc>.
Install them with \\[asciidoc-install-grammars].

\\{asciidoc-mode-map}"
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+\\s-*")

  (when (asciidoc--ensure-grammars)
    ;; Create both parsers over the full buffer.
    ;; Create inline parser first so the block parser ends up first
    ;; in `treesit-parser-list' (used by `treesit-buffer-root-node').
    (treesit-parser-create 'asciidoc-inline)
    (treesit-parser-create 'asciidoc)

    (setq-local treesit-primary-parser
                (car (treesit-parser-list nil 'asciidoc)))

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

    ;; Outline
    (setq-local treesit-outline-predicate asciidoc--outline-predicate)

    (treesit-major-mode-setup)))

;;; Auto-mode-alist

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciidoc-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . asciidoc-mode))

(provide 'asciidoc-mode)
;;; asciidoc-mode.el ends here
