;;; test-helper.el --- Test helpers for asciidoc-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared utilities for asciidoc-mode buttercup tests.

;;; Code:

(require 'buttercup)
(require 'asciidoc-mode)

(defvar asciidoc-test-grammars-available
  (and (treesit-available-p)
       (treesit-language-available-p 'asciidoc)
       (treesit-language-available-p 'asciidoc-inline))
  "Non-nil if both AsciiDoc tree-sitter grammars are installed.")

;; CI sets this so a broken grammar install fails the run instead of
;; quietly skipping most of the suite.
(when (and (getenv "ASCIIDOC_MODE_REQUIRE_GRAMMARS")
           (not asciidoc-test-grammars-available))
  (error "The AsciiDoc tree-sitter grammars are required but not installed"))

(defun asciidoc-test-assume-grammars ()
  "Skip the current spec unless the AsciiDoc grammars are installed.
Call it first thing in a spec."
  (assume asciidoc-test-grammars-available
          "tree-sitter grammars not installed"))

(defmacro with-asciidoc-buffer (content &rest body)
  "Create a temp buffer with CONTENT in `asciidoc-mode', then run BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (asciidoc-mode)
     ,@body))

(defmacro with-fontified-asciidoc-buffer (content &rest body)
  "Like `with-asciidoc-buffer' but also runs `font-lock-ensure'.
CONTENT is inserted and BODY is evaluated after fontification."
  (declare (indent 1) (debug t))
  `(with-asciidoc-buffer ,content
     (font-lock-ensure)
     ,@body))

(defun asciidoc-test-face-at (pos)
  "Return the face at POS in the current buffer.
A list holding a single face is returned as that face: rules that append
their face leave such a list behind, and it means the same thing."
  (let ((face (get-text-property pos 'face)))
    (if (and (consp face) (null (cdr face))) (car face) face)))

(defun asciidoc-test-face-at-includes (pos face)
  "Return non-nil if FACE is among the face(s) applied at POS.
Handles the `face' property being either a single face or a list."
  (let ((f (get-text-property pos 'face)))
    (memq face (if (listp f) f (list f)))))

(defun asciidoc-test-faces-in (needle)
  "Return the list of faces at the start of NEEDLE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle)
    (let ((face (get-text-property (match-beginning 0) 'face)))
      (if (listp face) face (list face)))))

(defun asciidoc-test-mono-face (text needle)
  "Fontify TEXT in `asciidoc-mode' and return the face just inside NEEDLE.
NEEDLE is a substring (e.g. a backtick-delimited code span); the face is
read one character past its start."
  (with-fontified-asciidoc-buffer text
    (let ((pos (string-match needle (buffer-string))))
      (asciidoc-test-face-at (1+ pos)))))

(provide 'test-helper)
;;; test-helper.el ends here
