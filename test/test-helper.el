;;; test-helper.el --- Test helpers for asciidoc-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared utilities for asciidoc-mode buttercup tests.

;;; Code:

(require 'buttercup)
(require 'asciidoc-mode)

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
  "Return the face at POS in the current buffer."
  (get-text-property pos 'face))

(provide 'test-helper)
;;; test-helper.el ends here
