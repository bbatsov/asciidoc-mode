;;; asciidoc-mode-smoke-test.el --- Font-lock smoke test for asciidoc-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; A smoke test that fontifies a large, construct-dense document
;; (test/fixtures/showcase.adoc) and checks that `asciidoc-mode' handles
;; it without error, produces a stable (idempotent) fontification, and
;; actually applies the expected faces.  This guards against regressions
;; that only show up on complex documents: runaway inline-parser state
;; that swallows the rest of the buffer, under-fontification, faces
;; bleeding across constructs, and errors in the font-lock keyword
;; matchers.

;;; Code:

(require 'cl-lib)
(require 'test-helper)

(defvar asciidoc-test-grammars-available
  (and (treesit-available-p)
       (treesit-language-available-p 'asciidoc)
       (treesit-language-available-p 'asciidoc-inline))
  "Non-nil if both AsciiDoc tree-sitter grammars are installed.")

(defvar asciidoc-test-showcase-file
  (expand-file-name "fixtures/showcase.adoc"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the construct-dense showcase fixture.")

(describe "Font-lock smoke test"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (defun asciidoc-test--showcase-buffer ()
    "Return a fontified `asciidoc-mode' buffer with the showcase document."
    (let ((buf (generate-new-buffer " *asciidoc-showcase*")))
      (with-current-buffer buf
        (insert-file-contents asciidoc-test-showcase-file)
        (asciidoc-mode)
        (font-lock-ensure))
      buf))

  (it "fontifies the whole showcase document without error"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((buf (asciidoc-test--showcase-buffer)))
      (unwind-protect
          (with-current-buffer buf
            ;; re-fontifying an already-fontified buffer must not error either
            (expect (font-lock-ensure) :not :to-throw)
            (expect (> (point-max) 1000) :to-be-truthy))
        (kill-buffer buf))))

  (it "produces a stable (idempotent) fontification"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((buf (asciidoc-test--showcase-buffer)))
      (unwind-protect
          (with-current-buffer buf
            (cl-flet ((face-vector ()
                        (let (acc (pos (point-min)))
                          (while (< pos (point-max))
                            (push (get-text-property pos 'face) acc)
                            (setq pos (1+ pos)))
                          (nreverse acc))))
              (let ((faces-before (face-vector)))
                (font-lock-flush)
                (font-lock-ensure)
                (expect (face-vector) :to-equal faces-before))))
        (kill-buffer buf))))

  (it "applies the expected faces across the document"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((buf (asciidoc-test--showcase-buffer)))
      (unwind-protect
          (with-current-buffer buf
            (let ((present (make-hash-table :test 'equal))
                  (pos (point-min))
                  missing)
              (while (< pos (point-max))
                (dolist (f (let ((v (get-text-property pos 'face)))
                             (if (listp v) v (list v))))
                  (when f (puthash f t present)))
                (setq pos (1+ pos)))
              (dolist (face '(asciidoc-document-title-face ; document title
                              asciidoc-title-1-face        ; section titles
                              asciidoc-title-2-face
                              bold                          ; *bold*
                              italic                        ; _italic_
                              font-lock-string-face         ; `mono` / code bodies
                              font-lock-warning-face        ; #highlight#
                              font-lock-constant-face       ; list markers / links / xrefs
                              font-lock-keyword-face        ; admonition labels
                              font-lock-comment-face        ; // comments
                              font-lock-delimiter-face      ; block / table delimiters
                              font-lock-preprocessor-face   ; element attributes
                              font-lock-variable-name-face  ; attribute names / references
                              font-lock-function-call-face  ; macro names
                              font-lock-type-face           ; block titles
                              font-lock-doc-face))          ; author / revision lines
                (unless (gethash face present) (push face missing)))
              (expect missing :to-be nil)))
        (kill-buffer buf))))

  (it "fontifies a source block with its language major mode"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((buf (asciidoc-test--showcase-buffer)))
      (unwind-protect
          (with-current-buffer buf
            ;; the [source,emacs-lisp] block body should be natively fontified
            (goto-char (point-min))
            (search-forward "(defun greet")
            (goto-char (match-beginning 0))
            (search-forward "defun")
            (expect (get-text-property (match-beginning 0) 'face)
                    :to-equal 'font-lock-keyword-face))
        (kill-buffer buf)))))

(provide 'asciidoc-mode-smoke-test)
;;; asciidoc-mode-smoke-test.el ends here
