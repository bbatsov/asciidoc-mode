;;; asciidoc-mode-test.el --- Tests for asciidoc-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Buttercup test suite for asciidoc-mode.

;;; Code:

(require 'test-helper)

(defvar asciidoc-test-grammars-available
  (and (treesit-available-p)
       (treesit-language-available-p 'asciidoc)
       (treesit-language-available-p 'asciidoc-inline))
  "Non-nil if both AsciiDoc tree-sitter grammars are installed.")

;;; Mode activation

(describe "Mode activation"
  (it "associates .adoc files with asciidoc-mode"
    (let ((entry (assoc "\\.adoc\\'" auto-mode-alist)))
      (expect entry :not :to-be nil)
      (expect (cdr entry) :to-be 'asciidoc-mode)))

  (it "associates .asciidoc files with asciidoc-mode"
    (let ((entry (assoc "\\.asciidoc\\'" auto-mode-alist)))
      (expect entry :not :to-be nil)
      (expect (cdr entry) :to-be 'asciidoc-mode)))

  (it "derives from text-mode"
    (with-asciidoc-buffer ""
      (expect (derived-mode-p 'text-mode) :to-be-truthy)))

  (it "sets comment-start"
    (with-asciidoc-buffer ""
      (expect comment-start :to-equal "// "))))

;;; Font-lock: headings

(describe "Font-lock: headings"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies document title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= Document Title\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-document-title-face)))

  (it "fontifies level-1 title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "== Section 1\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-title-1-face)))

  (it "fontifies level-2 title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "=== Section 2\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-title-2-face)))

  (it "fontifies level-3 title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "==== Section 3\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-title-3-face)))

  (it "fontifies level-4 title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "===== Section 4\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-title-4-face)))

  (it "fontifies level-5 title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "====== Section 5\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-title-5-face))))

;;; Font-lock: comments

(describe "Font-lock: comments"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies line comments"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "// a comment\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-comment-face))))

;;; Font-lock: inline

(describe "Font-lock: inline"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies bold text"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "some *bold* text\n"
      ;; The `*bold*' part should have the bold face.
      (let ((star-pos (string-match "\\*bold" "some *bold* text")))
        (expect (asciidoc-test-face-at (+ (point-min) star-pos))
                :to-equal 'bold))))

  (it "fontifies italic text"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "some _italic_ text\n"
      (let ((pos (string-match "_italic" "some _italic_ text")))
        (expect (asciidoc-test-face-at (+ (point-min) pos))
                :to-equal 'italic))))

  (it "fontifies monospace text"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "some `mono` text\n"
      (let ((pos (string-match "`mono" "some `mono` text")))
        (expect (asciidoc-test-face-at (+ (point-min) pos))
                :to-equal 'font-lock-string-face)))))

;;; Font-lock: blocks

(describe "Font-lock: blocks"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies listing block body"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "----\nsome code\n----\n"
      ;; "some code" starts at line 2
      (goto-char (point-min))
      (forward-line 1)
      (expect (asciidoc-test-face-at (point))
              :to-equal 'font-lock-string-face))))

;;; Font-lock: attributes

(describe "Font-lock: attributes"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies document attribute name"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ":author: Someone\n"
      ;; "author" should get variable-name face
      (expect (asciidoc-test-face-at 2)
              :to-equal 'font-lock-variable-name-face))))

;;; Font-lock: admonitions

(describe "Font-lock: admonitions"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies NOTE admonition"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "NOTE: This is a note.\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-keyword-face)))

  (it "fontifies WARNING admonition"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "WARNING: This is a warning.\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-warning-face))))

;;; Imenu

(describe "Imenu"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "creates section entries"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-asciidoc-buffer "== First\n\n=== Second\n"
      (let ((index (treesit-simple-imenu)))
        ;; Should have a "Section" group
        (expect (assoc "Section" index) :not :to-be nil)))))

(provide 'asciidoc-mode-test)
;;; asciidoc-mode-test.el ends here
