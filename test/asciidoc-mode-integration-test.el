;;; asciidoc-mode-integration-test.el --- Integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests for asciidoc-mode using a realistic AsciiDoc document.

;;; Code:

(require 'test-helper)

(defvar asciidoc-test-grammars-available
  (and (treesit-available-p)
       (treesit-language-available-p 'asciidoc)
       (treesit-language-available-p 'asciidoc-inline))
  "Non-nil if both AsciiDoc tree-sitter grammars are installed.")

(defvar asciidoc-integration-test-file
  (expand-file-name "fixtures/sample.adoc"
                     (file-name-directory (or load-file-name
                                              buffer-file-name)))
  "Path to the sample AsciiDoc fixture file.")

(defmacro with-sample-buffer (&rest body)
  "Open the sample fixture in `asciidoc-mode' with fontification, then run BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert-file-contents asciidoc-integration-test-file)
     (goto-char (point-min))
     (asciidoc-mode)
     (font-lock-ensure)
     ,@body))

(defun asciidoc-test-face-at-match (string &optional occurrence)
  "Search for STRING and return the face at the start of match.
OCCURRENCE selects which match (default 1)."
  (goto-char (point-min))
  (dotimes (_ (or occurrence 1))
    (search-forward string))
  (get-text-property (match-beginning 0) 'face))

;;; Font-lock integration

(describe "Integration: font-lock"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies document title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "= Sample Document")
              :to-equal 'asciidoc-document-title-face)))

  (it "fontifies level-1 headings"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "== Getting Started")
              :to-equal 'asciidoc-title-1-face)))

  (it "fontifies level-2 headings"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "=== Installation")
              :to-equal 'asciidoc-title-2-face)))

  (it "fontifies level-3 headings"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "==== Advanced Options")
              :to-equal 'asciidoc-title-3-face)))

  (it "fontifies bold text"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "*bold*")
              :to-equal 'bold)))

  (it "fontifies italic text"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "_italic_")
              :to-equal 'italic)))

  (it "fontifies monospace text"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "`monospace`")
              :to-equal 'font-lock-string-face)))

  (it "fontifies listing block body"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "def hello")
              :to-equal 'font-lock-string-face)))

  (it "fontifies indented literal block"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "  $ make install")
              :to-equal 'font-lock-string-face)))

  (it "fontifies line comments"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "// This is a line comment")
              :to-equal 'font-lock-comment-face)))

  (it "fontifies thematic breaks"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "'''")
              :to-equal 'font-lock-comment-delimiter-face)))

  (it "fontifies markdown-style quote blocks"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "> This is a markdown")
              :to-equal 'font-lock-doc-face)))

  (it "fontifies block titles"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match ".A block title")
              :to-equal 'font-lock-type-face)))

  (it "fontifies ordered list markers"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match ". Download")
              :to-equal 'font-lock-constant-face)))

  (it "fontifies unordered list markers"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "* Enable feature A")
              :to-equal 'font-lock-constant-face)))

  (it "fontifies admonition content"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      ;; The admonition node starts at ": " after the keyword.
      (goto-char (point-min))
      (search-forward "NOTE:")
      (expect (get-text-property (match-beginning 0) 'face)
              :to-be nil)
      (expect (get-text-property (+ (match-beginning 0) 4) 'face)
              :to-equal 'font-lock-keyword-face)))

  (it "fontifies block macro name"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "toc::")
              :to-equal 'font-lock-function-call-face))))

;;; Block-level override

(describe "Integration: block overrides inline"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "unordered list markers override spurious emphasis"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "* Enable feature A")
              :to-equal 'font-lock-constant-face)))

  (it "nested list markers override spurious emphasis"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (expect (asciidoc-test-face-at-match "** Nested item")
              :to-equal 'font-lock-constant-face)))

  (it "headings after list markers are not affected by inline misparse"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      ;; == Final Notes comes after the list section with * markers
      (expect (asciidoc-test-face-at-match "== Final Notes")
              :to-equal 'asciidoc-title-1-face)))

  (it "listing block body overrides inline misparse"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      ;; Code inside a listing block should get string face even if
      ;; the inline parser sees emphasis markers.
      (expect (asciidoc-test-face-at-match "def hello")
              :to-equal 'font-lock-string-face))))

;;; Navigation integration

(describe "Integration: navigation"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "navigates forward through sections with beginning-of-defun"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (beginning-of-defun -1)
      (expect (looking-at "== Getting Started") :to-be-truthy)
      (beginning-of-defun -1)
      (expect (looking-at "=== Installation") :to-be-truthy)
      (beginning-of-defun -1)
      (expect (looking-at "=== Configuration") :to-be-truthy)))

  (it "navigates backward through sections with beginning-of-defun"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (goto-char (point-max))
      (beginning-of-defun)
      (expect (looking-at "== Final Notes") :to-be-truthy)
      (beginning-of-defun)
      (expect (looking-at "== Macros and Links") :to-be-truthy)
      (beginning-of-defun)
      (expect (looking-at "== Admonitions and Blocks") :to-be-truthy)))

  (it "moves forward by sentence across block elements"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (search-forward "== Code Examples")
      (forward-line 1)
      ;; On "A listing block:" paragraph
      (forward-sentence)
      ;; Should land at end of that paragraph / start of next block
      (expect (point) :to-be-greater-than
              (save-excursion
                (goto-char (point-min))
                (search-forward "A listing block:")
                (match-beginning 0))))))

;;; Imenu integration

(describe "Integration: imenu"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "produces a Section group with all headings"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-sample-buffer
      (let* ((index (treesit-simple-imenu))
             (sections (cdr (assoc "Section" index)))
             (names (mapcar #'car sections)))
        (expect (assoc "Section" index) :not :to-be nil)
        (expect (length sections) :to-be-greater-than 5)
        (expect (cl-some (lambda (n) (string-match-p "Getting Started" n))
                         names)
                :to-be-truthy)
        (expect (cl-some (lambda (n) (string-match-p "Final Notes" n))
                         names)
                :to-be-truthy)))))

(provide 'asciidoc-mode-integration-test)
;;; asciidoc-mode-integration-test.el ends here
