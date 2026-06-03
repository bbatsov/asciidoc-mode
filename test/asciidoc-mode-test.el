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
      (expect comment-start :to-equal "// ")))

  (it "enables outline-minor-mode"
    (assume asciidoc-test-grammars-available
            "tree-sitter grammars not installed")
    (with-asciidoc-buffer ""
      (expect outline-minor-mode :to-be-truthy))))

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
                :to-equal 'font-lock-string-face))))

  (it "fontifies autolinks"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "Visit https://example.com for details.\n"
      (let ((pos (string-match "https" "Visit https://example.com for details.")))
        (expect (asciidoc-test-face-at (+ (point-min) pos))
                :to-equal 'font-lock-constant-face)))))

;;; Font-lock: inline content inside blocks
;;
;; The inline parser is restricted to inline-content ranges via
;; `treesit-range-settings'.  These tests guard that inline markup is
;; still fontified inside the various containers, and that block markup
;; no longer poisons inline fontification later in the buffer.

(describe "Font-lock: inline content inside blocks"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies code inside an unordered list item"
    (assume asciidoc-test-grammars-available skip-reason)
    (expect (asciidoc-test-mono-face "* item with `code` here\n" "`code`")
            :to-equal 'font-lock-string-face))

  (it "fontifies code inside a table cell"
    (assume asciidoc-test-grammars-available skip-reason)
    (expect (asciidoc-test-mono-face "|===\n| cell `code` text\n|===\n" "`code`")
            :to-equal 'font-lock-string-face))

  (it "fontifies code inside a quote block"
    (assume asciidoc-test-grammars-available skip-reason)
    (expect (asciidoc-test-mono-face "____\nquoted `code` text\n____\n" "`code`")
            :to-equal 'font-lock-string-face))

  (it "fontifies code after a stray list marker (no cascade)"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; A `*' list marker with no matching `*' used to put the inline
    ;; parser into an error state spanning the rest of the buffer.
    (expect (asciidoc-test-mono-face
             "* a lone bullet\n\nlater paragraph with `code`.\n" "`code`")
            :to-equal 'font-lock-string-face))

  (it "produces no inline parse error for a mixed document"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "= Title\n\n* one\n* two\n* three\n\nText `code` and more.\n"
      (let ((parser (car (treesit-parser-list nil 'asciidoc-inline))))
        (expect (string-match-p
                 "ERROR" (treesit-node-string (treesit-parser-root-node parser)))
                :to-be nil)))))

;;; Font-lock: blocks

(describe "Font-lock: blocks"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies listing block body"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "----\nsome code\n----\n"
      (goto-char (point-min))
      (forward-line 1)
      (expect (asciidoc-test-face-at (point))
              :to-equal 'font-lock-string-face)))

  (it "fontifies indented literal blocks"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= T\n\n  indented literal\n"
      (goto-char (point-min))
      (forward-line 2)
      (expect (asciidoc-test-face-at (point))
              :to-equal 'font-lock-string-face)))

  (it "fontifies markdown-style quote blocks"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= T\n\n> A quoted line\n"
      (goto-char (point-min))
      (forward-line 2)
      (expect (asciidoc-test-face-at (point))
              :to-equal 'font-lock-doc-face)))

  (it "fontifies thematic breaks"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= T\n\n'''\n"
      (goto-char (point-min))
      (forward-line 2)
      (expect (asciidoc-test-face-at (point))
              :to-equal 'font-lock-comment-delimiter-face))))

;;; Font-lock: block delimiters

(describe "Font-lock: block delimiters"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies listing block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "----\ncode\n----\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-delimiter-face)))

  (it "fontifies literal block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "....\nlit\n....\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-delimiter-face)))

  (it "fontifies passthrough block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "++++\npass\n++++\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-delimiter-face)))

  (it "fontifies open block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "--\nopen\n--\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-delimiter-face)))

  (it "fontifies quote block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "____\nquote\n____\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-delimiter-face)))

  (it "fontifies example block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "====\nexample\n====\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-delimiter-face))))

;;; Font-lock: tables

(describe "Font-lock: tables"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies table delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "|===\n| A | B\n|===\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'font-lock-delimiter-face)))

  (it "fontifies table cell format specifiers"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "|===\na| x\n|===\n"
      ;; The "a" cell-format spec on line 2 should get preprocessor face.
      (let ((pos (string-match "a| x" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-preprocessor-face)))))

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
              :to-equal 'font-lock-variable-name-face)))

  (it "fontifies document attribute value"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ":author: Jane Doe\n"
      (let ((pos (string-match "Jane" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-string-face)))))

;;; Font-lock: admonitions

(describe "Font-lock: admonitions"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies the NOTE label as a keyword"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; The whole "NOTE:" label is highlighted via a font-lock keyword.
    (with-fontified-asciidoc-buffer "= Title\n\nNOTE: This is a note.\n"
      (let ((pos (string-match "NOTE:" (buffer-string))))
        ;; the keyword itself
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-keyword-face)
        ;; the trailing colon
        (expect (asciidoc-test-face-at (+ 1 pos 4))
                :to-equal 'font-lock-keyword-face))))

  (it "does not clobber inline markup in the admonition body"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; Inline `code` inside an admonition body must still be fontified as
    ;; monospace, not overridden by the admonition label highlighting.
    (with-fontified-asciidoc-buffer "= Title\n\nNOTE: see `code` here.\n"
      (let ((pos (string-match "`code`" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-string-face))))

  (it "recognizes all admonition labels"
    (assume asciidoc-test-grammars-available skip-reason)
    (dolist (label '("NOTE" "TIP" "IMPORTANT" "CAUTION" "WARNING"))
      (with-fontified-asciidoc-buffer (format "= Title\n\n%s: body.\n" label)
        (let ((pos (string-match label (buffer-string))))
          (expect (asciidoc-test-face-at (1+ pos))
                  :to-equal 'font-lock-keyword-face))))))

;;; Native source block fontification

(describe "Source block language extraction"
  (it "extracts the language from a source block attribute list"
    (expect (asciidoc--code-block-language "source,ruby") :to-equal "ruby"))
  (it "handles an empty leading style"
    (expect (asciidoc--code-block-language ",js") :to-equal "js"))
  (it "ignores positional style options"
    (expect (asciidoc--code-block-language "source%nowrap,python")
            :to-equal "python"))
  (it "returns nil for non-source styles"
    (expect (asciidoc--code-block-language "NOTE") :to-be nil)
    (expect (asciidoc--code-block-language "quote") :to-be nil))
  (it "returns nil when there is no language"
    (expect (asciidoc--code-block-language "source") :to-be nil)))

(describe "Source block language mode resolution"
  (it "resolves a single mapped mode"
    (let ((asciidoc-code-lang-modes '(("foo" . emacs-lisp-mode))))
      (expect (asciidoc--code-block-lang-mode "foo") :to-equal 'emacs-lisp-mode)))
  (it "resolves a candidate list to the first available mode"
    (let ((asciidoc-code-lang-modes
           '(("foo" . (asciidoc-no-such-mode emacs-lisp-mode)))))
      (expect (asciidoc--code-block-lang-mode "foo") :to-equal 'emacs-lisp-mode)))
  (it "falls back to LANG-mode when not in the alist"
    (let ((asciidoc-code-lang-modes nil))
      (expect (asciidoc--code-block-lang-mode "emacs-lisp")
              :to-equal 'emacs-lisp-mode)))
  (it "returns nil when no candidate is available"
    (let ((asciidoc-code-lang-modes '(("foo" . (asciidoc-no-such-mode)))))
      (expect (asciidoc--code-block-lang-mode "foo") :to-be nil))))

(describe "Native source block fontification"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies a source block with the language major mode"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "= T\n\n[source,emacs-lisp]\n----\n(defun foo () nil)\n----\n"
      (let ((pos (string-match "defun" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-keyword-face))))

  (it "leaves the string face when fontification is disabled"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((asciidoc-fontify-code-blocks-natively nil))
      (with-fontified-asciidoc-buffer
          "= T\n\n[source,emacs-lisp]\n----\n(defun foo () nil)\n----\n"
        (let ((pos (string-match "defun" (buffer-string))))
          (expect (asciidoc-test-face-at (1+ pos))
                  :to-equal 'font-lock-string-face)))))

  (it "respects the size cap"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((asciidoc-fontify-code-blocks-natively 3))
      (with-fontified-asciidoc-buffer
          "= T\n\n[source,emacs-lisp]\n----\n(defun foo () nil)\n----\n"
        (let ((pos (string-match "defun" (buffer-string))))
          (expect (asciidoc-test-face-at (1+ pos))
                  :to-equal 'font-lock-string-face)))))

  (it "does not natively fontify a plain listing block"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= T\n\n----\n(defun foo () nil)\n----\n"
      (let ((pos (string-match "defun" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-string-face))))

  (it "keeps the string face for an unknown language"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "= T\n\n[source,nosuchlang]\n----\nplain text\n----\n"
      (let ((pos (string-match "plain" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-string-face))))

  (it "does not recurse on a [source,asciidoc] block"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; Resolving the language to `asciidoc-mode' itself must not re-enter
    ;; native fontification; the body keeps the verbatim string face.
    (with-fontified-asciidoc-buffer
        "= T\n\n[source,asciidoc]\n----\n== Nested\n----\n"
      (let ((pos (string-match "Nested" (buffer-string))))
        (expect (asciidoc-test-face-at pos)
                :to-equal 'font-lock-string-face)))))

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

;;; Navigation

(describe "Navigation"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "moves to next section with beginning-of-defun"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-asciidoc-buffer "== First\n\nSome text.\n\n== Second\n"
      (goto-char (point-min))
      (beginning-of-defun -1)
      (expect (looking-at "== Second") :to-be-truthy)))

  (it "moves to previous section with beginning-of-defun"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-asciidoc-buffer "== First\n\nSome text.\n\n== Second\n"
      (goto-char (point-max))
      (beginning-of-defun)
      (expect (looking-at "== Second") :to-be-truthy)))

  (it "moves forward by sentence across paragraphs"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-asciidoc-buffer "== S\n\nFirst paragraph.\n\nSecond paragraph.\n"
      (goto-char (point-min))
      (search-forward "First")
      (goto-char (match-beginning 0))
      (forward-sentence)
      (expect (looking-at "\nSecond") :to-be-truthy))))

(provide 'asciidoc-mode-test)
;;; asciidoc-mode-test.el ends here
