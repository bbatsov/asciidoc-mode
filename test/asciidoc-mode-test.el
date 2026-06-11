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
              :to-equal 'asciidoc-title-5-face)))

  (it "does not bleed the title face onto header attribute lines"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; The grammar nests document attributes inside `document_title'; the
    ;; title face must not cover the `:name: value' lines below the title.
    (with-fontified-asciidoc-buffer "= Title\n:author: Jane\n\nBody.\n"
      (let ((pos (string-match ":author:" (buffer-string))))
        ;; the attribute name is metadata, not part of the title
        (expect (asciidoc-test-face-at (+ 1 pos 1))
                :to-equal 'asciidoc-metadata-key-face)
        ;; the leading ":" delimiter is not title-faced
        (expect (asciidoc-test-face-at (1+ pos))
                :not :to-equal 'asciidoc-document-title-face)))))

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
                :to-equal 'asciidoc-code-face))))

  (it "fontifies superscript text and raises it"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "E = mc^2^ today\n"
      (let ((pos (+ (point-min) (string-match "2" "E = mc^2^ today"))))
        (expect (asciidoc-test-face-at pos)
                :to-equal 'asciidoc-superscript-face)
        (expect (get-text-property pos 'display)
                :to-equal (list 'raise asciidoc-superscript-raise)))))

  (it "fontifies subscript text and lowers it"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "H~2~O is water\n"
      (let ((pos (+ (point-min) (string-match "2" "H~2~O is water"))))
        (expect (asciidoc-test-face-at pos)
                :to-equal 'asciidoc-subscript-face)
        (expect (get-text-property pos 'display)
                :to-equal (list 'raise asciidoc-subscript-raise)))))

  (it "leaves the superscript delimiters on the baseline"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "E = mc^2^ today\n"
      ;; the opening `^' must not carry the raise display property
      (let ((caret (+ (point-min) (string-match "\\^2" "E = mc^2^ today"))))
        (expect (get-text-property caret 'display) :to-be nil))))

  (it "fontifies autolinks with the URL face"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "Visit https://example.com for details.\n"
      (let ((pos (string-match "https" "Visit https://example.com for details.")))
        (expect (asciidoc-test-face-at (+ (point-min) pos))
                :to-equal 'asciidoc-url-face))))

  (it "fontifies a link label distinctly from its URL"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "See https://example.com[the docs] now.\n"
      (let ((url (string-match "example" (buffer-string)))
            (label (string-match "the docs" (buffer-string))))
        (expect (asciidoc-test-face-at (+ (point-min) url))
                :to-equal 'asciidoc-url-face)
        (expect (asciidoc-test-face-at (+ (point-min) label))
                :to-equal 'asciidoc-link-face))))

  (it "fontifies footnote marker and text distinctly"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "Claim footnote:[a source] here.\n"
      (expect (asciidoc-test-face-at (+ (point-min) (string-match "footnote" (buffer-string))))
              :to-equal 'asciidoc-footnote-marker-face)
      (expect (asciidoc-test-face-at (+ (point-min) (string-match "a source" (buffer-string))))
              :to-equal 'asciidoc-footnote-text-face)))

  (it "fontifies cross-references with the cross-reference face"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= T\n\nSee <<some-id>> for details.\n"
      (let ((pos (string-match "<<some-id>>" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'asciidoc-cross-reference-face))))

  (it "fontifies anchors with the anchor face"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= T\n\n[[my-anchor]] target.\n"
      (let ((pos (string-match "\\[\\[my-anchor" (buffer-string))))
        (expect (asciidoc-test-face-at (+ pos 2))
                :to-equal 'asciidoc-anchor-face))))

  (it "gives reference faces their aligned looks"
    ;; cross-references read as links (navigable); anchors recede but stay
    ;; findable via an overline; footnote bodies recede into the comment face
    (expect (face-attribute 'asciidoc-cross-reference-face :inherit nil t)
            :to-equal 'link)
    (expect (face-attribute 'asciidoc-anchor-face :overline nil t) :to-be-truthy)
    (expect (face-attribute 'asciidoc-footnote-text-face :inherit nil t)
            :to-equal 'font-lock-comment-face))

  (it "fontifies a highlighted span with the highlight face"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "A #marked# word.\n"
      (let ((pos (string-match "marked" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'asciidoc-highlight-face))))

  (it "fontifies a role name and applies a known role's styling"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "A [.underline]#styled# word.\n"
      (let ((role (string-match "underline" (buffer-string)))
            (text (string-match "styled" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ role))
                :to-equal 'font-lock-preprocessor-face)
        ;; a known role (underline) styles the span text accordingly
        (expect (asciidoc-test-face-at (1+ text))
                :to-equal 'asciidoc-underline-face))))

  (it "leaves an unknown role's span unstyled"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "A [.bogus]#plain# word.\n"
      (let ((text (string-match "plain" (buffer-string))))
        ;; no styling face, and crucially not the highlight/mark face
        (expect (asciidoc-test-face-at (1+ text)) :to-equal 'default))))

  (it "fontifies multiple roles on an unconstrained span"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "A [.big.bold]##word## here.\n"
      (let ((big (string-match "big" (buffer-string)))
            (bold (string-match "bold" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ big))
                :to-equal 'font-lock-preprocessor-face)
        (expect (asciidoc-test-face-at (1+ bold))
                :to-equal 'font-lock-preprocessor-face)))))

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
            :to-equal 'asciidoc-code-face))

  (it "fontifies code inside a table cell"
    (assume asciidoc-test-grammars-available skip-reason)
    (expect (asciidoc-test-mono-face "|===\n| cell `code` text\n|===\n" "`code`")
            :to-equal 'asciidoc-code-face))

  (it "fontifies code inside a quote block"
    (assume asciidoc-test-grammars-available skip-reason)
    (expect (asciidoc-test-mono-face "____\nquoted `code` text\n____\n" "`code`")
            :to-equal 'asciidoc-code-face))

  (it "fontifies code after a stray list marker (no cascade)"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; A `*' list marker with no matching `*' used to put the inline
    ;; parser into an error state spanning the rest of the buffer.
    (expect (asciidoc-test-mono-face
             "* a lone bullet\n\nlater paragraph with `code`.\n" "`code`")
            :to-equal 'asciidoc-code-face))

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
              :to-equal 'asciidoc-code-face)))

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
              :to-equal 'asciidoc-markup-face)))

  (it "fontifies literal block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "....\nlit\n....\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-markup-face)))

  (it "fontifies passthrough block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "++++\npass\n++++\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-markup-face)))

  (it "fontifies open block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "--\nopen\n--\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-markup-face)))

  (it "fontifies quote block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "____\nquote\n____\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-markup-face)))

  (it "fontifies example block delimiters"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "====\nexample\n====\n"
      (expect (asciidoc-test-face-at 1)
              :to-equal 'asciidoc-markup-face))))

;;; Font-lock: list markers

(describe "Font-lock: list markers"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fades unordered and ordered markers into the markup face"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "* one\n"
      (expect (asciidoc-test-face-at (point-min)) :to-equal 'asciidoc-markup-face))
    (with-fontified-asciidoc-buffer ". one\n"
      (expect (asciidoc-test-face-at (point-min)) :to-equal 'asciidoc-markup-face)))

  (it "keeps the checkbox distinct while fading its bullet"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "* [x] done\n"
      ;; the `*' bullet fades, but the `[x]' checkbox stays a meaningful token
      (expect (asciidoc-test-face-at (point-min)) :to-equal 'asciidoc-markup-face)
      (let ((box (string-match "\\[x\\]" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ box))
                :to-equal 'font-lock-constant-face)))))

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
              :to-equal 'asciidoc-markup-face)))

  (it "fontifies table cell format specifiers"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "|===\na| x\n|===\n"
      ;; The "a" cell-format spec on line 2 should get preprocessor face.
      (let ((pos (string-match "a| x" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-preprocessor-face))))

  (it "fontifies the CSV table fence"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ",===\nName,Age\n,===\n"
      (expect (asciidoc-test-face-at 1) :to-equal 'asciidoc-markup-face)))

  (it "fontifies the DSV table fence"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ":===\na:b:c\n:===\n"
      (expect (asciidoc-test-face-at 1) :to-equal 'asciidoc-markup-face)))

  (it "fontifies inline markup inside a CSV cell"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ",===\nName,*bold*\n,===\n"
      (let ((pos (string-match "\\*bold" (buffer-string))))
        (expect (asciidoc-test-face-at (+ (point-min) pos)) :to-equal 'bold))))

  (it "fontifies inline markup inside a DSV cell"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ":===\nTerm:_def_\n:===\n"
      (let ((pos (string-match "_def" (buffer-string))))
        (expect (asciidoc-test-face-at (+ (point-min) pos)) :to-equal 'italic)))))

;;; Font-lock: description lists

(describe "Font-lock: description lists"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies the term and the `::' marker distinctly"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "CPU:: The brain.\n"
      (expect (asciidoc-test-face-at (point-min))
              :to-equal 'font-lock-keyword-face)
      (let ((marker (string-match "::" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ marker))
                :to-equal 'asciidoc-markup-face))))

  (it "fontifies inline markup inside a description definition"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "CPU:: The *brain* of it.\n"
      (let ((pos (string-match "\\*brain" (buffer-string))))
        (expect (asciidoc-test-face-at (+ (point-min) pos)) :to-equal 'bold)))))

;;; Font-lock: attributes

(describe "Font-lock: attributes"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "fontifies document attribute name"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ":author: Someone\n"
      ;; "author" should get the metadata-key face
      (expect (asciidoc-test-face-at 2)
              :to-equal 'asciidoc-metadata-key-face)))

  (it "fontifies document attribute value"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer ":author: Jane Doe\n"
      (let ((pos (string-match "Jane" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'asciidoc-metadata-value-face))))

  (it "highlights inline attribute references"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; The grammar does not parse `{name}' references, so a font-lock
    ;; keyword highlights them.
    (with-fontified-asciidoc-buffer "= T\n\nSee {version} now.\n"
      (let ((pos (string-match "{version}" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'font-lock-variable-name-face)))))

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
                :to-equal 'asciidoc-code-face))))

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
  (it "resolves json to an available json mode"
    (expect (memq (asciidoc--code-block-lang-mode "json")
                  '(js-json-mode json-ts-mode json-mode))
            :to-be-truthy))
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

  (it "leaves the code face when fontification is disabled"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((asciidoc-fontify-code-blocks-natively nil))
      (with-fontified-asciidoc-buffer
          "= T\n\n[source,emacs-lisp]\n----\n(defun foo () nil)\n----\n"
        (let ((pos (string-match "defun" (buffer-string))))
          (expect (asciidoc-test-face-at (1+ pos))
                  :to-equal 'asciidoc-code-face)))))

  (it "respects the size cap"
    (assume asciidoc-test-grammars-available skip-reason)
    (let ((asciidoc-fontify-code-blocks-natively 3))
      (with-fontified-asciidoc-buffer
          "= T\n\n[source,emacs-lisp]\n----\n(defun foo () nil)\n----\n"
        (let ((pos (string-match "defun" (buffer-string))))
          (expect (asciidoc-test-face-at (1+ pos))
                  :to-equal 'asciidoc-code-face)))))

  (it "does not natively fontify a plain listing block"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= T\n\n----\n(defun foo () nil)\n----\n"
      (let ((pos (string-match "defun" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'asciidoc-code-face))))

  (it "keeps the code face for an unknown language"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "= T\n\n[source,nosuchlang]\n----\nplain text\n----\n"
      (let ((pos (string-match "plain" (buffer-string))))
        (expect (asciidoc-test-face-at (1+ pos))
                :to-equal 'asciidoc-code-face))))

  (it "does not recurse on a [source,asciidoc] block"
    (assume asciidoc-test-grammars-available skip-reason)
    ;; Resolving the language to `asciidoc-mode' itself must not re-enter
    ;; native fontification; the body keeps the verbatim code face.
    (with-fontified-asciidoc-buffer
        "= T\n\n[source,asciidoc]\n----\n== Nested\n----\n"
      (let ((pos (string-match "Nested" (buffer-string))))
        (expect (asciidoc-test-face-at pos)
                :to-equal 'asciidoc-code-face)))))

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

;;; Reference navigation

(describe "Reference navigation"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "jumps from a cross-reference to its anchor"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "[[target]] The target.\n\nSee <<target>> now.\n"
      (re-search-forward "<<t")
      (backward-char)
      (asciidoc-follow-reference-at-point)
      (expect (looking-at "\\[\\[target\\]\\]") :to-be-truthy)))

  (it "jumps from an xref macro to a shorthand anchor"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "[#sec] Section.\n\nSee xref:sec[the section].\n"
      (re-search-forward "xref:s")
      (backward-char)
      (asciidoc-follow-reference-at-point)
      (expect (looking-at "\\[#sec\\]") :to-be-truthy)))

  (it "pushes the mark before jumping"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "[[target]] here.\n\nSee <<target>>.\n"
      (re-search-forward "<<t")
      (backward-char)
      (let ((from (point)))
        (asciidoc-follow-reference-at-point)
        (expect (mark) :to-equal from))))

  (it "opens a bare URL with browse-url"
    (assume asciidoc-test-grammars-available skip-reason)
    (let (followed)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq followed url))))
        (with-fontified-asciidoc-buffer "Visit https://example.com today.\n"
          (re-search-forward "https")
          (backward-char)
          (asciidoc-follow-reference-at-point)
          (expect followed :to-equal "https://example.com")))))

  (it "opens a link macro target with browse-url"
    (assume asciidoc-test-grammars-available skip-reason)
    (let (followed)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq followed url))))
        (with-fontified-asciidoc-buffer "See link:https://example.com[the site].\n"
          (re-search-forward "link:h")
          (backward-char)
          (asciidoc-follow-reference-at-point)
          (expect followed :to-equal "https://example.com")))))

  (it "follows a cross-reference to a section by its auto-id"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "= D\n\n== Getting Started\n\nSee <<_getting_started>>.\n"
      (re-search-forward "<<_")
      (backward-char)
      (asciidoc-follow-reference-at-point)
      (expect (looking-at "== Getting Started") :to-be-truthy)))

  (it "follows a natural-title cross-reference to its section"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "== Getting Started\n\nSee <<Getting Started>>.\n"
      (re-search-forward "<<G")
      (backward-char)
      (asciidoc-follow-reference-at-point)
      (expect (looking-at "== Getting Started") :to-be-truthy)))

  (it "errors when a cross-reference has no anchor"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "See <<missing>> here.\n"
      (re-search-forward "<<m")
      (backward-char)
      (expect (asciidoc-follow-reference-at-point) :to-throw 'user-error)))

  (it "marks reference text as clickable"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "[[target]] x\n\nSee <<target>> now.\n"
      (let ((pos (+ (point-min) (string-match "<<target" (buffer-string)))))
        (expect (get-text-property pos 'keymap)
                :to-equal asciidoc-reference-map))))

  (it "underlines reference text under the mouse pointer"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "[[target]] x\n\nSee <<target>> now.\n"
      (let ((pos (+ (point-min) (string-match "<<target" (buffer-string)))))
        (expect (get-text-property pos 'mouse-face)
                :to-equal 'asciidoc-link-mouse-face)
        (expect (face-attribute 'asciidoc-link-mouse-face :underline nil t)
                :to-be-truthy)))))

;;; Section id generation

(describe "Section id generation"
  (it "matches Asciidoctor's default algorithm"
    (with-temp-buffer
      (expect (asciidoc--section-id "Introduction to AsciiDoc")
              :to-equal "_introduction_to_asciidoc")
      (expect (asciidoc--section-id "What's New?") :to-equal "_what_s_new")
      (expect (asciidoc--section-id "Section 1.2") :to-equal "_section_1_2")))

  (it "honors a custom idseparator"
    (with-temp-buffer
      (insert ":idseparator: -\n")
      (expect (asciidoc--section-id "Hello World") :to-equal "_hello-world")))

  (it "honors a custom idprefix"
    (with-temp-buffer
      (insert ":idprefix: sect_\n")
      (expect (asciidoc--section-id "Goals") :to-equal "sect_goals"))))

;;; Xref backend

(describe "Xref backend"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "reports the cross-reference id at point"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "See <<my-id>> now.\n"
      (re-search-forward "<<m")
      (backward-char)
      (expect (xref-backend-identifier-at-point 'asciidoc) :to-equal "my-id")))

  (it "reports the id of an xref macro at point"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "See xref:my-id[text].\n"
      (re-search-forward "xref:m")
      (backward-char)
      (expect (xref-backend-identifier-at-point 'asciidoc) :to-equal "my-id")))

  (it "resolves a definition to its explicit anchor"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "[[my-id]] here.\n\nSee <<my-id>>.\n"
      (let* ((defs (xref-backend-definitions 'asciidoc "my-id"))
             (loc (xref-item-location (car defs))))
        (expect (length defs) :to-equal 1)
        (goto-char (marker-position (xref-location-marker loc)))
        (expect (looking-at "\\[\\[my-id\\]\\]") :to-be-truthy))))

  (it "resolves a definition to a section by auto-id"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "= D\n\n== Getting Started\n\nSee <<_getting_started>>.\n"
      (let* ((defs (xref-backend-definitions 'asciidoc "_getting_started"))
             (loc (xref-item-location (car defs))))
        (goto-char (marker-position (xref-location-marker loc)))
        (expect (looking-at "== Getting Started") :to-be-truthy))))

  (it "resolves a definition by section title"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "== Getting Started\n\nSee <<Getting Started>>.\n"
      (let* ((defs (xref-backend-definitions 'asciidoc "Getting Started"))
             (loc (xref-item-location (car defs))))
        (goto-char (marker-position (xref-location-marker loc)))
        (expect (looking-at "== Getting Started") :to-be-truthy))))

  (it "offers explicit and section ids for completion"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "= D\n\n== Intro\n\n[[boom]] x\n"
      (let ((ids (xref-backend-identifier-completion-table 'asciidoc)))
        (expect (member "boom" ids) :to-be-truthy)
        (expect (member "_intro" ids) :to-be-truthy))))

  (it "finds <<id>> and xref:id[] references to an anchor"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer
        "[[foo]] def.\n\nSee <<foo>>, xref:foo[here], and <<foo,cap>>.\n"
      (expect (length (xref-backend-references 'asciidoc "foo")) :to-equal 3)))

  (it "reports the anchor id at point on a definition"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "[[my-anchor]] here.\n"
      (goto-char (point-min))
      (search-forward "my-anchor")
      (backward-char)
      (expect (xref-backend-identifier-at-point 'asciidoc)
              :to-equal "my-anchor"))))

;;; Cross-reference completion

(describe "Cross-reference completion"
  (it "completes ids and titles inside `<<'"
    (with-temp-buffer
      (insert "== My Section\n\n[[explicit]] x\n\nSee <<")
      (goto-char (point-max))
      (let* ((cap (asciidoc--capf))
             (cands (all-completions "" (nth 2 cap))))
        (expect (member "explicit" cands) :to-be-truthy)
        (expect (member "_my_section" cands) :to-be-truthy)
        (expect (member "My Section" cands) :to-be-truthy))))

  (it "completes ids inside `xref:'"
    (with-temp-buffer
      (insert "[[explicit]] x\n\nSee xref:")
      (goto-char (point-max))
      (let* ((cap (asciidoc--capf))
             (cands (all-completions "" (nth 2 cap))))
        (expect (member "explicit" cands) :to-be-truthy))))

  (it "bounds completion to the id text after `<<'"
    (with-temp-buffer
      (insert "See <<my-i")
      (goto-char (point-max))
      (let ((cap (asciidoc--capf)))
        (expect (buffer-substring-no-properties (nth 0 cap) (nth 1 cap))
                :to-equal "my-i"))))

  (it "completes attribute names inside `{'"
    (with-temp-buffer
      (insert ":custom-attr: v\n\nSee {cu")
      (goto-char (point-max))
      (let* ((cap (asciidoc--capf))
             (cands (all-completions "" (nth 2 cap))))
        (expect (member "custom-attr" cands) :to-be-truthy)
        (expect (member "doctitle" cands) :to-be-truthy))))

  (it "completes the language after `[source,'"
    (with-temp-buffer
      (insert "[source,ru")
      (goto-char (point-max))
      (let* ((cap (asciidoc--capf))
             (cands (all-completions "" (nth 2 cap))))
        (expect (member "ruby" cands) :to-be-truthy))))

  (it "does not complete in the reftext after a comma"
    (with-temp-buffer
      (insert "See <<id,the te")
      (goto-char (point-max))
      (expect (asciidoc--capf) :to-be nil)))

  (it "does not complete outside a cross reference"
    (with-temp-buffer
      (insert "just some text here")
      (goto-char (point-max))
      (expect (asciidoc--capf) :to-be nil)))

  (it "does not complete after the reference is closed"
    (with-temp-buffer
      (insert "See <<id>> and ")
      (goto-char (point-max))
      (expect (asciidoc--capf) :to-be nil))))

;;; Cross-file references

(describe "Cross-file references (Antora)"
  :var (root pages skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))
  (before-each
    (setq root (make-temp-file "asciidoc-antora" t))
    (setq pages (expand-file-name "modules/ROOT/pages" root))
    (make-directory (expand-file-name "basics" pages) t)
    (with-temp-file (expand-file-name "antora.yml" root)
      (insert "name: cider\nversion: ~\n"))
    (with-temp-file (expand-file-name "basics/install.adoc" pages)
      (insert "= Install\n\n== Setup Steps\n\ncontent\n"))
    (with-temp-file (expand-file-name "usage.adoc" pages)
      (insert "= Usage\n\nSee xref:basics/install.adoc#setup-steps[here].\n")))
  (after-each
    (dolist (b (buffer-list))
      (when (and (buffer-file-name b) (string-prefix-p root (buffer-file-name b)))
        (kill-buffer b)))
    (delete-directory root t))

  (it "detects the Antora context"
    (with-current-buffer (find-file-noselect (expand-file-name "usage.adoc" pages))
      (let ((ctx (asciidoc--antora-context)))
        (expect (plist-get ctx :module) :to-equal "ROOT")
        (expect (plist-get ctx :component) :to-equal "cider"))))

  (it "resolves a page fragment using Antora id rules"
    (with-current-buffer (find-file-noselect (expand-file-name "usage.adoc" pages))
      (let ((m (asciidoc--resolve-reference "basics/install.adoc#setup-steps")))
        (expect (file-name-nondirectory (buffer-file-name (marker-buffer m)))
                :to-equal "install.adoc")
        (with-current-buffer (marker-buffer m)
          (goto-char m)
          (expect (looking-at "== Setup Steps") :to-be-truthy)))))

  (it "resolves a bare page to the top of the file"
    (with-current-buffer (find-file-noselect (expand-file-name "usage.adoc" pages))
      (let ((m (asciidoc--resolve-reference "basics/install.adoc")))
        (expect (marker-position m) :to-equal 1)
        (expect (file-name-nondirectory (buffer-file-name (marker-buffer m)))
                :to-equal "install.adoc"))))

  (it "returns nil for a missing target file"
    (with-current-buffer (find-file-noselect (expand-file-name "usage.adoc" pages))
      (expect (asciidoc--resolve-reference "basics/nope.adoc#x") :to-be nil)))

  (it "opens the target file when following a cross-file xref"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-current-buffer (find-file-noselect (expand-file-name "usage.adoc" pages))
      (font-lock-ensure)
      (goto-char (point-min))
      (re-search-forward "install.adoc#s")
      (backward-char)
      (asciidoc-follow-reference-at-point)
      (expect (get-file-buffer (expand-file-name "basics/install.adoc" pages))
              :to-be-truthy))))

(describe "Cross-file references (generic)"
  (it "resolves a relative path against the current file's directory"
    (let ((root (make-temp-file "asciidoc-generic" t)))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "sub" root))
            (with-temp-file (expand-file-name "sub/target.adoc" root)
              (insert "= Target\n\n[[anchor]] here.\n"))
            (with-temp-file (expand-file-name "main.adoc" root)
              (insert "= Main\n\nSee it.\n"))
            (with-current-buffer (find-file-noselect (expand-file-name "main.adoc" root))
              (let ((m (asciidoc--resolve-reference "sub/target.adoc#anchor")))
                (expect (file-name-nondirectory (buffer-file-name (marker-buffer m)))
                        :to-equal "target.adoc")
                (with-current-buffer (marker-buffer m)
                  (goto-char m)
                  (expect (looking-at "\\[\\[anchor\\]\\]") :to-be-truthy)))))
        (dolist (b (buffer-list))
          (when (and (buffer-file-name b) (string-prefix-p root (buffer-file-name b)))
            (kill-buffer b)))
        (delete-directory root t)))))

;;; Flyspell

(describe "Flyspell integration"
  :var (skip-reason)
  (before-all
    (unless asciidoc-test-grammars-available
      (setq skip-reason "tree-sitter grammars not installed")))

  (it "registers a flyspell verify predicate"
    (expect (get 'asciidoc-mode 'flyspell-mode-predicate)
            :to-equal 'asciidoc--flyspell-verify))

  (it "checks words in ordinary prose"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "Some ordinary prose here.\n"
      (goto-char (point-min))
      (search-forward "prose")
      (expect (asciidoc--flyspell-verify) :to-be-truthy)))

  (it "skips words inside a link"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "See https://zzqqzz.example here.\n"
      (goto-char (point-min))
      (search-forward "zzqqzz")
      (expect (asciidoc--flyspell-verify) :to-be nil)))

  (it "skips words inside a cross-reference"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "See <<some-section>> now.\n"
      (goto-char (point-min))
      (search-forward "section")
      (expect (asciidoc--flyspell-verify) :to-be nil)))

  (it "skips words inside inline code"
    (assume asciidoc-test-grammars-available skip-reason)
    (with-fontified-asciidoc-buffer "Run `somefn` to start.\n"
      (goto-char (point-min))
      (search-forward "somefn")
      (expect (asciidoc--flyspell-verify) :to-be nil))))

;;; Flymake

(describe "Flymake (Asciidoctor)"
  (it "parses per-line diagnostics into typed reports"
    (with-temp-buffer
      (insert "line one\nline two\nline three\n")
      (let ((diags (asciidoc--flymake-parse-output
                    (concat "asciidoctor: WARNING: <stdin>: line 2: invalid reference\n"
                            "asciidoctor: ERROR: <stdin>: Line 3: missing include\n")
                    (current-buffer) 0)))
        (expect (length diags) :to-equal 2)
        (expect (flymake-diagnostic-type (nth 0 diags)) :to-equal :warning)
        (expect (flymake-diagnostic-type (nth 1 diags)) :to-equal :error))))

  (it "surfaces a fatal failure with no locatable line as a buffer error"
    (with-temp-buffer
      (insert "= T\n\nbody\n")
      (let ((diags (asciidoc--flymake-parse-output
                    "asciidoctor: FAILED: missing converter for backend\n"
                    (current-buffer) 1)))
        (expect (length diags) :to-equal 1)
        (expect (flymake-diagnostic-type (car diags)) :to-equal :error))))

  (it "ignores unrelated output"
    (with-temp-buffer
      (insert "body\n")
      (expect (asciidoc--flymake-parse-output "all good\n" (current-buffer) 0)
              :to-be nil)))

  (it "registers the backend in asciidoc-mode buffers"
    (with-temp-buffer
      (asciidoc-mode)
      (expect (memq #'asciidoc-flymake flymake-diagnostic-functions)
              :to-be-truthy)))

  (it "reports an include error end to end"
    (assume (executable-find asciidoc-asciidoctor-command)
            "asciidoctor not installed")
    (let ((buf (generate-new-buffer "fm-test.adoc")) result done)
      (unwind-protect
          (with-current-buffer buf
            (setq buffer-file-name
                  (expand-file-name "fm-test.adoc" temporary-file-directory))
            (asciidoc-mode)
            (insert "= T\n\ninclude::no-such-file-xyz.adoc[]\n")
            (asciidoc-flymake (lambda (diags &rest _) (setq result diags done t)))
            (let ((n 0))
              (while (and (not done) (< n 100))
                (accept-process-output nil 0.1)
                (setq n (1+ n))))
            (expect done :to-be-truthy)
            (expect (> (length result) 0) :to-be-truthy))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;;; Filling

(describe "Filling"
  (it "fills a paragraph containing a URL without a bogus comment prefix"
    (with-asciidoc-buffer
        "Visit https://example.com/a/b for more details about the project here.\n"
      (setq-local fill-column 40)
      (goto-char (point-min))
      (fill-paragraph)
      ;; The `//' inside the URL must not become a fill/comment prefix.
      (expect (string-match-p "^[ \t]*//" (buffer-string)) :to-be nil))))

;;; Heading commands

(describe "Heading commands"
  (cl-flet ((line-after (text cmd)
              (with-asciidoc-buffer text
                (goto-char (point-min))
                (funcall cmd)
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))))

    (it "demotes a heading by adding a marker"
      (expect (line-after "== Section\n" #'asciidoc-demote-heading)
              :to-equal "=== Section"))

    (it "promotes a heading by removing a marker"
      (expect (line-after "=== Section\n" #'asciidoc-promote-heading)
              :to-equal "== Section"))

    (it "refuses to promote past the topmost level"
      (with-asciidoc-buffer "= Title\n"
        (goto-char (point-min))
        (expect (asciidoc-promote-heading) :to-throw 'user-error)))

    (it "refuses to demote past the deepest level"
      (with-asciidoc-buffer "====== Section\n"
        (goto-char (point-min))
        (expect (asciidoc-demote-heading) :to-throw 'user-error)))

    (it "errors when point is not on a heading"
      (with-asciidoc-buffer "Just a paragraph.\n"
        (goto-char (point-min))
        (expect (asciidoc-promote-heading) :to-throw 'user-error)))))

;;; Keymap

(describe "Keymap"
  (it "binds org-style heading keys"
    (expect (keymap-lookup asciidoc-mode-map "M-<left>")
            :to-be 'asciidoc-promote-heading)
    (expect (keymap-lookup asciidoc-mode-map "M-<right>")
            :to-be 'asciidoc-demote-heading)
    (expect (keymap-lookup asciidoc-mode-map "C-c C-n")
            :to-be 'outline-next-visible-heading)
    (expect (keymap-lookup asciidoc-mode-map "C-c C-u")
            :to-be 'outline-up-heading)))

(provide 'asciidoc-mode-test)
;;; asciidoc-mode-test.el ends here
