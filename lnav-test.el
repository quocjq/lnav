;;; lnav-test.el --- Tests for lnav -*- lexical-binding: t; -*-
;;
;; Run: emacs -Q --batch -L . -l lnav-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'lnav)

(defvar lnav-test-pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")
    ("\"" . "\"")
    ("'" . "'")
    ("`" . "`")))

(defmacro lnav-test-with-buffer (content &rest body)
  "Run BODY in a temp buffer with CONTENTS and default `lnav-pairs'."
  (declare (indent 1))
  `(with-temp-buffer
     (transient-mark-mode 1)
     (insert ,content)
     (goto-char (point-min))
     (let ((lnav-pairs lnav-test-pairs))
       ,@body)))

(defun lnav-test-roots ()
  "Return chunk tree roots for current buffer."
  (lnav--parse-chunk-tree))

(ert-deftest lnav-tree-nested ()
  (lnav-test-with-buffer "(a(b)c)"
    (let ((root (car (lnav-test-roots))))
      (should (= 1 (lnav--chunk-before-open root)))
      (should (= 2 (lnav--chunk-after-open root)))
      (should (= 7 (lnav--chunk-before-close root)))
      (should (= 8 (lnav--chunk-after-close root)))
      (should (= 1 (length (lnav--chunk-children root))))
      (let ((child (car (lnav--chunk-children root))))
        (should (= 3 (lnav--chunk-before-open child)))
        (should (= 4 (lnav--chunk-after-open child)))
        (should (= 5 (lnav--chunk-before-close child)))
        (should (= 6 (lnav--chunk-after-close child)))
        (should (eq root (lnav--chunk-parent child)))))))

(ert-deftest lnav-tree-siblings ()
  (lnav-test-with-buffer "(a)(b)"
    (let ((roots (lnav-test-roots)))
      (should (= 2 (length roots)))
      (should (= 1 (lnav--chunk-before-open (nth 0 roots))))
      (should (= 4 (lnav--chunk-before-open (nth 1 roots))))
      (should (= 7 (lnav--chunk-after-close (nth 1 roots)))))))

(ert-deftest lnav-tree-empty-pair ()
  (lnav-test-with-buffer "{}"
    (let ((root (car (lnav-test-roots))))
      (should (= 1 (lnav--chunk-before-open root)))
      (should (= 2 (lnav--chunk-after-open root)))
      (should (= 2 (lnav--chunk-before-close root)))
      (should (= 3 (lnav--chunk-after-close root))))))

(ert-deftest lnav-tree-quotes-flat ()
  (lnav-test-with-buffer "(\"a\" \"b\")"
    (let ((root (car (lnav-test-roots)))
          (kids nil))
      (should (= 9 (lnav--chunk-before-close root)))
      (setq kids (lnav--chunk-children root))
      (should (= 2 (length kids)))
      (let ((q1 (nth 0 kids))
            (q2 (nth 1 kids)))
        (should (= 2 (lnav--chunk-before-open q1)))
        (should (= 4 (lnav--chunk-before-close q1)))
        (should (= 6 (lnav--chunk-before-open q2)))
        (should (= 8 (lnav--chunk-before-close q2)))))))

(ert-deftest lnav-tree-apostrophe-inside-quotes ()
  (lnav-test-with-buffer "\"don't\""
    (let ((root (car (lnav-test-roots))))
      (should (= 1 (lnav--chunk-before-open root)))
      (should (= 7 (lnav--chunk-before-close root)))
      (should (= 8 (lnav--chunk-after-close root))))))

(ert-deftest lnav-tree-unmatched-degrades ()
  (lnav-test-with-buffer "(a]"
    (let ((root (car (lnav-test-roots))))
      (should (= 1 (lnav--chunk-before-open root)))
      (should (null (lnav--chunk-before-close root))))))

(ert-deftest lnav-tab-in ()
  (lnav-test-with-buffer "{}"
    (goto-char 1)
    (lnav-jump-forward)
    (should (= 2 (point)))))

(ert-deftest lnav-tab-out-of-empty ()
  (lnav-test-with-buffer "{}"
    (goto-char 2)
    (lnav-jump-forward)
    (should (= 3 (point)))))

(ert-deftest lnav-tab-two-taps ()
  (lnav-test-with-buffer "{}"
    (goto-char 1)
    (lnav-jump-forward)
    (lnav-jump-forward)
    (should (= 3 (point)))))

(ert-deftest lnav-backtab-out-of-empty ()
  (lnav-test-with-buffer "{}"
    (goto-char 2)
    (lnav-jump-backward)
    (should (= 1 (point)))))

(ert-deftest lnav-backtab-into-empty ()
  (lnav-test-with-buffer "{}"
    (goto-char 3)
    (lnav-jump-backward)
    (should (= 2 (point)))))

(ert-deftest lnav-tab-multi-char-pair ()
  (lnav-test-with-buffer "\\begin{}"
    (let ((lnav-pairs '(("\\begin{" . "\\end{"))))
      (goto-char 1)
      (lnav-jump-forward)
      (should (= 8 (point)))
      (lnav-jump-forward)
      (should (= 9 (point))))))

(ert-deftest lnav-backtab-multi-char-pair ()
  (lnav-test-with-buffer "\\begin{}\\end{"
    (let ((lnav-pairs '(("\\begin{" . "\\end{"))))
      (goto-char 14)
      (lnav-jump-backward)
      (should (= 9 (point))))))

(ert-deftest lnav-evil-text-object-range ()
  (skip-unless (require 'evil nil t))
  (lnav-test-with-buffer "(ab)"
    (goto-char 3)
    (should (equal (list 2 4) (lnav--evil-chunk-range t)))
    (should (equal (list 1 5) (lnav--evil-chunk-range nil)))))

(ert-deftest lnav-chunk-at-point-innermost ()
  (lnav-test-with-buffer "(a(b)c)"
    (goto-char 4)
    (let ((chunk (lnav--chunk-at-point)))
      (should (= 3 (lnav--chunk-before-open chunk)))
      (should (= 5 (lnav--chunk-before-close chunk))))
    (goto-char 2)
    (let ((chunk (lnav--chunk-at-point)))
      (should (= 1 (lnav--chunk-before-open chunk))))))

(ert-deftest lnav-boundaries-include-after-close ()
  (lnav-test-with-buffer "(a)"
    (should (equal '(1 2 3 4) (lnav--chunk-boundaries)))))

(ert-deftest lnav-jump-commands ()
  (lnav-test-with-buffer "(ab)"
    (goto-char 3)
    (lnav-jump-after-open)
    (should (= 2 (point)))
    (lnav-jump-before-close)
    (should (= 4 (point)))
    (lnav-jump-after-close)
    (should (= 5 (point)))
    (goto-char 3)
    (lnav-jump-before-open)
    (should (= 1 (point)))))

(ert-deftest lnav-next-previous-chunk ()
  (lnav-test-with-buffer "(a) b (c)"
    (goto-char 1)
    (lnav-next-chunk)
    (should (= 2 (point)))
    (lnav-next-chunk)
    (should (= 3 (point)))
    (goto-char (point-max))
    (lnav-previous-chunk)
    (should (= 9 (point)))
    (lnav-previous-chunk)
    (should (= 8 (point)))))

(ert-deftest lnav-chunk-in-out ()
  (lnav-test-with-buffer "(a(b)c)"
    (goto-char 2)
    (lnav-chunk-in)
    (should (= 3 (point)))
    (lnav-chunk-out)
    (should (= 1 (point)))
    (should-error (lnav-chunk-out))))

(ert-deftest lnav-select-chunk-region ()
  (lnav-test-with-buffer "(ab)"
    (goto-char 3)
    (lnav-select-chunk)
    (should (= (region-beginning) 2))
    (should (= (region-end) 4))
    (lnav-select-chunk-around)
    (should (= (region-beginning) 1))
    (should (= (region-end) 5))))

(ert-deftest lnav-delimiter-pair ()
  (lnav-test-with-buffer "x"
    (should (equal '("(" . ")") (lnav--delimiter-pair "(")))
    (should (equal '("(" . ")") (lnav--delimiter-pair ")")))
    (should (equal '("\"" . "\"") (lnav--delimiter-pair "\"")))
    (should (equal '("**" . "**") (lnav--delimiter-pair "**")))))

(ert-deftest lnav-surround-region ()
  (lnav-test-with-buffer "ab"
    (goto-char 3)
    (push-mark 1 nil t)
    (lnav-surround "(")
    (should (equal "(ab)" (buffer-string)))
    (should (equal 1 (region-beginning)))
    (should (equal 5 (region-end)))))

(ert-deftest lnav-surround-self-closing-quote ()
  (lnav-test-with-buffer "ab"
    (goto-char 3)
    (push-mark 1 nil t)
    (lnav-surround "\"")
    (should (equal "\"ab\"" (buffer-string)))))

(ert-deftest lnav-surround-chunk ()
  (lnav-test-with-buffer "(ab)"
    (goto-char 3)
    (lnav-surround "[")
    (should (equal "([ab])" (buffer-string)))
    (should (= 3 (point)))))

(ert-deftest lnav-delete-enclosing-pair ()
  (lnav-test-with-buffer "(ab)"
    (goto-char 3)
    (lnav-delete-enclosing-pair)
    (should (equal "ab" (buffer-string)))
    (should (= 1 (point)))))

(ert-deftest lnav-delete-enclosing-quotes ()
  (lnav-test-with-buffer "\"ab\""
    (goto-char 3)
    (lnav-delete-enclosing-pair)
    (should (equal "ab" (buffer-string)))))

(ert-deftest lnav-change-enclosing-pair ()
  (lnav-test-with-buffer "(ab)"
    (goto-char 3)
    (lnav-change-enclosing-pair "[")
    (should (equal "[ab]" (buffer-string)))
    (should (= 2 (point)))))

(ert-deftest lnav-surround-no-double-pair-with-smartparens ()
  (skip-unless (require 'smartparens nil t))
  (lnav-test-with-buffer "ab"
    (smartparens-mode 1)
    (goto-char 3)
    (push-mark 1 nil t)
    (lnav-surround "(")
    (should (equal "(ab)" (buffer-string)))
    (smartparens-mode -1)))

(provide 'lnav-test)

;;; lnav-test.el ends here
