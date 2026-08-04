;;; lnav.el --- Mode-agnostic TAB navigation through bracketed structures -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 lunixose
;;
;; Author: lunixose <https://github.com/lunixose>
;; Maintainer: lunixose <lunixose@protonmail.com>
;; Keywords: convenience, editing
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1.0
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `lnav' provides mode-agnostic navigation through bracketed
;; structures via <tab> and <backtab>.
;;
;;   {|       -> {|}       (tab-in)
;;   {|}      -> {}|       (tab-out)
;;   |{}      -> {|}       (tab-in)
;;   |{}      -> {}|       (two taps)
;;   {}|      -> {|       (backtab-into-empty)
;;
;; Works in any major mode; only the *fallback* (what TAB does when no
;; pair is in scope) is mode-aware.  Default fallback is
;; `indent-for-tab-command' with org-table awareness.  Snippet frameworks
;; (yasnippet, tempel) take precedence over pair jumps when their fields
;; are active.
;;
;; Define new pairs declaratively:
;;
;;   (lnav-add-pair "\\begin{" "\\end{")   ; LaTeX
;;   (lnav-add-pair "$" "$")               ; inline math
;;   (lnav-add-pair "**" "**")             ; markdown bold
;;
;; Or customize `lnav-pairs' directly.  Per-mode fallbacks:
;;
;;   (lnav-set-fallback #'org-cycle 'org-mode)

;;; Code:

(require 'cl-lib)

(declare-function yas-expand "yasnippet" ())
(declare-function yas-active-snippets "yasnippet" ())
(declare-function tempel-expand "tempel" ())
(declare-function org-at-table-p "org" ())
(declare-function org-table-next-field "org" (&optional _arg))
(declare-function read-function "subr-x" (prompt &optional default))

;;; Customizable Variables

(defgroup lnav nil
  "Mode-agnostic TAB navigation through bracketed structures."
  :group 'convenience
  :prefix 'lnav)

(defcustom lnav-pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")
    ("\"" . "\"")
    ("'" . "'")
    ("`" . "`"))
  "Alist of (OPEN . CLOSE) pairs `lnav-jump-forward' and
`lnav-jump-backward' recognize.  Each side is a string (single
char or multi-char).  Order is irrelevant; longest match wins when
multiple pairs start at the same position."
  :type '(alist :key-type string :value-type string)
  :group 'lnav)

(defcustom lnav-fallback-alist
  '((t . lnav--default-fallback))
  "Alist mapping major-mode symbols to fallback functions invoked
when no pair is in scope.  The `t' key is the default fallback."
  :type '(alist :key-type (choice symbol (const t))
          :value-type function)
  :group 'lnav)

(defcustom lnav-excluded-modes nil
  "Major-mode symbols where `lnav-mode' is disabled.  The
mode remains available but does nothing in these modes."
  :type '(repeat symbol)
  :group 'lnav)

(defcustom lnav-enable-backward t
  "Non-nil means <backtab> invokes `lnav-jump-backward'."
  :type 'boolean
  :group 'lnav)

(defcustom lnav-scan-distance 32
  "Maximum characters to scan on either side of the cursor when
matching multi-char pairs."
  :type 'integer
  :group 'lnav)

;;; Internal State

(defvar lnav-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>")    #'lnav-jump-forward)
    (define-key map (kbd "<backtab>") #'lnav-jump-backward)
    ;; Chunk navigation (testing; remap or remove after evaluation).
    (define-key map (kbd "C-c n") #'lnav-next-chunk)
    (define-key map (kbd "C-c p") #'lnav-previous-chunk)
    map)
  "Keymap used by `lnav-mode'.")

;;; Public API: Pair Management

(defun lnav-add-pair (open close &optional _mode)
  "Register (OPEN . CLOSE) pair for `lnav-mode'.  OPEN and
CLOSE are strings (single or multi-char).  Subsequent calls
overwrite earlier registrations of the same OPEN string."
  (interactive
   (list (read-string "Open: ")
         (read-string "Close: ")))
  (setq lnav-pairs
        (cons (cons open close)
              (cl-remove open lnav-pairs :key #'car :test #'string=))))
;;;###autoload
(defun lnav-remove-pair (open)
  "Remove pair whose OPEN string matches OPEN."
  (interactive "sOpen: ")
  (setq lnav-pairs
        (cl-remove open lnav-pairs :key #'car :test #'string=)))

(defun lnav-set-fallback (function &optional mode)
  "Set fallback FUNCTION for MODE (a major-mode symbol).
If MODE is nil, sets the global default fallback (the `t' key)."
  (interactive
   (list (read-function "Fallback function: ")
         (read-string "Major mode (empty for default): ")))
  (let ((key (or mode t)))
    (let ((entry (assoc key lnav-fallback-alist)))
      (if entry
          (setcdr entry function)
        (push (cons key function) lnav-fallback-alist)))))

;;; Snippet / Completion Detection

(defun lnav--in-snippet-field-p ()
  "Non-nil when a snippet field is active and wants TAB."
  (cond
   ((bound-and-true-p yas--active-snippets)
    (or (and (fboundp 'yas-active-snippets) (yas-active-snippets))
        t))
   ((bound-and-true-p tempel--active)
    t)
   ((and (bound-and-true-p yas--active-snippets) t) t)))

(defun lnav--expand-snippet-or-complete ()
  "Hand TAB off to whichever snippet framework is active."
  (cond
   ((bound-and-true-p yas--active-snippets)
    (call-interactively #'yas-expand))
   ((bound-and-true-p tempel--active)
    (call-interactively #'tempel-expand))
   (t nil)))

;;; Pair Matching Primitives

(defun lnav--char-before-p (str)
  "Return non-nil if the string STR sits immediately before point."
  (let ((len (length str)))
    (and (>= (point) (1+ len))
         (string= (buffer-substring (- (point) len) (point)) str))))

(defun lnav--char-after-p (str)
  "Return non-nil if the string STR sits immediately after point."
  (let ((len (length str)))
    (and (<= (+ (point) len) (point-max))
         (string= (buffer-substring (point) (+ (point) len)) str))))

(defun lnav--forward-empty-pair-at-point-p ()
  "Return the close string if cursor sits between an empty 1-char
registered pair (e.g., {|}, (|)), otherwise nil."
  (let ((before (char-before))
        (after  (char-after)))
    (when (and before after (not (bobp)))
      (let* ((bs (char-to-string before))
             (as (char-to-string after))
             (pair (assoc bs lnav-pairs #'string=)))
        (when (and pair (string= as (cdr pair)))
          as)))))

(defun lnav--backward-empty-pair-at-point-p ()
  "Return the close string if cursor sits between an immediately
adjacent open (before) and close (after) forming an empty pair,
otherwise nil.  Symmetric to forward variant."
  (lnav--forward-empty-pair-at-point-p))

(defun lnav--find-multi-char-close (close start-pos)
  "Search forward from START-POS for CLOSE.  Return start position
of CLOSE, or nil.  Bounded by `lnav-scan-distance'.

Available for future use; current jump logic does not require it."
  (save-excursion
    (goto-char start-pos)
    (let ((end-limit (min (point-max) (+ start-pos lnav-scan-distance))))
      (when (re-search-forward (regexp-quote close) end-limit t)
        (max start-pos (- (point) (length close)))))))

;;; Jump Logic

(defun lnav--try-tab-in ()
  "If cursor sits immediately before a registered open, move past
the open.  Return non-nil on success.

For 1-char pairs this lands between open and close (because close
sits immediately after).  For multi-char pairs (e.g., LaTeX
\\begin{) it lands just after the open, where content begins."
  (cl-loop for (open . _close) in lnav-pairs
           thereis
           (when (lnav--char-after-p open)
             (forward-char (length open))
             t)))

(defun lnav--try-tab-out-of-empty ()
  "If cursor sits between an empty pair, move past close.  Return
non-nil on success."
  (let ((close (lnav--forward-empty-pair-at-point-p)))
    (when close
      (forward-char (length close))
      t)))

(defun lnav--try-tab-back-out-of-empty ()
  "If cursor sits between an empty pair, move to before open.
Return non-nil on success."
  (let ((close (lnav--backward-empty-pair-at-point-p)))
    (when close
      (backward-char (length close))
      t)))

(defun lnav--try-tab-back-into-empty ()
  "If cursor sits immediately after a registered close, move
between open and close.  Return non-nil on success."
  (cl-loop for (open . close) in lnav-pairs
           thereis
           (when (lnav--char-before-p close)
             (let* ((open-end (- (point) (length close)))
                    (search-start (max (point-min)
                                       (- open-end lnav-scan-distance)))
                    (open-found
                     (save-excursion
                       (goto-char open-end)
                       (re-search-backward (regexp-quote open) search-start t))))
               (when open-found
                 (goto-char open-end)
                 t)))))

(defun lnav--fallback ()
  "Invoke the fallback function registered for the current major
mode (or the global default).  Also handles snippet/table expansion
so TAB still does the right thing in those contexts."
  (let ((fn (or (alist-get major-mode lnav-fallback-alist)
                (alist-get t lnav-fallback-alist)
                #'lnav--default-fallback)))
    (funcall fn)))

(defun lnav--default-fallback ()
  "Default fallback: org table next-field, otherwise indent."
  (cond
   ((and (fboundp 'org-at-table-p)
         (derived-mode-p 'org-mode)
         (org-at-table-p))
    (call-interactively #'org-table-next-field))
   (t (indent-for-tab-command))))

;;;###autoload
(defun lnav-jump-forward ()
  "Move forward through the nearest bracketed structure, or fall
back to `lnav--fallback' if none is in scope.

Priority:
  1. Active snippet field (yasnippet / tempel) -> expand.
  2. Cursor immediately before open            -> tab-in.
  3. Cursor between empty pair                 -> tab-out.
  4. Fallback (per-mode or default)."
  (interactive "*")
  (or (lnav--expand-snippet-or-complete)
      (lnav--try-tab-in)
      (lnav--try-tab-out-of-empty)
      (lnav--fallback)))

;;;###autoload
(defun lnav-jump-backward ()
  "Move backward through the nearest bracketed structure, or fall
back if none is in scope.  Symmetric to `lnav-jump-forward'."
  (interactive "*")
  (or (when (and lnav-enable-backward
                 (lnav--expand-snippet-or-complete))
        t)
      (lnav--try-tab-back-out-of-empty)
      (lnav--try-tab-back-into-empty)
      (lnav--fallback)))

;;; Chunk Navigation

(defun lnav--char-pair-p (pair)
  "Non-nil if PAIR is a 1-char-open/1-char-close entry."
  (and (= 1 (length (car pair)))
       (= 1 (length (cdr pair)))))

(defun lnav--open-char-p (ch)
  "Non-nil if CH is a registered 1-char opening bracket."
  (when ch
    (cl-find-if (lambda (pair)
                  (and (lnav--char-pair-p pair)
                       (= ch (aref (car pair) 0))))
                lnav-pairs)))

(defun lnav--matching-close-char (open-char)
  "Return the close char matching OPEN-CHAR, or nil.  Only
considers 1-char pairs."
  (let ((pair (cl-find-if (lambda (p)
                            (and (lnav--char-pair-p p)
                                 (= open-char (aref (car p) 0))))
                          lnav-pairs)))
    (and pair (aref (cdr pair) 0))))

(defun lnav--chunk-boundaries ()
  "Return a sorted list of chunk-boundary positions in the current
buffer.  A boundary is one of:
  - position 1 (start of buffer)
  - point-max (end of buffer)
  - immediately before an opening bracket at chunk 0 (root level)
  - immediately after any opening bracket
  - immediately before a closing bracket that matches the current chunk

Multi-char pairs are ignored.  Unmatched brackets degrade
gracefully."
  (let* ((max-pos (point-max))
         (boundaries (list 1 max-pos))
         (stack '()))
    (save-excursion
      (goto-char 1)
      (let ((bpos 1))
        (while (< bpos max-pos)
          (let* ((ch (char-after bpos))
                 (top (car stack))
                 (mc (and top (lnav--matching-close-char top))))
            (cond
             ;; Stack-top matches this char as its close: closing bracket.
             ((and mc (= mc ch))
              (push bpos boundaries)
              (pop stack))
             ;; This char is an opening bracket.
             ((lnav--open-char-p ch)
              (when (null stack)
                (push bpos boundaries))
              (push (1+ bpos) boundaries)
              (push ch stack)))
            (setq bpos (1+ bpos))))))
    (sort (delete-dups boundaries) #'<)))

;;;###autoload
(defun lnav-next-chunk ()
  "Move cursor to the next chunk boundary.

A chunk boundary is a position immediately inside an opening
bracket, immediately outside a closing bracket, or immediately
before a root-level opening bracket.  See
`lnav--chunk-boundaries' for the precise definition.

If the cursor is already at the last boundary, moves to end of
buffer."
  (interactive "^")
  (let ((target (or (cl-find-if (lambda (b) (> b (point)))
                                (lnav--chunk-boundaries))
                    (point-max))))
    (goto-char target)))

;;;###autoload
(defun lnav-previous-chunk ()
  "Move cursor to the previous chunk boundary.  Reverse of
`lnav-next-chunk'.  If the cursor is already at the first
boundary, moves to start of buffer."
  (interactive "^")
  (let ((target 1))
    (dolist (b (lnav--chunk-boundaries) target)
      (when (< b (point))
        (setq target b)))
    (goto-char target)))

;;; Minor Mode

;;;###autoload
(define-minor-mode lnav-mode
  "Toggle `lnav-mode'.  When enabled, <tab> and <backtab>
navigate bracketed structures."
  :lighter " lsr"
  :keymap lnav-mode-map
  :group 'lnav
  (if (and lnav-mode
           (memq major-mode lnav-excluded-modes))
      (progn
        (setq lnav-mode nil)
        (user-error "lnav-mode disabled for %s" major-mode))
    (setq lnav-mode (if lnav-mode 1 0))))

;;;###autoload
(define-globalized-minor-mode global-lnav-mode lnav-mode
  lnav-on
  "Enable `lnav-mode' globally, respecting `lnav-excluded-modes'.")

(defun lnav-on ()
  "Enable `lnav-mode' for the current buffer unless its major
mode is in `lnav-excluded-modes'."
  (unless (memq major-mode lnav-excluded-modes)
    (lnav-mode 1)))

(provide 'lnav)

;;; lnav.el ends here
