;;; lnav.el --- Mode-agnostic TAB navigation through bracketed structures -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 lunixose
;;
;; Author: lunixose <https://github.com/lunixose>
;; Maintainer: lunixose <lunixose@protonmail.com>
;; Keywords: convenience, editing
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.4.0
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
;; `lnav' provides mode-agnostic navigation and editing through
;; bracketed structures via <tab> and <backtab>, plus a chunk model
;; where each pair ((), [], {}, quotes, ...) is a chunk that can nest.
;;
;;   {|       -> {|}       (tab-in)
;;   {|}      -> {}|       (tab-out)
;;   |{}      -> {|}       (tab-in)
;;   |{}      -> {}|       (two taps)
;;   {}|      -> {|       (backtab-into-empty)
;;
;; Chunk navigation (per chunk, four positions):
;;
;;   C-c n   next chunk boundary          C-c p   previous chunk boundary
;;   C-c b   jump before open             C-c a   jump after open
;;   C-c c   jump before close            C-c d   jump after close
;;   C-c i   descend into first child     C-c o   ascend to parent
;;   C-c s   select chunk (inside)        C-c S   select chunk (around)
;;   C-c w   surround region/chunk        C-c x   delete enclosing pair
;;   C-c r   change enclosing pair
;;   C-c >   slurp forward                C-c <   slurp backward
;;   C-c ]   barf forward                 C-c [   barf backward
;;   C-c k   kill sexp                    C-c t   transpose sexp
;;   C-c R   raise sexp                   C-c W   wrap whole sexp
;;   C-c f   forward sexp                 C-c F   backward sexp
;;   C-c N   next sexp                    C-c P   previous sexp
;;   C-c D   down sexp                    C-c U   up sexp
;;   C-c h   kill hybrid sexp             C-c j   join sexp
;;   C-c L   split sexp                   C-c m   select next thing
;;   C-c z   flash chunks                 C-c Z   flash chars
;;   C-c /   flash search
;;
;; In evil, `il' and `al' select the chunk inside / around point,
;; usable with any operator: `dil', `cil', `yal', etc.  `gs'
;; surrounds the region (visual) or chunk at point (normal), `gS'
;; deletes the enclosing pair, `gC' changes it, `g(' / `g)'
;; slurp, `g{' / `g}' barf, `gt' transpose, `gK' kill, `gr'
;; raise, `gw' wrap, `gf' / `gb' move by form, `gz' / `gZ'
;; flash chunks / chars.  All rebindable.
;;
;; Extra files: lnav-flash.el (flash label jumping, treesit flash),
;; lnav-structural.el (smartparens-style commands), lnav-typing.el
;; (auto-pair typing, strict mode), lnav-extras.el (pair highlight,
;; tags, cheat sheet).  `lnav-cheat-sheet' lists everything.
;;
;; Chunk tree rules: 1-char bracket pairs (()[]{} and any distinct
;; open/close pair in `lnav-pairs') nest.  Quote pairs (same open
;; and close char) are non-nesting leaves toggling open/close at
;; their bracket nesting level, matching vim `i"' semantics.  This
;; is approximate: quotes inside strings (e.g. the apostrophe in
;; "don't" while a double quote is open) may mis-pair.  Multi-char
;; pairs (LaTeX \begin{}, markdown **) participate in tab jump but
;; not in the chunk tree.
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
;;
;; Run the test suite with:
;;
;;   emacs -Q --batch -L . -l lnav-test.el -f ert-run-tests-batch-and-exit

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
    ;; Chunk navigation.
    (define-key map (kbd "C-c n") #'lnav-next-chunk)
    (define-key map (kbd "C-c p") #'lnav-previous-chunk)
    (define-key map (kbd "C-c b") #'lnav-jump-before-open)
    (define-key map (kbd "C-c a") #'lnav-jump-after-open)
    (define-key map (kbd "C-c c") #'lnav-jump-before-close)
    (define-key map (kbd "C-c d") #'lnav-jump-after-close)
    (define-key map (kbd "C-c i") #'lnav-chunk-in)
    (define-key map (kbd "C-c o") #'lnav-chunk-out)
    (define-key map (kbd "C-c s") #'lnav-select-chunk)
    (define-key map (kbd "C-c S") #'lnav-select-chunk-around)
    ;; Editing.
    (define-key map (kbd "C-c w") #'lnav-surround)
    (define-key map (kbd "C-c x") #'lnav-delete-enclosing-pair)
    (define-key map (kbd "C-c r") #'lnav-change-enclosing-pair)
    ;; Structural editing.
    (define-key map (kbd "C-c <") #'lnav-slurp-backward)
    (define-key map (kbd "C-c >") #'lnav-slurp-forward)
    (define-key map (kbd "C-c [") #'lnav-barf-backward)
    (define-key map (kbd "C-c ]") #'lnav-barf-forward)
    (define-key map (kbd "C-c k") #'lnav-kill-sexp)
    (define-key map (kbd "C-c t") #'lnav-transpose-sexp)
    (define-key map (kbd "C-c R") #'lnav-raise-sexp)
    (define-key map (kbd "C-c f") #'lnav-forward-sexp)
    (define-key map (kbd "C-c F") #'lnav-backward-sexp)
    (define-key map (kbd "C-c W") #'lnav-wrap-sexp)
    ;; Form navigation.
    (define-key map (kbd "C-c N") #'lnav-next-sexp)
    (define-key map (kbd "C-c P") #'lnav-previous-sexp)
    (define-key map (kbd "C-c D") #'lnav-down-sexp)
    (define-key map (kbd "C-c U") #'lnav-backward-up-sexp)
    (define-key map (kbd "C-c h") #'lnav-kill-hybrid-sexp)
    (define-key map (kbd "C-c j") #'lnav-join-sexp)
    (define-key map (kbd "C-c L") #'lnav-split-sexp)
    (define-key map (kbd "C-c m") #'lnav-select-next-thing)
    ;; Flash.
    (define-key map (kbd "C-c z") #'lnav-flash-chunk)
    (define-key map (kbd "C-c Z") #'lnav-flash-char)
    (define-key map (kbd "C-c /") #'lnav-flash-search)
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

;;; Pair Properties

(defvar lnav-pair-property-alist nil
  "Alist mapping an OPEN string to a plist of pair properties.
Supported keys:
  :wrap         function called when the open is typed over a region
  :autoskip     non-nil to skip an existing close instead of doubling
  :post-handlers list of functions run after the pair is inserted")

(defun lnav-set-pair-property (open &rest props)
  "Set pair property PROPS for OPEN (a key=value list)."
  (setq lnav-pair-property-alist
        (cl-remove open lnav-pair-property-alist :key #'car :test #'string=))
  (push (cons open props) lnav-pair-property-alist))

(defun lnav-pair-property (open key)
  "Return property KEY for the pair with opener OPEN."
  (plist-get (cdr (assoc open lnav-pair-property-alist)) key))

;;;###autoload
(defun lnav-local-pair (modes open close &rest props)
  "Register OPEN/CLOSE pair only in MODES (a list of mode symbols).
PROPS are pair properties (see `lnav-set-pair-property')."
  (declare (indent 1))
  (dolist (mode modes)
    (add-hook (intern (format "%s-hook" mode))
              (lambda ()
                (when (derived-mode-p mode)
                  (setq-local lnav-pairs
                              (cons (cons open close) lnav-pairs))
                  (when props
                    (setq-local lnav-pair-property-alist
                                (cons (cons open props)
                                      lnav-pair-property-alist))))))))

(defmacro lnav-with-delimiters (pairs &rest body)
  "Run BODY with `lnav-pairs' bound to PAIRS."
  (declare (indent 1))
  `(let ((lnav-pairs ,pairs)) ,@body))

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

(defun lnav--bracket-open-close-alist ()
  "Alist of (CHAR . CLOSE-CHAR) for 1-char bracket pairs whose
open and close differ.  These nest in the chunk tree."
  (cl-loop for (open . close) in lnav-pairs
           when (and (= 1 (length open)) (= 1 (length close))
                     (not (string= open close)))
           collect (cons (aref open 0) (aref close 0))))

(defun lnav--quote-chars ()
  "List of 1-char pair chars whose open and close are equal.
These act as non-nesting toggles: each char alternates open and
close at its nesting level."
  (cl-loop for (open . close) in lnav-pairs
           when (and (= 1 (length open)) (= 1 (length close))
                     (string= open close))
           collect (aref open 0)))

(cl-defstruct (lnav--chunk (:constructor lnav--chunk-create))
  "A single pair treated as a navigation chunk."
  open close before-open after-open before-close after-close parent children)

(defun lnav--parse-chunk-tree ()
  "Parse the buffer into a tree of `lnav--chunk' nodes.

Each 1-char bracket pair ()[]{} becomes a chunk; chunks nest.
Quote pairs (same open/close char) are non-nesting leaves: each
occurrence toggles open/close at its bracket nesting level, and
the nearest open quote of the same char is closed.  Multi-char
pairs are ignored.  Unmatched brackets degrade gracefully: the
chunk keeps its open positions and a nil close."
  (let* ((brackets (lnav--bracket-open-close-alist))
         (quotes (lnav--quote-chars))
         (stack nil)
         (roots nil)
         (pos 1))
    (while (< pos (point-max))
      (let* ((ch (char-after pos))
             (open-bracket (assoc ch brackets))
             (close-bracket (rassoc ch brackets)))
        (cond
         (open-bracket
          (let ((node (lnav--chunk-create
                       :open (char-to-string ch)
                       :close (char-to-string (cdr open-bracket))
                       :before-open pos
                       :after-open (1+ pos))))
            (if stack
                (progn
                  (setf (lnav--chunk-parent node) (car stack))
                  (setf (lnav--chunk-children (car stack))
                        (nconc (lnav--chunk-children (car stack)) (list node))))
              (push node roots))
            (push node stack)))
         ((memq ch quotes)
          (let ((idx (cl-position-if
                      (lambda (n)
                        (and (not (assoc (aref (lnav--chunk-open n) 0) brackets))
                             (= (aref (lnav--chunk-open n) 0) ch)))
                      stack)))
            (if idx
                (let ((node (nth idx stack)))
                  (setf (lnav--chunk-before-close node) pos
                        (lnav--chunk-after-close node) (1+ pos))
                  (setq stack (cl-subseq stack (1+ idx))))
              (let ((node (lnav--chunk-create
                           :open (char-to-string ch)
                           :close (char-to-string ch)
                           :before-open pos
                           :after-open (1+ pos))))
                (if stack
                    (progn
                      (setf (lnav--chunk-parent node) (car stack))
                      (setf (lnav--chunk-children (car stack))
                            (nconc (lnav--chunk-children (car stack)) (list node))))
                  (push node roots))
                (push node stack)))))
         (close-bracket
          (let ((idx (cl-position-if
                      (lambda (n)
                        (= (aref (lnav--chunk-open n) 0) (car close-bracket)))
                      stack)))
            (when idx
              (let ((node (nth idx stack)))
                (setf (lnav--chunk-before-close node) pos
                      (lnav--chunk-after-close node) (1+ pos))
                (setq stack (cl-subseq stack (1+ idx)))))))))
        (setq pos (1+ pos)))
    (nreverse roots)))

(defun lnav--chunk-boundaries ()
  "Return a sorted list of chunk-boundary positions in the current
buffer.  A boundary is one of:
  - position 1 (start of buffer)
  - point-max (end of buffer)
  - immediately before or after the opening of any chunk
  - immediately before or after the closing of any closed chunk"
  (let ((bounds (list 1 (point-max))))
    (cl-labels ((walk (node)
                 (push (lnav--chunk-before-open node) bounds)
                 (push (lnav--chunk-after-open node) bounds)
                 (when (lnav--chunk-before-close node)
                   (push (lnav--chunk-before-close node) bounds)
                   (push (lnav--chunk-after-close node) bounds))
                 (dolist (child (lnav--chunk-children node))
                   (walk child))))
      (dolist (root (lnav--parse-chunk-tree))
        (walk root)))
    (sort (delete-dups bounds) #'<)))

;;;###autoload
(defun lnav-next-chunk ()
  "Move cursor to the next chunk boundary.  See
`lnav--chunk-boundaries'.  If the cursor is already at the last
boundary, moves to end of buffer."
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

(defun lnav--chunk-at-point (&optional pos)
  "Return the innermost chunk containing POS (default point), or
nil.  An unclosed chunk is treated as extending to end of buffer."
  (let ((pos (or pos (point)))
        (found nil))
    (cl-labels ((walk (node)
                 (when (and (<= (lnav--chunk-before-open node) pos)
                            (< pos (or (lnav--chunk-after-close node)
                                       (1+ (point-max)))))
                   (setq found node)
                   (dolist (child (lnav--chunk-children node))
                     (walk child)))))
      (dolist (root (lnav--parse-chunk-tree))
        (walk root)))
    found))

(defun lnav--chunk-required ()
  "Return the innermost chunk at point, or signal a user-error."
  (or (lnav--chunk-at-point)
      (user-error "No chunk at point")))

;;;###autoload
(defun lnav-jump-before-open ()
  "Move to immediately before the opening delimiter of the
innermost chunk containing point."
  (interactive "*")
  (goto-char (lnav--chunk-before-open (lnav--chunk-required))))

;;;###autoload
(defun lnav-jump-after-open ()
  "Move to immediately after the opening delimiter of the
innermost chunk containing point."
  (interactive "*")
  (goto-char (lnav--chunk-after-open (lnav--chunk-required))))

;;;###autoload
(defun lnav-jump-before-close ()
  "Move to immediately before the closing delimiter of the
innermost chunk containing point."
  (interactive "*")
  (let ((chunk (lnav--chunk-required)))
    (or (lnav--chunk-before-close chunk)
        (user-error "Chunk not closed"))
    (goto-char (lnav--chunk-before-close chunk))))

;;;###autoload
(defun lnav-jump-after-close ()
  "Move to immediately after the closing delimiter of the
innermost chunk containing point."
  (interactive "*")
  (let ((chunk (lnav--chunk-required)))
    (or (lnav--chunk-after-close chunk)
        (user-error "Chunk not closed"))
    (goto-char (lnav--chunk-after-close chunk))))

;;;###autoload
(defun lnav-chunk-in ()
  "Descend into the first child chunk of the chunk at point."
  (interactive "*")
  (let ((kids (lnav--chunk-children (lnav--chunk-required))))
    (if kids
        (goto-char (lnav--chunk-before-open (car kids)))
      (user-error "No child chunk"))))

;;;###autoload
(defun lnav-chunk-out ()
  "Ascend to the start of the parent chunk of the chunk at point."
  (interactive "*")
  (let ((parent (lnav--chunk-parent (lnav--chunk-required))))
    (if parent
        (goto-char (lnav--chunk-before-open parent))
      (user-error "No parent chunk"))))

;;;###autoload
(defun lnav-select-chunk ()
  "Select the contents of the chunk at point (inside the
delimiters) with the region."
  (interactive "*")
  (let ((chunk (lnav--chunk-required)))
    (or (lnav--chunk-before-close chunk)
        (user-error "Chunk not closed"))
    (push-mark (lnav--chunk-before-close chunk) t t)
    (goto-char (lnav--chunk-after-open chunk))))

;;;###autoload
(defun lnav-select-chunk-around ()
  "Select the chunk at point including its delimiters with the
region."
  (interactive "*")
  (let ((chunk (lnav--chunk-required)))
    (or (lnav--chunk-after-close chunk)
        (user-error "Chunk not closed"))
    (push-mark (lnav--chunk-after-close chunk) t t)
    (goto-char (lnav--chunk-before-open chunk))))

;;; Editing Operations

(defun lnav--delimiter-pair (delimiter)
  "Return (OPEN . CLOSE) for DELIMITER.  If DELIMITER matches a
registered open or close in `lnav-pairs', its mate is used;
otherwise DELIMITER wraps itself (e.g. `\"' gives (\"\" . \"\"'))."
  (cond
   ((assoc delimiter lnav-pairs)
    (cons delimiter (cdr (assoc delimiter lnav-pairs))))
   ((rassoc delimiter lnav-pairs)
    (cons (car (rassoc delimiter lnav-pairs)) delimiter))
   (t (cons delimiter delimiter))))

(defun lnav--wrap-region (beg end open close &optional reselect)
  "Wrap region BEG..END with OPEN and CLOSE, leaving point after
OPEN.  With RESELECT non-nil, leaves the full wrapped region
selected instead."
  (let ((open-len (length open))
        (close-len (length close)))
    (goto-char end)
    (insert close)
    (goto-char beg)
    (insert open)
    (if reselect
        (progn
          (push-mark (+ end open-len close-len) t t)
          (goto-char beg))
      (goto-char (+ beg open-len)))))

;;;###autoload
(defun lnav-surround (delimiter)
  "Wrap the active region, or the chunk at point, in DELIMITER.

DELIMITER is a string.  If it names a registered open or close in
`lnav-pairs', its registered mate is used; otherwise DELIMITER
wraps itself (e.g. `\"' gives \"\").  Multi-char delimiters work.

With an active region, wraps the region (re-selecting it).  With
no region, wraps the contents of the innermost chunk at point.

Editing is done with programmatic insert, so smartparens and
electric-pair-mode (which only act on self-insert) never add a
second pair."
  (interactive (list (read-string "Delimiter: ")))
  (let* ((pair (lnav--delimiter-pair delimiter))
         (open (car pair))
         (close (cdr pair)))
    (if (use-region-p)
        (lnav--wrap-region (region-beginning) (region-end) open close t)
      (let ((chunk (lnav--chunk-required)))
        (or (lnav--chunk-before-close chunk)
            (user-error "Chunk not closed"))
        (lnav--wrap-region (lnav--chunk-after-open chunk)
                           (lnav--chunk-before-close chunk)
                           open close)))))

;;;###autoload
(defun lnav-delete-enclosing-pair ()
  "Remove the delimiters of the innermost chunk at point, keeping
its contents.  Point moves to the start of the former contents."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (open (lnav--chunk-open chunk))
         (close (lnav--chunk-close chunk))
         (bopen (lnav--chunk-before-open chunk))
         (bclose (lnav--chunk-before-close chunk)))
    (or bclose (user-error "Chunk not closed"))
    (delete-region bclose (+ bclose (length close)))
    (delete-region bopen (+ bopen (length open)))
    (goto-char bopen)))

;;;###autoload
(defun lnav-change-enclosing-pair (delimiter)
  "Replace the delimiters of the innermost chunk at point with
DELIMITER's pair (see `lnav--delimiter-pair')."
  (interactive (list (read-string "New delimiter: ")))
  (let* ((chunk (lnav--chunk-required))
         (pair (lnav--delimiter-pair delimiter))
         (old-open (lnav--chunk-open chunk))
         (old-close (lnav--chunk-close chunk))
         (bopen (lnav--chunk-before-open chunk))
         (bclose (lnav--chunk-before-close chunk)))
    (or bclose (user-error "Chunk not closed"))
    (goto-char bclose)
    (delete-region bclose (+ bclose (length old-close)))
    (insert (cdr pair))
    (goto-char bopen)
    (delete-region bopen (+ bopen (length old-open)))
    (insert (car pair))
    (goto-char (+ bopen (length (car pair))))))

;;; Structural Editing

(defun lnav--form-bounds-forward (pos)
  "Return (BEG . END) of the form starting at or after POS.
Whitespace is skipped.  What counts as a form is decided by the
buffer's syntax table via `forward-sexp'."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\r\n")
    (when (< (point) (point-max))
      (let ((beg (point)))
        (condition-case nil
            (progn (forward-sexp 1) (cons beg (point)))
          (scan-error nil))))))

(defun lnav--form-bounds-backward (pos)
  "Return (BEG . END) of the form ending at or before POS.
Whitespace is skipped.  Syntax-table aware via `backward-sexp'."
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t\r\n")
    (when (> (point) (point-min))
      (let ((end (point)))
        (condition-case nil
            (progn (backward-sexp 1) (cons (point) end))
          (scan-error nil))))))

(defun lnav--last-form-in-chunk (chunk)
  "Return (BEG . END) of the last form inside CHUNK, or nil."
  (when-let ((bclose (lnav--chunk-before-close chunk)))
    (save-excursion
      (goto-char bclose)
      (skip-chars-backward " \t\r\n")
      (let ((end (point)))
        (when (> end (lnav--chunk-after-open chunk))
          (condition-case nil
              (progn (backward-sexp 1) (cons (point) end))
            (scan-error nil)))))))

;;;###autoload
(defun lnav-slurp-forward ()
  "Pull the next form into the innermost chunk at point.
`(a) b' -> `(a b)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (close (lnav--chunk-close chunk))
         (bclose (lnav--chunk-before-close chunk))
         (form (and bclose
                    (lnav--form-bounds-forward
                     (lnav--chunk-after-close chunk)))))
    (or form (user-error "Nothing to slurp"))
    (let ((clen (length close)))
      (delete-region bclose (+ bclose clen))
      (goto-char (- (cdr form) clen))
      (insert close))))

;;;###autoload
(defun lnav-slurp-backward ()
  "Pull the previous form into the innermost chunk at point.
`a (b)' -> `(a b)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (open (lnav--chunk-open chunk))
         (bopen (lnav--chunk-before-open chunk))
         (form (lnav--form-bounds-backward bopen)))
    (or form (user-error "Nothing to slurp"))
    (let ((olen (length open)))
      (delete-region bopen (+ bopen olen))
      (goto-char (car form))
      (insert open))))

;;;###autoload
(defun lnav-barf-forward ()
  "Push the last form of the innermost chunk at point out past its
close delimiter.  `(a b) c' -> `(a) b c'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (close (lnav--chunk-close chunk))
         (bopen (lnav--chunk-before-open chunk))
         (bclose (lnav--chunk-before-close chunk))
         (last (and bclose (lnav--last-form-in-chunk chunk))))
    (or last (user-error "Nothing to barf"))
    (let ((ws-beg (save-excursion
                    (goto-char (car last))
                    (skip-chars-backward " \t\r\n")
                    (point)))
          (clen (length close)))
      (when (<= ws-beg (+ bopen (length (lnav--chunk-open chunk))))
        (user-error "Nothing to barf"))
      (delete-region bclose (+ bclose clen))
      (goto-char ws-beg)
      (insert close))))

;;;###autoload
(defun lnav-barf-backward ()
  "Push the first form of the innermost chunk at point out past its
open delimiter.  `a (b c)' -> `a b (c)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (open (lnav--chunk-open chunk))
         (bopen (lnav--chunk-before-open chunk))
         (bclose (lnav--chunk-before-close chunk))
         (aopen (lnav--chunk-after-open chunk)))
    (or bclose (user-error "Chunk not closed"))
    (let* ((first-beg (save-excursion
                        (goto-char aopen)
                        (skip-chars-forward " \t\r\n")
                        (point)))
           (first-end (save-excursion
                        (goto-char first-beg)
                        (condition-case nil
                            (progn (forward-sexp 1) (point))
                          (scan-error nil))))
           (ws-end (and first-end
                        (save-excursion
                          (goto-char first-end)
                          (skip-chars-forward " \t\r\n")
                          (point))))
           (olen (length open)))
      (or (and first-end ws-end (< ws-end bclose))
          (user-error "Nothing to barf"))
      (delete-region bopen (+ bopen olen))
      (goto-char (- ws-end olen))
      (insert open))))

;;;###autoload
(defun lnav-raise-sexp ()
  "Replace the parent chunk at point with the innermost chunk.
`(a (b) c)' with point inside `(b)' -> `(b)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk)))
    (or parent (user-error "No parent chunk"))
    (let ((sexp (buffer-substring (lnav--chunk-before-open chunk)
                                  (lnav--chunk-after-close chunk))))
      (delete-region (lnav--chunk-before-open parent)
                     (lnav--chunk-after-close parent))
      (goto-char (lnav--chunk-before-open parent))
      (insert sexp))))

;;;###autoload
(defun lnav-transpose-sexp ()
  "Swap the chunk at point with the following form.
`(a) (b)' -> `(b) (a)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (cur-beg (lnav--chunk-before-open chunk))
         (cur-end (lnav--chunk-after-close chunk))
         (next (lnav--form-bounds-forward cur-end)))
    (or next (user-error "No next form"))
    (let ((cur (buffer-substring cur-beg cur-end))
          (nxt (buffer-substring (car next) (cdr next))))
      (delete-region cur-beg (cdr next))
      (goto-char cur-beg)
      (insert nxt " " cur))))

;;;###autoload
(defun lnav-kill-sexp ()
  "Kill the chunk at point including its delimiters."
  (interactive "*")
  (let ((chunk (lnav--chunk-required)))
    (kill-region (lnav--chunk-before-open chunk)
                 (lnav--chunk-after-close chunk))))

;;;###autoload
(defun lnav-forward-sexp ()
  "Move to the end of the next form (chunk or atom)."
  (interactive "^")
  (let ((form (lnav--form-bounds-forward (point))))
    (or form (user-error "No next form"))
    (goto-char (cdr form))))

;;;###autoload
(defun lnav-backward-sexp ()
  "Move to the start of the previous form (chunk or atom)."
  (interactive "^")
  (let ((form (lnav--form-bounds-backward (point))))
    (or form (user-error "No previous form"))
    (goto-char (car form))))

;;;###autoload
(defun lnav-wrap-sexp (delimiter)
  "Wrap the chunk at point, delimiters included, in DELIMITER.
`(a)' with `(' -> `((a))'.  See `lnav--delimiter-pair'."
  (interactive (list (read-string "Delimiter: ")))
  (let* ((chunk (lnav--chunk-required))
         (pair (lnav--delimiter-pair delimiter)))
    (lnav--wrap-region (lnav--chunk-before-open chunk)
                       (lnav--chunk-after-close chunk)
                       (car pair) (cdr pair))))

;;;###autoload
(defun lnav-wrap-round ()
  "Wrap the chunk at point in parentheses."
  (interactive "*")
  (lnav-wrap-sexp "("))

;;;###autoload
(defun lnav-wrap-curly ()
  "Wrap the chunk at point in braces."
  (interactive "*")
  (lnav-wrap-sexp "{"))

;;;###autoload
(defun lnav-wrap-square ()
  "Wrap the chunk at point in brackets."
  (interactive "*")
  (lnav-wrap-sexp "["))

;;;###autoload
(defun lnav-wrap-quote ()
  "Wrap the chunk at point in double quotes."
  (interactive "*")
  (lnav-wrap-sexp "\""))

;;; Evil Integration

;;;###autoload
(with-eval-after-load 'evil
  (require 'lnav-evil))

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
mode is in `lnav-excluded-modes' or it is a minibuffer buffer."
  (unless (or (minibufferp)
              (memq major-mode lnav-excluded-modes))
    (lnav-mode 1)))

(provide 'lnav)

(require 'lnav-flash)
(require 'lnav-structural)
(require 'lnav-typing)
(require 'lnav-extras)

;;; lnav.el ends here
