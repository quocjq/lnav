;;; lnav-typing.el --- typing behavior and strict mode for lnav -*- lexical-binding: t; -*-
;;
;; `lnav-typing-mode' (off by default): auto-pair open delimiters,
;; skip over existing closing delimiters, delete empty pairs on
;; backspace, and wrap an active region when typing an open.
;; `lnav-strict-mode': refuse deletions that would unbalance bracket
;; pairs.

;;; Code:

(require 'cl-lib)

(defvar lnav-pairs nil)
(defvar lnav-typing-mode nil)
(defvar lnav-strict-mode nil)

(declare-function lnav--char-after-p "lnav" (str))
(declare-function lnav--forward-empty-pair-at-point-p "lnav" ())
(declare-function lnav--bracket-open-close-alist "lnav" ())
(declare-function lnav-pair-property "lnav" (open key))

(defgroup lnav-typing nil
  "Typing behavior for lnav."
  :group 'lnav
  :prefix "lnav-typing")

(defcustom lnav-typing-wrap-region t
  "Wrap an active region when typing an opening delimiter."
  :type 'boolean
  :group 'lnav-typing)

;;;###autoload
(define-minor-mode lnav-typing-mode
  "Auto-pair delimiters while typing, skip existing closes, delete
empty pairs on backspace.  Off by default so parinfer users are
unaffected."
  :lighter " ltp"
  :group 'lnav-typing
  (if lnav-typing-mode
      (add-hook 'post-self-insert-hook #'lnav-typing--post-self-insert nil t)
    (remove-hook 'post-self-insert-hook #'lnav-typing--post-self-insert t)))

(defun lnav-typing--skip-p ()
  "Non-nil if typing handling should not run for the current command."
  (or (memq this-command
            '(dabbrev-expand yas-expand company-complete-common))
      (and (boundp 'parinfer-rust-mode) parinfer-rust-mode)))

(defun lnav-typing--post-self-insert ()
  "Auto-pair, skip, or wrap for the character just inserted."
  (when (and lnav-typing-mode (not (lnav-typing--skip-p)))
    (let* ((ch (char-before))
           (ch-str (and ch (char-to-string ch)))
           (open-pair (and ch-str (assoc ch-str lnav-pairs #'string=)))
           (close-pair (and ch-str (cl-rassoc ch-str lnav-pairs :test #'string=))))
      (cond
       ;; A closing delimiter that already follows: skip over it.
       ((and close-pair (lnav--char-after-p ch-str))
        (delete-char -1)
        (forward-char (length ch-str)))
       ;; An opening delimiter: auto-insert the close, or wrap region.
       (open-pair
        (let ((close (cdr open-pair)))
          (cond
           ((and lnav-typing-wrap-region (use-region-p))
            (let ((beg (region-beginning))
                  (end (region-end)))
              (delete-char -1)
              (goto-char end)
              (insert close)
              (goto-char beg)
              (insert (car open-pair))
              (setq mark-active t)))
           ((and (string= (car open-pair) close)
                 (lnav--char-after-p close))
            ;; Same-char pair already present: skip instead of doubling.
            (delete-char -1)
            (forward-char (length close)))
           ((not (lnav--char-after-p close))
            (insert close)
            (backward-char (length close))))))))))

(defun lnav-typing--backward-delete-char (&rest _args)
  "Delete an empty pair when backspacing between its delimiters."
  (when (and lnav-typing-mode (not (lnav-typing--skip-p)))
    (when-let ((close (lnav--forward-empty-pair-at-point-p)))
      (delete-char (length close)))))

(advice-add 'backward-delete-char :before #'lnav-typing--backward-delete-char)

;;; Strict mode

(defun lnav--balanced-p ()
  "Non-nil if every registered bracket pair in the buffer is balanced."
  (let ((counts (make-hash-table)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((ch (char-after)))
          (dolist (p (lnav--bracket-open-close-alist))
            (when (= ch (car p))
              (cl-incf (gethash (car p) counts 0)))
            (when (= ch (cdr p))
              (cl-decf (gethash (car p) counts 0))))
          (forward-char 1))))
    (cl-every (lambda (k) (zerop (gethash k counts 0)))
              (hash-table-keys counts))))

(defun lnav-strict--delete-region (beg end)
  "Refuse deletions that would unbalance bracket pairs."
  (when lnav-strict-mode
    (let ((diff (make-hash-table)))
      (save-excursion
        (goto-char beg)
        (while (< (point) (min end (point-max)))
          (let ((ch (char-after)))
            (dolist (p (lnav--bracket-open-close-alist))
              (when (= ch (car p))
                (cl-incf (gethash (car p) diff 0)))
              (when (= ch (cdr p))
                (cl-decf (gethash (car p) diff 0)))))
          (forward-char 1)))
      (when (cl-some (lambda (k) (not (zerop (gethash k diff 0))))
                     (hash-table-keys diff))
        (user-error "lnav-strict: deletion would unbalance pairs")))))

(advice-add 'delete-region :before #'lnav-strict--delete-region)

;;;###autoload
(define-minor-mode lnav-strict-mode
  "Refuse edits that unbalance bracket pairs."
  :lighter " lst"
  :group 'lnav-typing)

(provide 'lnav-typing)

;;; lnav-typing.el ends here
