;;; lnav-extras.el --- highlight, tags, and cheat sheet for lnav -*- lexical-binding: t; -*-
;;
;; `lnav-show-pair-mode' highlights the matching delimiters of the
;; chunk at point.  `lnav-wrap-tag' / `lnav-rewrap-tag' wrap chunks
;; in markup tags.  `lnav-cheat-sheet' lists all commands.

;;; Code:

(require 'cl-lib)

(declare-function lnav--chunk-at-point "lnav" (&optional pos))
(declare-function lnav--chunk-required "lnav" ())
(declare-function lnav--chunk-before-open "lnav" (chunk))
(declare-function lnav--chunk-after-open "lnav" (chunk))
(declare-function lnav--chunk-before-close "lnav" (chunk))
(declare-function lnav--chunk-after-close "lnav" (chunk))
(declare-function lnav--wrap-region "lnav" (beg end open close &optional reselect))
(declare-function lnav--delimiter-pair "lnav" (delimiter))

(defvar lnav-show-pair-mode nil)

(defface lnav-show-pair-face
  '((t (:inherit highlight :weight bold)))
  "Face for the highlighted matching delimiters.")

(defvar-local lnav--show-overlays nil
  "Active overlays for `lnav-show-pair-mode'.")

(defun lnav-show--clear ()
  "Delete all `lnav-show-pair-mode' overlays."
  (dolist (ov lnav--show-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq lnav--show-overlays nil))

(defun lnav-show--refresh ()
  "Re-highlight the chunk at point."
  (when lnav-show-pair-mode
    (lnav-show--clear)
    (when-let ((chunk (lnav--chunk-at-point)))
      (when (lnav--chunk-before-close chunk)
        (dolist (pos (list (cons (lnav--chunk-before-open chunk)
                                 (lnav--chunk-after-open chunk))
                           (cons (lnav--chunk-before-close chunk)
                                 (lnav--chunk-after-close chunk))))
          (let ((ov (make-overlay (car pos) (cdr pos))))
            (overlay-put ov 'face 'lnav-show-pair-face)
            (push ov lnav--show-overlays)))))))

;;;###autoload
(define-minor-mode lnav-show-pair-mode
  "Highlight the matching delimiters of the chunk at point."
  :lighter " lsp"
  :group 'lnav
  (if lnav-show-pair-mode
      (add-hook 'post-command-hook #'lnav-show--refresh nil t)
    (remove-hook 'post-command-hook #'lnav-show--refresh t)
    (lnav-show--clear)))

;;; Tags

;;;###autoload
(defun lnav-wrap-tag (tag)
  "Wrap the chunk at point in TAG markup, e.g. TAG=`div' gives
`<div>...</div>'."
  (interactive "sTag: ")
  (let* ((chunk (lnav--chunk-required))
         (open (format "<%s>" tag))
         (close (format "</%s>" tag)))
    (lnav--wrap-region (lnav--chunk-before-open chunk)
                       (lnav--chunk-after-close chunk)
                       open close)))

;;;###autoload
(defun lnav-rewrap-tag (tag)
  "Replace the tag wrapping the chunk at point with TAG."
  (interactive "sTag: ")
  (let* ((chunk (lnav--chunk-required))
         (bopen (lnav--chunk-before-open chunk))
         (aclose (lnav--chunk-after-close chunk)))
    (save-excursion
      (goto-char bopen)
      (when (re-search-backward "<[^/>][^>]*>" (max (point-min) (- bopen 200)) t)
        (let ((tag-beg (match-beginning 0))
              (tag-end (match-end 0)))
          (delete-region tag-beg tag-end)
          (goto-char aclose)
          (when (re-search-forward "</[^>]*>" (min (point-max) (+ aclose 200)) t)
            (delete-region (match-beginning 0) (match-end 0))))))
    (goto-char (lnav--chunk-before-open chunk))
    (insert (format "<%s>" tag))
    (goto-char (lnav--chunk-after-close chunk))
    (insert (format "</%s>" tag))))

;;; Cheat sheet

;;;###autoload
(defun lnav-cheat-sheet ()
  "Show a buffer listing all lnav commands."
  (interactive)
  (with-output-to-temp-buffer "*lnav cheat sheet*"
    (princ "lnav commands\n==============\n\n")
    (dolist (group
             '(("Navigation" . (lnav-next-chunk lnav-previous-chunk
                                  lnav-jump-before-open lnav-jump-after-open
                                  lnav-jump-before-close lnav-jump-after-close
                                  lnav-chunk-in lnav-chunk-out))
               ("Form navigation" . (lnav-forward-sexp lnav-backward-sexp
                                       lnav-next-sexp lnav-previous-sexp
                                       lnav-forward-parallel-sexp
                                       lnav-backward-parallel-sexp
                                       lnav-down-sexp lnav-backward-down-sexp
                                       lnav-backward-up-sexp
                                       lnav-beginning-of-sexp lnav-end-of-sexp
                                       lnav-beginning-of-next-sexp
                                       lnav-end-of-next-sexp
                                       lnav-beginning-of-previous-sexp
                                       lnav-end-of-previous-sexp
                                       lnav-forward-symbol lnav-backward-symbol))
               ("Editing" . (lnav-surround lnav-wrap-sexp lnav-wrap-round
                              lnav-wrap-curly lnav-wrap-square lnav-wrap-quote
                              lnav-delete-enclosing-pair
                              lnav-change-enclosing-pair
                              lnav-change-inner lnav-wrap-tag lnav-rewrap-tag))
               ("Structural" . (lnav-slurp-forward lnav-slurp-backward
                                 lnav-barf-forward lnav-barf-backward
                                 lnav-kill-sexp lnav-kill-hybrid-sexp
                                 lnav-delete-sexp lnav-backward-delete-sexp
                                 lnav-copy-sexp lnav-backward-copy-sexp
                                 lnav-clone-sexp lnav-transpose-sexp
                                 lnav-raise-sexp lnav-convolute-sexp
                                 lnav-swap-enclosing-sexp lnav-emit-sexp
                                 lnav-extract-before-sexp
                                 lnav-extract-after-sexp lnav-split-sexp
                                 lnav-join-sexp lnav-absorb-sexp
                                 lnav-splice-sexp-killing-backward
                                 lnav-splice-sexp-killing-forward
                                 lnav-splice-sexp-killing-around
                                 lnav-add-to-previous-sexp
                                 lnav-add-to-next-sexp
                                 lnav-select-next-thing
                                 lnav-select-next-thing-exchange))
               ("Selection" . (lnav-select-chunk lnav-select-chunk-around
                                lnav-narrow-to-sexp))
               ("Flash" . (lnav-flash-chunk lnav-flash-char lnav-flash-word
                            lnav-flash-search lnav-flash-fuzzy
                            lnav-flash-treesit))))
      (princ (concat (car group) "\n" (make-string (length (car group)) ?-) "\n"))
      (dolist (cmd (cdr group))
        (when (fboundp cmd)
          (princ (format "%-40s %s\n" cmd (documentation cmd))))
        )
      (princ "\n"))
    (princ "Modes: lnav-mode, global-lnav-mode, lnav-typing-mode,\n")
    (princ "       lnav-strict-mode, lnav-show-pair-mode\n")))

(provide 'lnav-extras)

;;; lnav-extras.el ends here
