;;; lnav-evil.el --- Evil text objects for lnav -*- lexical-binding: t; -*-
;;
;; Loaded by `lnav' once evil is present.  Defines the text
;; objects `il' (inside chunk) and `al' (around chunk), usable
;; with any operator: dil, cil, yal, ...

;;; Code:

(eval-when-compile
  (require 'evil nil t))

(declare-function lnav--chunk-at-point "lnav" (&optional pos))
(declare-function lnav--chunk-after-open "lnav" (chunk))
(declare-function lnav--chunk-before-open "lnav" (chunk))
(declare-function lnav--chunk-before-close "lnav" (chunk))
(declare-function lnav--chunk-after-close "lnav" (chunk))
(declare-function lnav-surround "lnav" (delimiter))
(declare-function lnav-delete-enclosing-pair "lnav" ())
(declare-function lnav-change-enclosing-pair "lnav" (delimiter))
(declare-function lnav-slurp-forward "lnav" ())
(declare-function lnav-slurp-backward "lnav" ())
(declare-function lnav-barf-forward "lnav" ())
(declare-function lnav-barf-backward "lnav" ())
(declare-function lnav-raise-sexp "lnav" ())
(declare-function lnav-transpose-sexp "lnav" ())
(declare-function lnav-kill-sexp "lnav" ())
(declare-function lnav-forward-sexp "lnav" ())
(declare-function lnav-backward-sexp "lnav" ())
(declare-function lnav-wrap-sexp "lnav" (delimiter))
(declare-function lnav-flash-chunk "lnav-flash" ())
(declare-function lnav-flash-char "lnav-flash" ())

(defun lnav--evil-chunk-range (inside)
  "Return (BEG END) for the chunk at point for evil text objects.
INSIDE non-nil excludes the delimiters."
  (let ((chunk (lnav--chunk-at-point)))
    (when chunk
      (let ((beg (if inside (lnav--chunk-after-open chunk)
                   (lnav--chunk-before-open chunk)))
            (end (if inside (lnav--chunk-before-close chunk)
                   (lnav--chunk-after-close chunk))))
        (when (and beg end)
          (list beg end))))))

(with-no-warnings
  ;;;###autoload
  (evil-define-text-object lnav-evil-inside-chunk (count &optional beg end type)
    (lnav--evil-chunk-range t))

  ;;;
  (evil-define-text-object lnav-evil-around-chunk (count &optional beg end type)
    (lnav--evil-chunk-range nil))

  (define-key evil-inner-text-objects-map "l" #'lnav-evil-inside-chunk)
  (define-key evil-outer-text-objects-map "l" #'lnav-evil-around-chunk))

;;;###autoload
(define-key evil-normal-state-map (kbd "gs") #'lnav-surround)
(define-key evil-visual-state-map (kbd "gs") #'lnav-surround)
;;;###autoload
(define-key evil-normal-state-map (kbd "gS") #'lnav-delete-enclosing-pair)
;;;###autoload
(define-key evil-normal-state-map (kbd "gC") #'lnav-change-enclosing-pair)
;;;###autoload
(define-key evil-normal-state-map (kbd "g(") #'lnav-slurp-backward)
;;;###autoload
(define-key evil-normal-state-map (kbd "g)") #'lnav-slurp-forward)
;;;###autoload
(define-key evil-normal-state-map (kbd "g{") #'lnav-barf-backward)
;;;###autoload
(define-key evil-normal-state-map (kbd "g}") #'lnav-barf-forward)
;;;###autoload
(define-key evil-normal-state-map (kbd "gt") #'lnav-transpose-sexp)
;;;###autoload
(define-key evil-normal-state-map (kbd "gK") #'lnav-kill-sexp)
;;;###autoload
(define-key evil-normal-state-map (kbd "gr") #'lnav-raise-sexp)
;;;###autoload
(define-key evil-normal-state-map (kbd "gw") #'lnav-wrap-sexp)
;;;###autoload
(define-key evil-normal-state-map (kbd "gf") #'lnav-forward-sexp)
;;;###autoload
(define-key evil-normal-state-map (kbd "gb") #'lnav-backward-sexp)
;;;###autoload
(define-key evil-normal-state-map (kbd "gz") #'lnav-flash-chunk)
;;;###autoload
(define-key evil-visual-state-map (kbd "gz") #'lnav-flash-chunk)
;;;###autoload
(define-key evil-normal-state-map (kbd "gZ") #'lnav-flash-char)

(provide 'lnav-evil)

;;; lnav-evil.el ends here
