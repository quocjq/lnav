;;; lnav-structural.el --- structural editing and navigation for lnav -*- lexical-binding: t; -*-
;;
;; The smartparens-style structural editing surface: hybrid kill,
;; delete/copy/clone variants, splice-killing, convolute, absorb,
;; emit, extract, split/join, swap, add-to-previous/next, and the
;; navigation family (parallel sexp, beginning/end-of-sexp, down/up).

;;; Code:

(require 'cl-lib)

(declare-function lnav--chunk-at-point "lnav" (&optional pos))
(declare-function lnav--chunk-required "lnav" ())
(declare-function lnav--chunk-open "lnav" (chunk))
(declare-function lnav--chunk-close "lnav" (chunk))
(declare-function lnav--chunk-before-open "lnav" (chunk))
(declare-function lnav--chunk-after-open "lnav" (chunk))
(declare-function lnav--chunk-before-close "lnav" (chunk))
(declare-function lnav--chunk-after-close "lnav" (chunk))
(declare-function lnav--chunk-parent "lnav" (chunk))
(declare-function lnav--chunk-children "lnav" (chunk))
(declare-function lnav--form-bounds-forward "lnav" (pos))
(declare-function lnav--form-bounds-backward "lnav" (pos))
(declare-function lnav--parse-chunk-tree "lnav" ())
(declare-function lnav-slurp-forward "lnav" ())
(declare-function lnav-delete-enclosing-pair "lnav" ())
(declare-function lnav-next-chunk "lnav" ())

;;; Hybrid sexp

;;;###autoload
(defun lnav-kill-hybrid-sexp ()
  "Kill the contents of the chunk at point up to but excluding its
closing delimiter."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (bclose (lnav--chunk-before-close chunk)))
    (or bclose (user-error "Chunk not closed"))
    (kill-region (lnav--chunk-before-open chunk) bclose)))

;;;###autoload
(defalias 'lnav-slurp-hybrid-sexp 'lnav-slurp-forward
  "Pull the next form into the chunk at point.")
;;;###autoload
(defalias 'lnav-push-hybrid-sexp 'lnav-slurp-forward
  "Push the last form out of the chunk at point.")

;;; Kill / delete / copy / clone

;;;###autoload
(defun lnav-delete-sexp ()
  "Delete the next form after point."
  (interactive "*")
  (let ((f (lnav--form-bounds-forward (point))))
    (or f (user-error "No next form"))
    (delete-region (car f) (cdr f))))

;;;###autoload
(defun lnav-backward-delete-sexp ()
  "Delete the previous form before point."
  (interactive "*")
  (let ((f (lnav--form-bounds-backward (point))))
    (or f (user-error "No previous form"))
    (delete-region (car f) (cdr f))))

;;;###autoload
(defun lnav-copy-sexp ()
  "Copy the next form after point to the kill ring."
  (interactive)
  (let ((f (lnav--form-bounds-forward (point))))
    (or f (user-error "No next form"))
    (kill-ring-save (car f) (cdr f))))

;;;###autoload
(defun lnav-backward-copy-sexp ()
  "Copy the previous form before point to the kill ring."
  (interactive)
  (let ((f (lnav--form-bounds-backward (point))))
    (or f (user-error "No previous form"))
    (kill-ring-save (car f) (cdr f))))

;;;###autoload
(defun lnav-clone-sexp ()
  "Copy the chunk at point and insert the copy after it."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (cur-end (lnav--chunk-after-close chunk)))
    (let ((copy (buffer-substring (lnav--chunk-before-open chunk)
                                  cur-end)))
      (goto-char cur-end)
      (insert " " copy))))

;;; Splice-killing

(defun lnav--splice-and (kill-fn)
  "Splice the chunk at point, then run KILL-FN after the splice."
  (let* ((chunk (lnav--chunk-required))
         (open (lnav--chunk-open chunk))
         (close (lnav--chunk-close chunk))
         (bopen (lnav--chunk-before-open chunk))
         (bclose (lnav--chunk-before-close chunk)))
    (or bclose (user-error "Chunk not closed"))
    (delete-region bclose (+ bclose (length close)))
    (delete-region bopen (+ bopen (length open)))
    (goto-char bopen)
    (funcall kill-fn)))

;;;###autoload
(defun lnav-splice-sexp-killing-backward ()
  "Splice the chunk at point and kill the previous form."
  (interactive "*")
  (lnav--splice-and
   (lambda ()
     (let ((p (lnav--form-bounds-backward (point))))
       (when p (kill-region (car p) (cdr p)))))))

;;;###autoload
(defun lnav-splice-sexp-killing-forward ()
  "Splice the chunk at point and kill the next form."
  (interactive "*")
  (lnav--splice-and
   (lambda ()
     (let ((n (lnav--form-bounds-forward (point))))
       (when n (kill-region (car n) (cdr n)))))))

;;;###autoload
(defun lnav-splice-sexp-killing-around ()
  "Splice the chunk at point and kill the previous and next forms."
  (interactive "*")
  (lnav--splice-and
   (lambda ()
     (let ((p (lnav--form-bounds-backward (point))))
       (when p (kill-region (car p) (cdr p))))
     (let ((n (lnav--form-bounds-forward (point))))
       (when n (kill-region (car n) (cdr n)))))))

;;; Convolute / swap / emit / extract

;;;###autoload
(defun lnav-convolute-sexp ()
  "Move the chunk at point into the place of its parent chunk.
`(a (b c) d)' with point in `(b c)' -> `((a) b c d)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk)))
    (or parent (user-error "No parent chunk"))
    (let* ((po (lnav--chunk-before-open parent))
           (pc (lnav--chunk-after-close parent))
           (co (lnav--chunk-before-open chunk))
           (cc (lnav--chunk-after-close chunk))
           (popen (lnav--chunk-open parent))
           (pclose (lnav--chunk-close parent))
           (copen (lnav--chunk-open chunk))
           (cclose (lnav--chunk-close chunk))
           (pre (string-trim-right
                 (buffer-substring (+ po (length popen)) co)))
           (content (string-trim
                     (buffer-substring (+ co (length copen))
                                       (lnav--chunk-before-close chunk))))
           (post (string-trim-left
                  (buffer-substring cc (lnav--chunk-before-close parent))))
           (new (concat copen popen pre pclose " " content " " post cclose)))
      (delete-region po pc)
      (goto-char po)
      (insert new))))

;;;###autoload
(defun lnav-swap-enclosing-sexp ()
  "Swap the chunk at point with its parent chunk.
`(a (b) c)' -> `((b) a c)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk)))
    (or parent (user-error "No parent chunk"))
    (let* ((popen (lnav--chunk-open parent))
           (pclose (lnav--chunk-close parent))
           (c-text (buffer-substring (lnav--chunk-before-open chunk)
                                     (lnav--chunk-after-close chunk)))
           (pre (string-trim
                 (buffer-substring (+ (lnav--chunk-before-open parent)
                                      (length popen))
                                   (lnav--chunk-before-open chunk))))
           (post (string-trim
                  (buffer-substring (lnav--chunk-after-close chunk)
                                    (lnav--chunk-before-close parent))))
           (new (concat popen c-text " " pre " " post pclose)))
      (delete-region (lnav--chunk-before-open parent)
                     (lnav--chunk-after-close parent))
      (goto-char (lnav--chunk-before-open parent))
      (insert new))))

;;;###autoload
(defun lnav-emit-sexp ()
  "Move the chunk at point to the end of its parent chunk.
`(a (b) c)' with point in `(b)' -> `(a c (b))'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk)))
    (or parent (user-error "No parent chunk"))
    (let* ((bopen (lnav--chunk-before-open chunk))
           (aclose (lnav--chunk-after-close chunk))
           (text (buffer-substring bopen aclose))
           (del-end (if (= (char-after aclose) ?\s) (1+ aclose) aclose))
           (del-len (- del-end bopen))
           (parent-bclose (lnav--chunk-before-close parent)))
      (delete-region bopen del-end)
      (goto-char (- parent-bclose del-len))
      (insert " " text))))

;;;###autoload
(defun lnav-extract-after-sexp ()
  "Move the chunk at point to just after its parent chunk.
`(a (b) c)' with point in `(b)' -> `(a c) (b)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk)))
    (or parent (user-error "No parent chunk"))
    (let* ((bopen (lnav--chunk-before-open chunk))
           (aclose (lnav--chunk-after-close chunk))
           (text (buffer-substring bopen aclose))
           (del-end (if (= (char-after aclose) ?\s) (1+ aclose) aclose))
           (del-len (- del-end bopen))
           (parent-end (lnav--chunk-after-close parent)))
      (delete-region bopen del-end)
      (goto-char (- parent-end del-len))
      (insert " " text))))

;;;###autoload
(defun lnav-extract-before-sexp ()
  "Move the chunk at point to just before its parent chunk.
`(a (b) c)' with point in `(b)' -> `(b) (a c)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk)))
    (or parent (user-error "No parent chunk"))
    (let* ((bopen (lnav--chunk-before-open chunk))
           (aclose (lnav--chunk-after-close chunk))
           (text (buffer-substring bopen aclose))
           (del-beg (if (= (char-before bopen) ?\s) (1- bopen) bopen))
           (parent-beg (lnav--chunk-before-open parent)))
      (delete-region del-beg aclose)
      (goto-char parent-beg)
      (insert text " "))))

;;; Split / join / absorb

;;;###autoload
(defun lnav-split-sexp ()
  "Split the chunk at point into two chunks at point.
`(a b)' with point between a and b -> `(a)(b)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (close (lnav--chunk-close chunk))
         (open (lnav--chunk-open chunk)))
    (insert close " " open)))

;;;###autoload
(defun lnav-join-sexp ()
  "Join the next chunk into the chunk at point.
`(a) (b)' -> `(a b)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (close (lnav--chunk-close chunk))
         (bclose (lnav--chunk-before-close chunk))
         (next (lnav--form-bounds-forward (lnav--chunk-after-close chunk))))
    (or bclose next (user-error "Nothing to join"))
    (let* ((nbeg (car next))
           (nend (cdr next))
           (nxt (lnav--chunk-at-point (1+ nbeg)))
           (inner (if (and nxt (= (lnav--chunk-before-open nxt) nbeg)
                           (= (lnav--chunk-after-close nxt) nend))
                      (buffer-substring (+ nbeg (length (lnav--chunk-open nxt)))
                                        (- nend (length (lnav--chunk-close nxt))))
                    (buffer-substring nbeg nend)))
           (del-beg (if (= (char-before nbeg) ?\s) (1- nbeg) nbeg)))
      (delete-region del-beg nend)
      (goto-char bclose)
      (delete-region bclose (+ bclose (length close)))
      (insert " " inner close))))

;;;###autoload
(defun lnav-absorb-sexp ()
  "Absorb the next form into the chunk at point."
  (interactive "*")
  (lnav-slurp-forward))

;;; Add to previous / next

;;;###autoload
(defun lnav-add-to-previous-sexp ()
  "Add the chunk at point to the previous chunk.
`(a) (b)' with point in `(a)' -> `(a (b))'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (bclose (lnav--chunk-before-close chunk))
         (close (lnav--chunk-close chunk))
         (next (lnav--form-bounds-forward (lnav--chunk-after-close chunk))))
    (or bclose next (user-error "Nothing to add"))
    (let* ((nbeg (car next))
           (nend (cdr next))
           (nxt (lnav--chunk-at-point (1+ nbeg)))
           (text (buffer-substring nbeg nend))
           (del-beg (if (= (char-before nbeg) ?\s) (1- nbeg) nbeg)))
      (unless (and nxt (= (lnav--chunk-before-open nxt) nbeg))
        (user-error "Next sexp is not a chunk"))
      (delete-region del-beg nend)
      (goto-char bclose)
      (delete-region bclose (+ bclose (length close)))
      (insert " " text close))))

;;;###autoload
(defun lnav-add-to-next-sexp ()
  "Add the chunk at point to the next chunk.
`(a) (b)' with point in `(a)' -> `((a) b)'."
  (interactive "*")
  (let* ((chunk (lnav--chunk-required))
         (cur-beg (lnav--chunk-before-open chunk))
         (cur-end (lnav--chunk-after-close chunk))
         (next (lnav--form-bounds-forward cur-end)))
    (or next (user-error "No next sexp"))
    (let* ((nbeg (car next))
           (nend (cdr next))
           (nxt (lnav--chunk-at-point (1+ nbeg)))
           (text (buffer-substring cur-beg cur-end)))
      (unless (and nxt (= (lnav--chunk-before-open nxt) nbeg))
        (user-error "Next sexp is not a chunk"))
      (let ((inner (buffer-substring (+ nbeg (length (lnav--chunk-open nxt)))
                                     (- nend (length (lnav--chunk-close nxt))))))
        (delete-region cur-beg nend)
        (goto-char cur-beg)
        (insert (lnav--chunk-open nxt) text " " inner
                (lnav--chunk-close nxt))))))

;;; Change inner

;;;###autoload
(defun lnav-change-inner (replacement)
  "Replace the contents of the chunk at point with REPLACEMENT."
  (interactive "sReplacement: ")
  (let* ((chunk (lnav--chunk-required))
         (bclose (lnav--chunk-before-close chunk)))
    (or bclose (user-error "Chunk not closed"))
    (delete-region (lnav--chunk-after-open chunk) bclose)
    (insert replacement)))

;;; Incremental selection

(defvar-local lnav--select-region nil
  "Last selected region (BEG . END) for `lnav-select-next-thing'.")

;;;###autoload
(defun lnav-select-next-thing ()
  "Select the chunk at point; repeat to expand to its parent."
  (interactive)
  (let* ((chunk (lnav--chunk-required))
         (cur (cons (lnav--chunk-before-open chunk)
                    (lnav--chunk-after-close chunk)))
         (target (if (equal cur lnav--select-region)
                     (let ((parent (lnav--chunk-parent chunk)))
                       (or parent (user-error "At outermost sexp"))
                       (cons (lnav--chunk-before-open parent)
                             (lnav--chunk-after-close parent)))
                   cur)))
    (setq lnav--select-region target)
    (push-mark (cdr target) t t)
    (goto-char (car target))))

;;;###autoload
(defun lnav-select-next-thing-exchange ()
  "Like `lnav-select-next-thing', but exchange point and mark."
  (interactive)
  (lnav-select-next-thing)
  (let ((p (point)))
    (goto-char (mark))
    (set-mark p)))

;;;###autoload
(defun lnav-select-previous-thing ()
  "Select the chunk at point, contracting to a previous selection.
Shrinks the selection to the parent's previous sibling region."
  (interactive)
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk)))
    (if parent
        (progn
          (push-mark (lnav--chunk-after-close chunk) t t)
          (goto-char (lnav--chunk-before-open chunk)))
      (user-error "At outermost sexp"))))

;;;###autoload
(defalias 'lnav-select-previous-thing-exchange 'lnav-select-next-thing-exchange
  "Exchange point and mark for `lnav-select-previous-thing'.")

;;; Navigation

;;;###autoload
(defun lnav-next-sexp ()
  "Move to the start of the next form after the chunk at point."
  (interactive "^")
  (let* ((chunk (lnav--chunk-at-point))
         (start (if chunk (lnav--chunk-after-close chunk)
                  (let ((f (lnav--form-bounds-forward (point))))
                    (if f (cdr f) (point)))))
         (f (lnav--form-bounds-forward start)))
    (or f (user-error "No next sexp"))
    (goto-char (car f))))

;;;###autoload
(defun lnav-previous-sexp ()
  "Move to the start of the previous form before the chunk at point."
  (interactive "^")
  (let* ((chunk (lnav--chunk-at-point))
         (start (if chunk (lnav--chunk-before-open chunk) (point)))
         (f (lnav--form-bounds-backward start)))
    (or f (user-error "No previous sexp"))
    (goto-char (car f))))

;;;###autoload
(defun lnav-forward-parallel-sexp ()
  "Move to the next chunk at the same nesting depth."
  (interactive "^")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk))
         (siblings (if parent (lnav--chunk-children parent)
                     (lnav--parse-chunk-tree)))
         (target (cl-find-if (lambda (c)
                               (> (lnav--chunk-before-open c) (point)))
                             siblings)))
    (or target (user-error "No next sibling"))
    (goto-char (lnav--chunk-before-open target))))

;;;###autoload
(defun lnav-backward-parallel-sexp ()
  "Move to the previous chunk at the same nesting depth."
  (interactive "^")
  (let* ((chunk (lnav--chunk-required))
         (parent (lnav--chunk-parent chunk))
         (siblings (if parent (lnav--chunk-children parent)
                     (lnav--parse-chunk-tree)))
         (target nil))
    (dolist (s siblings)
      (when (< (lnav--chunk-before-open s) (point))
        (setq target s)))
    (or target (user-error "No previous sibling"))
    (goto-char (lnav--chunk-before-open target))))

;;;###autoload
(defun lnav-backward-up-sexp ()
  "Move to the start of the parent chunk of the chunk at point."
  (interactive "^")
  (let ((parent (lnav--chunk-parent (lnav--chunk-required))))
    (or parent (user-error "No enclosing sexp"))
    (goto-char (lnav--chunk-before-open parent))))

;;;###autoload
(defun lnav-beginning-of-sexp ()
  "Move to the start of the chunk at point."
  (interactive "^")
  (goto-char (lnav--chunk-before-open (lnav--chunk-required))))

;;;###autoload
(defun lnav-end-of-sexp ()
  "Move to the end of the chunk at point."
  (interactive "^")
  (goto-char (lnav--chunk-after-close (lnav--chunk-required))))

;;;###autoload
(defun lnav-beginning-of-next-sexp ()
  "Move to the start of the next form after the chunk at point."
  (interactive "^")
  (let ((next (lnav--form-bounds-forward
               (lnav--chunk-after-close (lnav--chunk-required)))))
    (or next (user-error "No next sexp"))
    (goto-char (car next))))

;;;###autoload
(defun lnav-end-of-next-sexp ()
  "Move to the end of the next form after the chunk at point."
  (interactive "^")
  (let ((next (lnav--form-bounds-forward
               (lnav--chunk-after-close (lnav--chunk-required)))))
    (or next (user-error "No next sexp"))
    (goto-char (cdr next))))

;;;###autoload
(defun lnav-beginning-of-previous-sexp ()
  "Move to the start of the previous form before point."
  (interactive "^")
  (let ((prev (lnav--form-bounds-backward (point))))
    (or prev (user-error "No previous sexp"))
    (goto-char (car prev))))

;;;###autoload
(defun lnav-end-of-previous-sexp ()
  "Move to the end of the previous form before point."
  (interactive "^")
  (let ((prev (lnav--form-bounds-backward (point))))
    (or prev (user-error "No previous sexp"))
    (goto-char (cdr prev))))

;;;###autoload
(defun lnav-down-sexp ()
  "Move to the end of the last child chunk of the chunk at point."
  (interactive "^")
  (let* ((chunk (lnav--chunk-required))
         (kids (lnav--chunk-children chunk)))
    (or kids (user-error "No child sexp"))
    (goto-char (lnav--chunk-after-close (car (last kids))))))

;;;###autoload
(defun lnav-backward-down-sexp ()
  "Move to the start of the last child chunk of the chunk at point."
  (interactive "^")
  (let* ((chunk (lnav--chunk-required))
         (kids (lnav--chunk-children chunk)))
    (or kids (user-error "No child sexp"))
    (goto-char (lnav--chunk-before-open (car (last kids))))))

;;;###autoload
(defun lnav-forward-symbol ()
  "Move to the start of the next word."
  (interactive "^")
  (when (looking-at "\\w")
    (forward-word 1))
  (if (re-search-forward "\\w" nil t)
      (goto-char (match-beginning 0))
    (user-error "No next symbol")))

;;;###autoload
(defun lnav-backward-symbol ()
  "Move to the previous word."
  (interactive "^")
  (or (re-search-backward "\\w" nil t)
      (user-error "No previous symbol")))

;;;###autoload
(defun lnav-narrow-to-sexp ()
  "Narrow to the chunk at point."
  (interactive)
  (let* ((chunk (lnav--chunk-required))
         (bclose (lnav--chunk-before-close chunk)))
    (or bclose (user-error "Chunk not closed"))
    (narrow-to-region (lnav--chunk-before-open chunk)
                      (lnav--chunk-after-close chunk))))

(provide 'lnav-structural)

;;; lnav-structural.el ends here
