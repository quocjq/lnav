;;; lnav-flash.el --- flash-style label jumping for lnav -*- lexical-binding: t; -*-
;;
;; Label navigation modeled on flash.nvim.  A label is shown at each
;; candidate position; press it to jump.  Two-key labels kick in when
;; matches overflow the label alphabet.  Sources: chunks, characters,
;; words, and search (regex or fuzzy).  Multi-window aware.

;;; Code:

(require 'cl-lib)

(declare-function lnav--parse-chunk-tree "lnav" ())
(declare-function lnav--chunk-before-open "lnav" (chunk))
(declare-function lnav--chunk-after-open "lnav" (chunk))
(declare-function lnav--chunk-children "lnav" (chunk))
(declare-function treesit-node-at "treesit" (pos &optional named node-type))
(declare-function treesit-node-start "treesit" (node))
(declare-function treesit-node-end "treesit" (node))
(declare-function treesit-node-parent "treesit" (node &optional named))

(defgroup lnav-flash nil
  "Flash-style label jumping."
  :group 'lnav
  :prefix "lnav-flash")

(defcustom lnav-flash-labels "asdfghjklqwertyuiopzxcvbnm"
  "Alphabet used to label flash matches."
  :type 'string
  :group 'lnav-flash)

(defcustom lnav-flash-autojump nil
  "Jump automatically when only one match remains."
  :type 'boolean
  :group 'lnav-flash)

(defcustom lnav-flash-multi-window nil
  "Flash across all visible windows of the frame."
  :type 'boolean
  :group 'lnav-flash)

(defcustom lnav-flash-show-backdrop nil
  "Dim non-match regions with a backdrop overlay."
  :type 'boolean
  :group 'lnav-flash)

(defface lnav-flash-label-face
  '((t (:background "yellow" :foreground "black" :weight bold)))
  "Face for flash labels.")

(defface lnav-flash-match-face
  '((t (:inherit lazy-highlight)))
  "Face for highlighted flash matches.")

(defface lnav-flash-backdrop-face
  '((t (:inherit region :extend t)))
  "Face dimming non-match regions.")

(defun lnav--flash-windows ()
  "Windows to flash over."
  (if lnav-flash-multi-window
      (window-list (selected-frame) nil)
    (list (selected-window))))

(defun lnav--flash-in-window-p (win)
  "Non-nil if WIN is usable for flashing."
  (and (window-live-p win)
       (not (window-minibuffer-p win))
       (buffer-live-p (window-buffer win))))

;;; Match collection

(defun lnav--flash-chunk-matches ()
  "Chunk opening positions as flash matches."
  (let (matches)
    (cl-labels ((walk (node)
                  (push (list :beg (lnav--chunk-before-open node)
                              :end (lnav--chunk-after-open node))
                        matches)
                  (dolist (child (lnav--chunk-children node))
                    (walk child))))
      (dolist (root (lnav--parse-chunk-tree))
        (walk root)))
    (nreverse matches)))

(defun lnav--flash-char-matches (char &optional from)
  "Occurrences of CHAR from FROM as flash matches."
  (let ((from (or from (point))) matches)
    (save-excursion
      (goto-char from)
      (while (re-search-forward (regexp-quote (char-to-string char)) nil t)
        (push (list :beg (match-beginning 0) :end (match-end 0)) matches)))
    (nreverse matches)))

(defun lnav--flash-word-matches (&optional from)
  "Word starts from FROM as flash matches."
  (let ((from (or from (point))) matches)
    (save-excursion
      (goto-char from)
      (while (re-search-forward "\\<\\w" nil t)
        (push (list :beg (match-beginning 0) :end (match-end 0)) matches)))
    (nreverse matches)))

(defun lnav--flash-fuzzy-p (query word)
  "Non-nil if QUERY chars appear in order in WORD."
  (let ((w (append (string-to-list word) nil))
        (q (append (string-to-list query) nil)))
    (while (and q w)
      (when (eq (car q) (car w))
        (setq q (cdr q)))
      (setq w (cdr w)))
    (null q)))

(defun lnav--flash-search-matches (query &optional fuzzy)
  "Matches for QUERY as flash matches.  FUZZY matches subsequences."
  (let (matches)
    (save-excursion
      (goto-char (point-min))
      (if fuzzy
          (while (re-search-forward "\\w+" nil t)
            (when (lnav--flash-fuzzy-p query (match-string 0))
              (push (list :beg (match-beginning 0) :end (match-end 0)) matches)))
        (condition-case nil
            (while (re-search-forward query nil t)
              (push (list :beg (match-beginning 0) :end (match-end 0)) matches))
          (invalid-regexp (user-error "Invalid regexp: %s" query)))))
    (nreverse matches)))

(defun lnav--flash-collect (fn &rest args)
  "Collect flash matches by calling FN with ARGS in each window.
FN returns matches with :beg/:end relative to that window's buffer."
  (let (all)
    (dolist (win (lnav--flash-windows))
      (when (lnav--flash-in-window-p win)
        (with-selected-window win
          (dolist (m (apply fn args))
            (push (cons :win (cons win m)) all)))))
    (nreverse all)))

(defun lnav--flash-sort-matches (matches)
  "Sort MATCHES nearest-first within each window."
  (let (grouped)
    (dolist (win (lnav--flash-windows))
      (let ((g (cl-remove-if-not (lambda (m) (eq (plist-get m :win) win)) matches))
            (pt (with-selected-window win (point))))
        (setq grouped
              (nconc grouped
                     (sort g (lambda (a b)
                               (< (abs (- (plist-get a :beg) pt))
                                  (abs (- (plist-get b :beg) pt)))))))))
    grouped))

;;; Label assignment

(defun lnav--flash-assign-labels (matches)
  "Assign label chars to MATCHES.
One char per match up to the alphabet length; two chars beyond."
  (let* ((n (length matches))
         (alen (max 1 (length lnav-flash-labels)))
         (two (> n alen)))
    (cl-loop for i from 0 below n
             for m in matches
             for l1 = (aref lnav-flash-labels
                            (mod (if two (floor i alen) i) alen))
             collect (plist-put
                      (plist-put m :l1 l1)
                      :l2 (and two (aref lnav-flash-labels (mod i alen)))))))

(defun lnav--flash-label-text (m)
  "Label text for match M."
  (if (plist-get m :l2)
      (format "%c%c" (plist-get m :l1) (plist-get m :l2))
    (char-to-string (plist-get m :l1))))

;;; Overlay display

(defun lnav--flash-show (matches &optional highlight backdrop)
  "Display label overlays for MATCHES.  Returns overlay list."
  (let (ovs)
    (when backdrop
      (dolist (win (lnav--flash-windows))
        (let ((ov (make-overlay (point-min) (point-max) (window-buffer win))))
          (overlay-put ov 'lnav-flash t)
          (overlay-put ov 'face 'lnav-flash-backdrop-face)
          (overlay-put ov 'priority -100)
          (push ov ovs))))
    (dolist (m matches)
      (let ((ov (make-overlay (plist-get m :beg) (plist-get m :end)
                              (window-buffer (plist-get m :win)))))
        (overlay-put ov 'lnav-flash t)
        (overlay-put ov 'before-string
                     (propertize (lnav--flash-label-text m)
                                 'face 'lnav-flash-label-face))
        (overlay-put ov 'priority 100)
        (when highlight
          (overlay-put ov 'face 'lnav-flash-match-face))
        (push ov ovs)))
    (nreverse ovs)))

(defun lnav--flash-cleanup (overlays)
  "Delete OVERLAYS."
  (dolist (ov overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov))))

;;; Selection loop

(defun lnav--flash-jump (match)
  "Jump to MATCH, switching to its window."
  (when match
    (select-window (plist-get match :win))
    (goto-char (plist-get match :beg))))

(defun lnav--flash-run (matches)
  "Run the flash selection loop over MATCHES.
Returns the selected match plist, or nil if cancelled."
  (let ((matches (lnav--flash-sort-matches matches))
        (selected nil)
        (cancelled nil)
        (pending nil)
        overlays)
    (unless matches
      (user-error "No matches"))
    (when (and lnav-flash-autojump (= (length matches) 1))
      (setq selected (car matches)))
    (unwind-protect
        (condition-case nil
            (progn
              (setq overlays (lnav--flash-show matches t lnav-flash-show-backdrop))
              (while (and (null selected) (not cancelled))
                (let ((key (read-key
                            (format "lnav-flash (%d)%s: " (length matches)
                                    (if pending (format " [%c]" pending) "")))))
                  (cond
                   ((memq key '(?\C-g ?\e)) (setq cancelled t))
                   ((eq key ?\C-m) (setq selected (car matches)))
                   ((eq key ?\C-h) (setq pending nil))
                   (pending
                    (if-let ((m (cl-find-if
                                 (lambda (x) (= (plist-get x :l2) key))
                                 matches)))
                        (setq selected m)
                      (setq pending nil)))
                   ((and (characterp key) (not (eq key 0)))
                    (if-let ((m (cl-find-if
                                 (lambda (x) (= (plist-get x :l1) key))
                                 matches)))
                        (if (plist-get m :l2)
                            (setq pending key)
                          (setq selected m))))))))
          (quit (setq cancelled t)))
      (lnav--flash-cleanup overlays))
    (if cancelled nil selected)))

;;; Commands

;;;###autoload
(defun lnav-flash-chunk ()
  "Flash-jump to a chunk.  Labels appear on every chunk's opening
delimiter; press one (two keys when crowded) to jump."
  (interactive)
  (lnav--flash-jump
   (lnav--flash-run (lnav--flash-collect #'lnav--flash-chunk-matches))))

;;;###autoload
(defun lnav-flash-char ()
  "Flash-jump to occurrences of a character."
  (interactive)
  (let ((char (read-char "Flash to char: ")))
    (lnav--flash-jump
     (lnav--flash-run (lnav--flash-collect #'lnav--flash-char-matches char)))))

;;;###autoload
(defun lnav-flash-word ()
  "Flash-jump to word starts."
  (interactive)
  (lnav--flash-jump
   (lnav--flash-run (lnav--flash-collect #'lnav--flash-word-matches))))

;;;###autoload
(defun lnav-flash-search (query)
  "Flash-jump to matches of QUERY (a regexp)."
  (interactive "sFlash search: ")
  (lnav--flash-jump
   (lnav--flash-run (lnav--flash-collect #'lnav--flash-search-matches query))))

;;;###autoload
(defun lnav-flash-fuzzy (query)
  "Flash-jump to words matching QUERY as a fuzzy subsequence."
  (interactive "sFlash fuzzy: ")
  (lnav--flash-jump
   (lnav--flash-run (lnav--flash-collect #'lnav--flash-search-matches query t))))

;;; Treesitter flash

;;;###autoload
(defun lnav-flash-treesit ()
  "Flash over the treesit ancestor nodes of the node at point."
  (interactive)
  (unless (and (fboundp 'treesit-node-at) (treesit-available-p))
    (user-error "Treesitter not available"))
  (unless (treesit-parser-list)
    (user-error "No treesitter parser for %s" major-mode))
  (let ((node (treesit-node-at (point)))
        (matches nil)
        (count 0))
    (while (and node (< count 30))
      (let ((beg (treesit-node-start node))
            (end (treesit-node-end node)))
        (when (and beg end (> end beg))
          (push (list :beg beg :end end) matches)))
      (setq node (treesit-node-parent node))
      (setq count (1+ count)))
    (lnav--flash-jump (lnav--flash-run (nreverse matches)))))

(provide 'lnav-flash)

;;; lnav-flash.el ends here
