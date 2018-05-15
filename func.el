;;; func --- useful functions
;;; Commentary:
;;; Useful editing functions

;;; Code:

;; Comment line. Only useful on Emacs version < 25
(defun comment-line ()
  "Comment or uncomment the current line.  Your cursor doesn't move."
  (interactive)
  (comment-or-uncomment-region (point-at-bol) (point-at-eol)))
(global-set-key (kbd "C-x C-;") 'comment-line)

(defun backward-delete-word-no-kill-ring (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
	(interactive "p")
	(delete-region (point) (progn (backward-word arg) (point))))

(defun forward-delete-word-no-kill-ring (arg)
  "Delete characters forward until encountering the beginning of a word.
With argument ARG, do this that many times."
	(interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(defun kill-and-delete-window ()
	(interactive)
	(kill-buffer)
	(if (equal 1 (length (window-list)))
			nil
		(delete-window)))


(defun beginning-of-line-edit ()
  "Move to the beginnging of the line and switch to edit mode."
  (interactive)
  (move-beginning-of-line nil)
  (modalka-mode -1))

(defun end-of-line-edit ()
  "Move to the end of the line and switch to edit mode."
  (interactive)
  (move-end-of-line nil)
  (modalka-mode -1))

(defun forward-word-edit (&optional n)
  "N is how many words you wish to jump.
Jump the word and switch to edit mode."
  (interactive "p\n")
  (forward-word n)
  (modalka-mode -1))

(defun backward-word-edit (&optional n)
  "N is how many words you wish to jump.
Jump the word backwards and switch to edit mode."
  (interactive "p\n")
  (backward-word n)
  (modalka-mode -1))

(defun replace-char ()
  "Replace the character underneath the cursor."
  (interactive)
  (let ((c (read-key)))
    (delete-char 1)
    (insert c)))

(defun iy-go-to-char-correctly (n char)
  "N is the number of occurances, CHAR is the character you wish to goto.
This function fixes a flaw that the original has with not landing directly on the char you pick."
  (interactive "p\ncGo to char: ")
  (iy-go-to-char n char)
  (backward-char))

(defun delete-forward-word-edit (&optional n)
  "N is how many words you wish to delete.
Delete the word forwards and switch to edit mode."
  (interactive "p\n")
  (forward-delete-word-no-kill-ring n)
  (modalka-mode -1))

(defun delete-backward-word-edit (&optional n)
  "N is how many words you wish to delete.
Delete the word backwards and switch to edit mode."
  (interactive "p\n")
  (backward-delete-word-no-kill-ring n)
  (modalka-mode -1))

(provide 'func)
;;; func.el ends here
