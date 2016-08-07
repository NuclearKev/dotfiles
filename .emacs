;; General Modifications
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(global-linum-mode t)
(column-number-mode t)
(line-number-mode -1)
(display-time-mode t)
(display-battery-mode t)
(fringe-mode 1)   ; Shrink fringes to 1 pixel
(set-face-attribute 'default nil :height 120)
(set-default-font "DejaVu Sans Mono")
(global-hl-line-mode t)
;; (set-face-background 'hl-line "#")      ; this wasn't working?
(ido-mode 1)
(setf ido-enable-flex-matching t)				;makes switches stuff easier
(setq-default tab-width 2)
(global-auto-revert-mode t)
(setq confirm-kill-emacs #'y-or-n-p)	;Asks if you wish to leave emacs
(setq org-src-fontify-natively t)	;syntax highlighting in org-mode source blocks
(setq browse-url-browser-function 'eww-browse-url)
(setq visible-bell 1)

;; For some reason I think this loads images faster
(setq imagemagick-enabled-types t)

(add-to-list 'load-path "/home/kdb/.emacs.d/elpa/emms-20160801.1349/")
(require 'emms-setup)
(emms-all)
(emms-default-players)

;; Removes trailing whitespace before saving.
(add-hook 'before-save-hook (lambda ()
															(delete-trailing-whitespace)))

;; Thou shall use 2 spaces indenting
(setq typescript-indent-level 2)

;; Enable a line at the 80 character column for certain modes
(setq fci-rule-column 80)

;; Enable some good modes when editing source files
(add-hook 'prog-mode-hook 'column-enforce-mode)
(add-hook 'prog-mode-hook 'nlinum-mode)
(add-hook 'prog-mode-hook 'auto-complete-mode)
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'paren-activate)

;; Lets not forget the text modes!
(add-hook 'text-mode-hook
					'(lambda () (fci-mode t)))
(add-hook 'latex-mode-hook
					'(lambda () (fci-mode t)))
(add-hook 'markdown-mode-hook
					'(lambda () (fci-mode t)))
(add-hook 'text-mode-hook
					'(lambda () (nlinum-mode t)))
(add-hook 'latex-mode-hook
					'(lambda () (nlinum-mode t)))
(add-hook 'markdown-mode-hook
					'(lambda () (nlinum-mode t)))
(add-hook 'latex-mode-hook
					'(lambda () (column-enforce-mode t)))
(add-hook 'latex-mode-hook
					'(lambda () (flycheck-mode t)))

;; Start flyspell for text, latex, erc, and org modes
(add-hook 'text-mode-hook
					'(lambda () (flyspell-mode t)))
(add-hook 'latex-mode-hook
					'(lambda () (flyspell-mode t)))
(add-hook 'erc-mode-hook
					'(lambda () (flyspell-mode t)))
(add-hook 'org-mode-hook
					'(lambda () (flyspell-mode t)))
(add-hook 'markdown-mode-hook
					'(lambda () (flyspell-mode t)))

;; Enable M-RET to add another comment line. This is mainly for typing long
;; explainations that take more than 1 line. For example, this comment...
;; Note: Since the other languages I use have things like /* */, they don't
;; need this feature.
(add-hook 'emacs-lisp-mode-hook
					'(lambda () (local-set-key (kbd "M-RET") 'comment-indent-new-line)))
(add-hook 'common-lisp-mode-hook
					'(lambda () (local-set-key (kbd "M-RET") 'comment-indent-new-line)))
(add-hook 'lisp-mode-hook
					'(lambda () (local-set-key (kbd "M-RET") 'comment-indent-new-line)))
(add-hook 'sh-mode-hook
					'(lambda () (local-set-key (kbd "M-RET") 'comment-indent-new-line)))
(add-hook 'R-mode-hook
					'(lambda () (local-set-key (kbd "M-RET") 'comment-indent-new-line)))

;; Make backtab go backwards for links, like any normal person would want it
(add-hook 'eww-mode-hook
					'(lambda () (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; Personal Keybindings
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-C c") 'comment-region)
(global-set-key (kbd "M-C C") 'capitalize-word)
(global-set-key (kbd "M-C u") 'uncomment-region)
(global-set-key (kbd "C-x &") 'calendar)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g M-f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g M-w") 'avy-goto-word-1)
(global-set-key (kbd "M-G G") 'magit-status)
;; (global-set-key (kbd "C-|") 'pop-global-mark) ;return to last cursor position
(global-set-key (kbd "C-x B") 'list-buffers)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; C-x M for Multicursor or Music (for emms controls)
;; Notice for the music controls, previous and next are upper case!
(global-set-key (kbd "C-x M d") 'mc/mark-all-dwim) ;d for dwim
(global-set-key (kbd "C-x M l") 'mc/mark-next-lines) ;l for lines
(global-set-key (kbd "C-x M a") 'mc/mark-all-like-this) ;a for all
(global-set-key (kbd "C-x M n") 'mc/mark-next-like-this) ;n for next
(global-set-key (kbd "C-x M p") 'emms-pause) ;p for pause
(global-set-key (kbd "C-x M P") 'emms-previous) ;P for previous
(global-set-key (kbd "C-x M N") 'emms-next) ;N for next
(global-set-key (kbd "C-x M s") 'emms-show) ;s for show
(global-set-key (kbd "C-x H a") 'helm-apropos)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
	(interactive "p")
	(delete-region (point) (progn (backward-word arg) (point))))

(defun forward-delete-word (arg)
  "Delete characters forward until encountering the beginning of a word.
With argument ARG, do this that many times."
	(interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(global-set-key (kbd "M-d") 'forward-delete-word)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)

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

(global-set-key (kbd "C-x K") 'kill-and-delete-window)

;;; Others
(setq ispell-program-name "/usr/bin/aspell")
(setq inferior-R-program-name "/usr/bin/R")
(setq erc-nick "NuclearKev")

;;; Set up to use melpa packages
;;; You may need to comment this out to get certain packages (like ace-window)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;;; Set Your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))
;;(slime-setup '(slime-company))
;;(global-company-mode)

;; Twittering-mode
(setq twittering-use-master-password t) ;allows me to automatically login to my twitter account
(setq twittering-icon-mode t)		;gimme pictures

;; eshell
(require 'em-tramp)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 300) ; for 5 minutes (time in secs)

;; jul-mode stuff
(add-to-list 'load-path "~/jul-mode")
(load "jul-mode.el")

;; Enable pdf-tools all day
(pdf-tools-install)

;; elscreen start
(elscreen-start)
(elscreen-set-prefix-key "\C-t")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes
	 (quote
		("366f94b5c9428b25dbc2ed7f80cd96314b7124acab404e30d201ebe9aac0ff9d" default)))
 '(eww-download-directory "~/packages")
 '(notmuch-init-file "~/.notmuch-config")
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#343333")))))
