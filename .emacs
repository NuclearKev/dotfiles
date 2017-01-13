;;; .emacs --- Emacs customizations

;;; Commentary:
;;
;; My Emacs customizations.  Is there more to say?
;;
;; Packges:
;;
;; ac-emoji
;; ace-window
;; ample-theme
;; auto-complete
;; column-enforce-mode
;; emms
;; emms-player
;; emojify
;; fill-column-indicator
;; flycheck
;; helm
;; highlight-parentheses
;; image+
;; magit
;; markdown-mode
;; mic-paren
;; multiple-cursors
;; nlinum
;; org-plus
;; pdf-tools
;; twittering-mode

;;; Code:

;; General Modifications
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(global-linum-mode t)
(column-number-mode t)
(line-number-mode -1)
(set-frame-font "DejaVu Sans Mono")
(display-time-mode t)
;; (display-battery-mode t)
(fringe-mode 1)   ; Shrink fringes to 1 pixel
;; (set-face-attribute 'default nil :height 120)
(global-hl-line-mode t)
(set-face-background 'hl-line "#3C3C3C")      ; this wasn't working?
(ido-mode 1)
(setf ido-enable-flex-matching t)				;makes switches stuff easier
(setq-default tab-width 2)
(global-auto-revert-mode t)
(setq confirm-kill-emacs #'y-or-n-p)	;Asks if you wish to leave emacs
(setq org-src-fontify-natively t)	;syntax highlighting in org-mode source blocks
(setq browse-url-browser-function 'eww-browse-url)
(setq visible-bell 1)
(setq fill-column 80)

(add-hook 'after-init-hook #'global-emojify-mode) ;gimme emojis EVERYWHERE! 🖕

(setq backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
	 '(("." . "~/.backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; MEPLA setup
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
	(add-to-list
	 'package-archives
	 '("org" . "http://orgmode.org/elpa/")
	 t)
  (package-initialize))

;; For some reason I think this loads images faster
(setq imagemagick-enabled-types t)

;; EMMS setup
(add-to-list 'load-path "/home/kdb/.emacs.d/elpa/emms-*.*/")
(require 'emms-setup)
(emms-minimalistic)											;I like things minimal
;; (emms-default-players)
;; I want to use mpv!
(require 'emms-player-mpv)
(add-to-list 'emms-player-list 'emms-player-mpv)
;; Show me info in the mode-line
(require 'emms-mode-line)
;; (emms-mode-line 1)
;; Show me the playing time
(require 'emms-playing-time)
(emms-playing-time 1)
;; (setq emms-mode-line-format "")					;Don't show me the file name
(setq emms-playing-time-style (quote time))	;make it show the time

;; Removes trailing whitespace before saving.
(add-hook 'before-save-hook (lambda ()
			      (delete-trailing-whitespace)))

;; Thou shall use 2 spaces indenting
;; (setq typescript-indent-level 2)

;; Enable a line at the 80 character column for certain modes
(setq fci-rule-column 80)

;; Enable some good modes when editing source files
(add-hook 'prog-mode-hook
					(lambda ()
						(column-enforce-mode 1)
						(nlinum-mode 1)
						(auto-complete-mode 1)
						(fci-mode 1)
						(flycheck-mode 1)
						(paren-activate)
						(auto-fill-mode 1)))

(add-hook 'image-mode-hook 'imagex-sticky-mode)

;; Lets not forget the text modes!
(add-hook 'text-mode-hook
					(lambda ()
						(column-enforce-mode 1)
						(nlinum-mode 1)
						(auto-fill-mode 1)
						(flyspell-mode 1)))

(add-hook 'markdown-mode-hook
					(lambda ()
						(column-enforce-mode 1)
						(auto-fill-mode 1)
						(nlinum-mode 1)
						(flyspell-mode 1)))

(add-hook 'latex-mode-hook
					(lambda ()
						(fci-mode 1)
						(nlinum-mode 1)
						(flycheck-mode 1)
						(column-enforce-mode 1)
						(auto-fill-mode 1)
						(flyspell-mode 1)))

(add-hook 'erc-mode-hook
					(lambda ()
						(erc-notify-mode 1)
						(flyspell-mode 1)))

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
					'(lambda ()
						 (local-set-key (kbd "<backtab>") 'shr-previous-link)
						 (local-set-key (kbd "&") 'eww-external-browser-seamonkey)))

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
(global-set-key (kbd "C-x M f") 'emms-play-file) ;play (f)ile
(global-set-key (kbd "C-x M D") 'emms-play-directory) ;play (D)irectory
(global-set-key (kbd "C-x M v") 'emms-playlist-mode-go) ;(v)iew playlist
(global-set-key (kbd "C-x H a") 'helm-apropos)

;; For easy emoji finding
(global-set-key (kbd "C-x E a") 'emojify-apropos-emoji)

;; Undo trees are amazing.
;; (require 'undo-tree)
;; (global-undo-tree-mode)

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

(global-set-key (kbd "M-d") 'forward-delete-word-no-kill-ring)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word-no-kill-ring)
(global-set-key (kbd "M-D") 'kill-word)
(global-set-key (kbd "M-S-<backspace>") 'backward-kill-word)

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

;; Allows me to see the battery level and status on my Chromebook
;; Maybe one day I'll add it to the mode line
(defun battery-level ()
	(interactive)
	(let (charge-now charge-full status)
		(with-temp-buffer
			(insert-file-contents "/sys/class/power_supply/sbs-20-000b/charge_now")
			(goto-char (point-min))
			(kill-line)
			(setq charge-now (car kill-ring))
			(insert-file-contents "/sys/class/power_supply/sbs-20-000b/charge_full")
			(goto-char (point-min))
			(kill-line)
			(setq charge-full (car kill-ring))
			(insert-file-contents "/sys/class/power_supply/sbs-20-000b/status")
			(goto-char (point-min))
			(kill-line)
			(setq status (car kill-ring)))
		(setq charge-now (concat charge-now ".0")) ;make it a float
		(setq charge-full (concat charge-full ".0")) ;make it a float
		(message "%s, %s"
						 (* 100 (/ (string-to-number charge-now)
											 (string-to-number charge-full)))
						 status)))

(global-set-key (kbd "M-<f1>") 'battery-level)

;;; Others
(setq ispell-program-name "/usr/bin/aspell")
;; (setq inferior-R-program-name "/usr/bin/R")
(setq erc-nick "NuclearKev")

;;; Set Your lisp system and, optionally, some contribs
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (setq slime-contribs '(slime-fancy))
;;(slime-setup '(slime-company))
;;(global-company-mode)

;; Twittering-mode
(setq twittering-use-master-password t) ;allows me to automatically login to my twitter account
(setq twittering-icon-mode t)		;gimme pictures

;; eshell
;; Run "alias sudo 'eshell/sudo $*'" sudo to work right
(require 'em-tramp)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 300) ; for 5 minutes (time in secs)
(setq tramp-histfile-override "/dev/null")

(defun browse-url-seamonkey-new-tab (url &optional new-window)
	;; new-window ignored
	"Open URL in a new tab in Seamonkey."
	(interactive (browse-url-interactive-arg "URL: "))
	(unless
			(string= ""
							 (shell-command-to-string
								(concat "seamonkey -remote 'openURL(" url ",new-tab)'")))
		(message "Starting Seamonkey...")
		(start-process (concat "seamonkey " url) nil "seamonkey" url)
		(message "Starting Seamonkey...done")))

(defun eww-external-browser-seamonkey (&optional url)
	"Open *eww* webpage in external browser.  URL won't be used."
	(interactive)
	(eww-copy-page-url)
	(browse-url-seamonkey-new-tab (car kill-ring)))

;; jul-mode stuff
;(add-to-list 'load-path "~/jul-mode")
;(load "jul-mode.el")

;; Enable pdf-tools all day
(pdf-tools-install)

(setq youtube-dl-dir "~/Downloads/") ;make sure to have the '/' at the end

(defun youtube-dl-video (url)
	"Easily download youtube videos in Emacs!

Pass it the URL of the video you wish to download.  Then it will
	place the full youtube-dl command in your kill ring.  Yank this
	to an eshell buffer or something."
	(interactive "sURL: ")
	(kill-append (concat "youtube-dl --output " youtube-dl-dir
											 "\%\(title\)s.\%\(ext\)s ")
							 t))

(defun youtube-dl-ogg (url)
	"Easily download youtube videos in Emacs!

Pass it the URL of the video you wish to convert to ogg and
	download.  Then it will place the full youtube-dl command in
	your kill ring.  Yank this to an eshell buffer or something."
	(interactive "sURL: ")
	(kill-append (concat "youtube-dl -x --audio-format vorbis "
											 "--output " youtube-dl-dir
											 "\%\(title\)s.\%\(ext\)s ")
							 t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(battery-status-function (quote battery-linux-sysfs))
 '(custom-enabled-themes (quote (ample)))
 '(custom-safe-themes
	 (quote
		("750153eac49be640ea0d01754b4178756129e8fc6cbfc75312b0f5a5c96e29bf" "990690b46d1d999ac9c92e0228fb362e5486a6c1e48325e19784ca75f0e5cc1d" "9e6e8b2377c0a176f702934794a1e7b8909a46147790b52e1be94ac7bb0bf333" "93b3b86e65d36de17a7a9d45c8797ea1a1134a1f997824daf439ac0ae2f60426" "4ab95b35f7720043592b49d890003874aa1954a3cf299edde13657c6a9182d85" "e1876e272a7e7a82a6196818a5f50551910dbdffcba557de5cdb71c7307b1144" "7557aa0d3854c7e910121ba2ef94f4c4e70de7d32ddebb609719f545f7f7be0d" "0c9cd73bf12f4bea0009c9fe520d362180c1fcf72d7590b484c0f20e20d109dc" "366f94b5c9428b25dbc2ed7f80cd96314b7124acab404e30d201ebe9aac0ff9d" default)))
 '(eww-download-directory "~/Downloads")
 '(fill-column 80)
 '(org-agenda-files (quote ("~/org/Schedule.org")))
 '(org-s5-theme-file nil)
 '(package-selected-packages
	 (quote
		(ac-emoji markdown-mode org-plus-contrib pdf-tools emojify emms-player-mpv emms image+ twittering-mode nlinum multiple-cursors mic-paren magit highlight-parentheses helm flycheck fill-column-indicator column-enforce-mode auto-complete ample-theme ace-window)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "stmp.openmailbox.org" t)
 '(smtpmail-smtp-service 25 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#343333"))))
 '(region ((t (:background "#3C3C3C")))))

(provide '.emacs)

;;; .emacs ends here
