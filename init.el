;; .emacs --- Emacs customizations

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
;; emms
;; emms-player
;; emojify
;; evil
;; flycheck
;; flx-ido
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
;; projectile-mode
;; twittering-mode

;;; Code:

(let ((minver "24.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)

;;; Enable debug during loading.
(setq debug-on-error t)

(defvar emacs-cache-folder "~/.cache/emacs/"
  "Cache folder is everything we do not want to track together
  with the configuration files.")
(if (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;; General Modifications
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(global-linum-mode t)
(column-number-mode t)
(line-number-mode -1)
;;(set-frame-font "DejaVu Sans Mono")
(display-time-mode t)
;; (display-battery-mode t)
(fringe-mode 1)   ; Shrink fringes to 1 ;; (set-face-attribute 'default nil :height 120)
(global-hl-line-mode t)
;; (set-face-background 'hl-line "#3C3C3C")      ; for ample-theme
;; (set-face-background 'hl-line "#f3f3fc") ;for plan9 theme
;; (ido-mode 1)
;; (setf ido-enable-flex-matching t)				;makes switches stuff easier
(setq-default tab-width 2)
(global-auto-revert-mode t)
(setq confirm-kill-emacs #'y-or-n-p)	;Asks if you wish to leave emacs
(setq org-src-fontify-natively t)	;syntax highlighting in org-modesource blocks
(setq browse-url-browser-function 'eww-browse-url)
(setq visible-bell 1)
(setq fill-column 80)
(setq echo-keystrokes 0.1)
;;; Show matching parenthesis
(show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
(setq show-paren-delay 0)
(setq show-paren-when-point-inside-paren t)


;; Work
;; You may need to get rid of the -default on the indent ones
(setq-default js-indent-level 2)
(setq-default js2-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default js2-strict-trailing-comma-warning nil) ;I don't care about commas
(setq-default typescript-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq-default jsx-indent-level 2)

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
						(nlinum-mode 1)
						(auto-complete-mode 1)
						;; (fci-mode 1)
						(flycheck-mode 1)))

(add-hook 'html-mode-hook
					(lambda ()
						(nlinum-mode 1)
						(auto-complete-mode 1)
						;; (fci-mode 1)
						(flycheck-mode 1)
            (flyspell-mode -1)))

(add-hook 'image-mode-hook 'imagex-sticky-mode)

;; Lets not forget the text modes!
(add-hook 'text-mode-hook
					(lambda ()
						;; (column-enforce-mode 1)
						(nlinum-mode 1)
						(auto-fill-mode 1)
						(flyspell-mode 1)))

(add-hook 'markdown-mode-hook
					(lambda ()
						;; (column-enforce-mode 1)
						(auto-fill-mode 1)
						(nlinum-mode 1)
						(flyspell-mode 1)))

(defun org-table-copy-down-no-inc (&optional arg)
  "Copy the previous cell's value without incrementing.
ARG is needed for `kill-word'."
  (interactive "p")
  (kill-word arg)
  (yank)
  (org-return)
  (org-table-blank-field)
  (yank)
  (org-table-align))

(add-hook ' org-mode-hook
            (lambda ()
              (local-set-key (kbd "C-S-<down>") 'org-table-copy-down-no-inc)))

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
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g M-c") 'avy-goto-char)
(global-set-key (kbd "M-g t") 'avy-goto-char-2)
(global-set-key (kbd "M-g M-t") 'avy-goto-char-2)
(global-set-key (kbd "M-g l") 'avy-goto-char-in-line)
(global-set-key (kbd "M-g M-l") 'avy-goto-char-in-line)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g M-f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g M-w") 'avy-goto-word-1)
(global-set-key (kbd "M-G G") 'magit-status)
;; (global-set-key (kbd "C-|") 'pop-global-mark) ;return to last cursor position
(global-set-key (kbd "C-x B") 'list-buffers)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x p f") 'projectile-find-file)
(global-unset-key (kbd "C-z"))         ;set to `suspend-frame' if you want that back :)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-h") 'ido-switch-buffer)
(global-set-key (kbd "M-k") 'ido-kill-buffer)
(add-hook 'org-mode-hook
					'(lambda () (local-set-key (kbd "M-h") 'ido-switch-buffer)))

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

(global-set-key (kbd "M-d") 'forward-delete-word-no-kill-ring)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word-no-kill-ring)
(global-set-key (kbd "M-D") 'kill-word)
(global-set-key (kbd "M-S-<backspace>") 'backward-kill-word)
;; To help with my right wrist
(global-set-key (kbd "M-7") 'backward-delete-word-no-kill-ring)
(global-set-key (kbd "C-7") 'backward-delete-char-untabify)
(global-set-key (kbd "M-&") 'backward-kill-word)
(global-set-key (kbd "M-N") 'newline)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

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


;; Evil bindings
(global-set-key (kbd "M-<SPC>") 'evil-force-normal-state)
(global-set-key (kbd "C-v") 'scroll-up-command)
(global-set-key (kbd "M-v") 'scroll-down-command)

;; (require 'hydra)
;; (defhydra hydra-movement (global-map "M-<SPC>")
;;   "Movement/Delete"
;;   ("j" backward-char)
;;   ("k" next-line)
;;   ("l" previous-line)
;;   (";" forward-char)
;;   ("J" backward-word)
;;   (":" forward-word)
;;   ("a" move-beginning-of-line)
;;   ("e" move-end-of-line)
;;   ("w" avy-goto-word-1)
;;   ("c" avy-goto-char)
;;   ("L" avy-goto-line)
;;   ("d" delete-char)
;;   ("M-d" forward-delete-word-no-kill-ring)
;;   ("DEL" backward-delete-char-untabify)
;;   ("M-DEL" backward-delete-word-no-kill-ring)
;;   ("K" kill-line)
;;   ("u" undo-tree-undo)
;;   ("U" undo-tree-visualize)
;;   ("C-<SPC>" set-mark-command)
;;   ("C-x <SPC>" rectangle-mark-mode)
;;   ("v" scroll-up-command)
;;   ("V" scroll-down-command)
;;   ("o" other-window)
;;   ("q" nil "quit"))

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

(require 'eyebrowse)
(eyebrowse-mode t)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;;; Others
(setq ispell-program-name "/usr/local/bin/aspell")
;; (setq inferior-R-program-name "/usr/bin/R")
(setq erc-nick "nuclearkev")

;;; Set Your lisp system and, optionally, some contribs
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime)
(slime-setup)
(setq slime-contribs '(slime-fancy))
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

(setq output-dir "~/Desktop/") ;make sure to have the '/' at the end

(defun youtube-dl-video (url)
	"Easily download youtube videos in Emacs!

Pass it the URL of the video you wish to download.  Then it will
	place the full youtube-dl command in your kill ring.  Yank this
	to an eshell buffer or something."
	(interactive "sURL: ")
	(kill-append (concat "youtube-dl --output " output-dir
											 "\%\(title\)s.\%\(ext\)s ")
							 t))

(defun youtube-dl-ogg (url)
	"Easily download youtube videos in Emacs!

Pass it the URL of the video you wish to convert to ogg and
	download.  Then it will place the full youtube-dl command in
	your kill ring.  Yank this to an eshell buffer or something."
	(interactive "sURL: ")
	(kill-append (concat "youtube-dl -x --audio-format vorbis "
											 "--output " output-dir
											 "\%\(title\)s.\%\(ext\)s ")
							 t))

(defun screen-shot (delay-time)
  "Easily take a screenshot your screen.
DELAY-TIME will specify how long until the screenshot is taken."
	(interactive "sDelay Time:")
	(shell-command (concat "scrot -d " delay-time))
	(shell-command (concat "mv *_scrot.png " output-dir)))

(defun screen-select ()
  "Easily take a screenshot of a cursor selected area."
	(interactive)
	(shell-command (concat "scrot -s"))
	(shell-command (concat "mv *_scrot.png " output-dir)))

(defun screen-record ()
  "Easily record your screen!"
  (interactive)
  (async-shell-command (concat "avconv -f alsa -ac 1 -i hw:1 -f x11grab -r 25 "
                               "-s 1920x1080 -i :0.0 -vcodec libx264 -threads 4 "
                               output-dir "screen-capture.mkv")))

;; Start projectile once everything is loaded
(use-package projectile
      :ensure t
      :init
      (projectile-mode t)
      :config
      ;; (setq projectile-require-project-root nil)
      (setq projectile-enable-caching t)
      ;;(projectile-global-mode)
      ;; (setq projectile-completion-system 'ivy)
      ;; (define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
      ;; (define-key projectile-mode-map [?\s-f] 'projectile-find-file)
      ;;(define-key projectile-mode-map [?\s-s] 'projectile-switch-project)
      ;;(define-key projectile-mode-map [?\s-g] 'projectile-grep)
)
(smartparens-global-mode t)

;; IDK if this even works
;; use web-mode for .jsx files
(setq flycheck-javascript-eslint-executable "/usr/local/bin/eslint")
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Temp fixes to auto-complete and fci-mode issue
;; (defvar sanityinc/fci-mode-suppressed nil)
;; (defadvice popup-create (before suppress-fci-mode activate)
;;   "Suspend fci-mode while popups are visible"
;;   (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
;;   (when fci-mode
;;     (turn-off-fci-mode)))
;; (defadvice popup-delete (after restore-fci-mode activate)
;;   "Restore fci-mode when all popups have closed"
;;   (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
;;     (setq sanityinc/fci-mode-suppressed nil)
;;     (turn-on-fci-mode)))

;; Haskell
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; (el-get-bundle slack)
;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "sked"
;;    :default t
;;    :client-id "57522817143.163641867380"
;;    :client-secret "b8a838e03ffabc43b4dad802e77bfaad"))
;;    ;; :token "aaaa-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
;;    ;; :subscribed-channels '(test-rename rrrrr)))

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

;; For writing D&D
(require 'darkroom)

;; Undo trees are amazing.
(require 'undo-tree)
(global-undo-tree-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#454545" "#cd5542" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(battery-status-function (quote battery-linux-sysfs))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "2cf7f9d1d8e4d735ba53facdc3c6f3271086b6906c4165b12e4fd8e3865469a6" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "750153eac49be640ea0d01754b4178756129e8fc6cbfc75312b0f5a5c96e29bf" "990690b46d1d999ac9c92e0228fb362e5486a6c1e48325e19784ca75f0e5cc1d" "9e6e8b2377c0a176f702934794a1e7b8909a46147790b52e1be94ac7bb0bf333" "93b3b86e65d36de17a7a9d45c8797ea1a1134a1f997824daf439ac0ae2f60426" "4ab95b35f7720043592b49d890003874aa1954a3cf299edde13657c6a9182d85" "e1876e272a7e7a82a6196818a5f50551910dbdffcba557de5cdb71c7307b1144" "7557aa0d3854c7e910121ba2ef94f4c4e70de7d32ddebb609719f545f7f7be0d" "0c9cd73bf12f4bea0009c9fe520d362180c1fcf72d7590b484c0f20e20d109dc" "366f94b5c9428b25dbc2ed7f80cd96314b7124acab404e30d201ebe9aac0ff9d" default)))
 '(eww-download-directory "~/Downloads")
 '(fci-rule-color "#f8fce8")
 '(fill-column 80)
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(org-agenda-files (quote ("~/org/Schedule.org")))
 '(org-s5-theme-file nil)
 '(package-selected-packages
   (quote
    (geiser use-package buffer-move string-inflection solarized-theme github-modern-theme nodejs-repl rjsx-mode jsx-mode omnisharp vue-html-mode vue-mode racket-mode exec-path-from-shell ## typescript-mode json-mode web-mode yasnippet avy plan9-theme ac-emoji markdown-mode org-plus-contrib pdf-tools emojify emms-player-mpv emms image+ twittering-mode nlinum multiple-cursors mic-paren magit highlight-parentheses helm flycheck fill-column-indicator column-enforce-mode auto-complete ample-theme ace-window)))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "bower_components" "elm-stuff")))
 '(send-mail-function (quote smtpmail-send-it))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(smtpmail-smtp-server "stmp.openmailbox.org")
 '(smtpmail-smtp-service 25))

(provide '.emacs)

;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
