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
;; flycheck
;; flx-ido
;; helm
;; highlight-parentheses
;; image+
;; magit
;; markdown-mode
;; mic-paren
;; modalka-mode
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
;; (setq debug-on-error t)

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
(setq-default org-src-fontify-natively t)	;syntax highlighting in org-modesource blocks
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
(setq-default ftp-program "sftp")
(setq-default vs-display-status nil)

;; Work
;; You may need to get rid of the -default on the indent ones
(setq-default js-indent-level 2)
(setq-default js2-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default js2-strict-trailing-comma-warning nil) ;I don't care about commas
(setq-default typescript-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq-default jsx-indent-level 2)
(setq-default fsharp-indent-level 2)
(setq-default fsharp-indent-offset 2)
(setq-default fsharp-ac-intellisense-enabled nil)

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
  (setq-default
   package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
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
            (modalka-mode 1)
						(flycheck-mode 1)))

(add-hook 'html-mode-hook
					(lambda ()
						(nlinum-mode 1)
						(auto-complete-mode 1)
						;; (fci-mode 1)
            (modalka-mode 1)
						(flycheck-mode 1)
            (flyspell-mode -1)))

(add-hook 'image-mode-hook 'imagex-sticky-mode)

;; Lets not forget the text modes!
(add-hook 'text-mode-hook
					(lambda ()
						;; (column-enforce-mode 1)
						(nlinum-mode 1)
            (modalka-mode 1)
						(auto-fill-mode 1)
						(flyspell-mode 1)))

(add-hook 'markdown-mode-hook
					(lambda ()
						;; (column-enforce-mode 1)
						(auto-fill-mode 1)
						(nlinum-mode 1)
            (modalka-mode 1)
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
(global-set-key (kbd "M-g c") 'iy-go-to-char)
(global-set-key (kbd "M-g C") 'iy-go-to-char-backward)
(global-set-key (kbd "M-g ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "M-g :") 'iy-go-to-or-up-to-continue-backward)
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

(require 'fsharp-mode)
(setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/local/bin/fsharpc")

;; Twittering-mode
(setq twittering-use-master-password t) ;allows me to automatically login to my twitter account
(setq twittering-icon-mode t)		;gimme pictures


(add-hook 'haskell-mode-hook
					'(lambda ()
						 (local-set-key (kbd "C-;") 'iy-go-to-char)))

(modalka-mode 1)
(add-to-list 'modalka-excluded-modes 'magit-status-mode)
(global-set-key (kbd "<escape>") #'modalka-mode)
;; (global-set-key (kbd "<escape>") #'modalka-global-mode)
(define-key modalka-mode-map (kbd "0") #'digit-argument)
(define-key modalka-mode-map (kbd "1") #'digit-argument)
(define-key modalka-mode-map (kbd "2") #'digit-argument)
(define-key modalka-mode-map (kbd "3") #'digit-argument)
(define-key modalka-mode-map (kbd "4") #'digit-argument)
(define-key modalka-mode-map (kbd "5") #'digit-argument)
(define-key modalka-mode-map (kbd "6") #'digit-argument)
(define-key modalka-mode-map (kbd "7") #'digit-argument)
(define-key modalka-mode-map (kbd "8") #'digit-argument)
(define-key modalka-mode-map (kbd "9") #'digit-argument)
(modalka-define-kbd "a" "C-a")
(define-key modalka-mode-map (kbd "b") #'backward-char)
(modalka-define-kbd "c c" "C-c C-c")
(modalka-define-kbd "c k" "C-c C-k")
(modalka-define-kbd "c l" "C-c C-l")
(modalka-define-kbd "c p" "C-c C-p")
(modalka-define-kbd "c n" "C-c C-n")
(modalka-define-kbd "c o" "C-c C-o")
(modalka-define-kbd "c G" "C-c p s g")
(modalka-define-kbd "d" "C-d")
(modalka-define-kbd "e" "C-e")
(define-key modalka-mode-map (kbd "f") #'forward-char)
(modalka-define-kbd "g g" "M-g g")
(modalka-define-kbd "g f" "M-g f")
(modalka-define-kbd "g l" "M-g l")
(modalka-define-kbd "g w" "M-g w")
(define-key modalka-mode-map (kbd "h f") #'forward-word)
(define-key modalka-mode-map (kbd "h b") #'backward-word)
(modalka-define-kbd "h a" "M-a")
(modalka-define-kbd "h e" "M-e")
(modalka-define-kbd "h n" "M-n")
(modalka-define-kbd "h p" "M-p")
(define-key modalka-mode-map (kbd "h d") #'forward-delete-word-no-kill-ring)
(define-key modalka-mode-map (kbd "h v") #'scroll-down-command)
(modalka-define-kbd "h >" "M->")
(modalka-define-kbd "h <" "M-<")
(define-key modalka-mode-map (kbd "h w") #'kill-ring-save)
(define-key modalka-mode-map (kbd "h <backspace>") #'backward-delete-word-no-kill-ring)
(define-key modalka-mode-map (kbd "h z") #'zap-to-char)
(define-key modalka-mode-map (kbd "h 4") #'ispell-word)
(define-key modalka-mode-map (kbd "h 5") #'query-replace)
(define-key modalka-mode-map (kbd "h C") #'capitalize-word)
(define-key modalka-mode-map (kbd "h u") #'undo-tree-visualize)
(define-key modalka-mode-map (kbd "h l") #'downcase-word)
(define-key modalka-mode-map (kbd "h c") #'comment-region)
(define-key modalka-mode-map (kbd "h u") #'uncomment-region)
(define-key modalka-mode-map (kbd "h U") #'upcase-word)
(define-key modalka-mode-map (kbd "h /") #'dabbrev-expand)
(define-key modalka-mode-map (kbd "h /") #'dabbrev-expand)
(modalka-define-kbd "h x" "M-x")
(define-key modalka-mode-map (kbd "h t") #'transpose-words)
(define-key modalka-mode-map (kbd "h \\") #'delete-horizontal-space)
(define-key modalka-mode-map (kbd "\\") #'delete-horizontal-space)
(define-key modalka-mode-map (kbd "h ;") #'comment-dwim)
(modalka-define-kbd "i" "TAB")
(modalka-define-kbd "j" "C-j")
(define-key modalka-mode-map (kbd "k") #'kill-line)
(define-key modalka-mode-map (kbd "l") #'recenter-top-bottom)
(define-key modalka-mode-map (kbd "m") #'newline)
(define-key modalka-mode-map (kbd "n") #'next-line)
(define-key modalka-mode-map (kbd "o") #'open-line)
(define-key modalka-mode-map (kbd "p") #'previous-line)
(modalka-define-kbd "q" "C-q")
(define-key modalka-mode-map (kbd "r") #'isearch-backward)
(define-key modalka-mode-map (kbd "s") #'isearch-forward)
(define-key modalka-mode-map (kbd "t") #'transpose-chars)
(define-key modalka-mode-map (kbd "u") #'universal-argument)
(define-key modalka-mode-map (kbd "v") #'scroll-up-command)
(define-key modalka-mode-map (kbd "w") #'kill-region)
(modalka-define-kbd "x 0" "C-x 0")
(modalka-define-kbd "x 1" "C-x 1")
(modalka-define-kbd "x 2" "C-x 2")
(modalka-define-kbd "x 3" "C-x 3")
(modalka-define-kbd "x x" "C-x C-x")
(modalka-define-kbd "x f" "C-x C-f")
(modalka-define-kbd "x ;" "C-x C-;")
(modalka-define-kbd "x s" "C-x C-s")
(modalka-define-kbd "x p f" "C-x p f")
(modalka-define-kbd "x m d" "C-x M d")
(modalka-define-kbd "x u" "C-x u")
(modalka-define-kbd "x o" "C-x o")
(modalka-define-kbd "x k" "C-x k")
(modalka-define-kbd "x b" "C-x b")
(modalka-define-kbd "x d" "C-x d")
(modalka-define-kbd "x (" "C-x (")
(modalka-define-kbd "x )" "C-x )")
(modalka-define-kbd "x e" "C-x e")
(modalka-define-kbd "x H a" "C-x H a")
(define-key modalka-mode-map (kbd "x SPC") #'rectangle-mark-mode)
(define-key modalka-mode-map (kbd "x *") #'calc)
(define-key modalka-mode-map (kbd "x &") #'calendar-mode)
(define-key modalka-mode-map (kbd "y") #'yank)
(define-key modalka-mode-map (kbd "z") #'zap-to-char)
(define-key modalka-mode-map (kbd "SPC") #'set-mark-command)
(define-key modalka-mode-map (kbd "/") #'undo-tree-undo)
(define-key modalka-mode-map (kbd "-") #'negative-argument)
(define-key modalka-mode-map (kbd "H <SPC>") #'mark-sexp)
(define-key modalka-mode-map (kbd "H f") #'forward-sexp)
(define-key modalka-mode-map (kbd "H b") #'backward-sexp)
(define-key modalka-mode-map (kbd "]") #'forward-sexp)
(define-key modalka-mode-map (kbd "[") #'backward-sexp)
(define-key modalka-mode-map (kbd ";") #'iy-go-to-char)
(define-key modalka-mode-map (kbd ":") #'iy-go-to-char-backward)
(define-key modalka-mode-map (kbd ">") #'iy-go-to-or-up-to-continue)
(define-key modalka-mode-map (kbd "<") #'iy-go-to-or-up-to-continue-backward)
(define-key modalka-mode-map (kbd ".") #'repeat)

(setq-default cursor-type '(bar . 1))
(setq modalka-cursor-type 'box)

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

;; org-more site generation stuff
(setq org-html-htmlize-output-type 'css)

(setq my-html-preamble
      "<header id=\"banner\">
  <h1><a href=\"/home.html\">Kevin \"The Nuclear\" Bloom</a></h1>
  <hr />
  <nav><ul>
    <li><a href=\"/contact.html\">Contact</a></li>
    <li><a href=\"/blog/blog.html\">Blog</a></li>
    <li><a href=\"/projects.html\">Projects</a></li>
    <li><a href=\"/about-me.html\">About Me</a></li>
  </ul></nav>
</header>")

(setq org-html-head
      (with-temp-buffer
        (let ((css-dir (file-name-as-directory "~/org-site/www/styles/"))
              (css-files '("main.css")))
          (insert "<style type=\"text/css\">\n")
          (dolist (file css-files)
            (insert-file-contents (concat css-dir file)))
          (insert "</style>")
          (buffer-string))))

(setq my-blog-extra-head
      (concat
       "<link rel='stylesheet' href='/../styles/main.css' />"))

(defun insert-css ()
  "Insert CSS into the stylesheet."
  (let* ((css-dir (expand-file-name (plist-get project-plist :publishing-directory)))
         (css-files (directory-files css-dir t "^.*\\.css$")))
    (dolist (file css-files)
      (with-temp-buffer
        (insert-file-contents file)
        (write-file file)))))

(defun my-blog-get-preview (file)
  "The comments in FILE have to be on their own lines, prefereably before and after paragraphs.
Written by Dennis Ogbe, modified by Kevin Bloom."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (+ 1 (re-search-forward "^#\\+BEGIN_PREVIEW$")))
          (end (progn (re-search-forward "^#\\+END_PREVIEW$")
                      (match-beginning 0))))
      (buffer-substring beg end))))

(defun my-blog-sitemap (project &optional sitemap-filename)
  "Generate the sitemap for my blog. Written by Dennis Ogbe, modified by Kevin Bloom."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (localdir (file-name-directory dir))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file sitemap-filename))))
      (erase-buffer)
      ;; loop through all of the files in the project
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link ;; changed this to fix links. see postprocessor.
               (file-relative-name file (file-name-as-directory
                                         (expand-file-name (concat (file-name-as-directory dir) "..")))))
              (oldlocal localdir))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ;; sitemap shouldn't list itself
          (unless (equal (file-truename sitemap-filename)
                         (file-truename file))
            (let (;; get the title and date of the current file
                  (title (org-publish-format-file-entry "%t" file project-plist))
                  (date (org-publish-format-file-entry "%d" file project-plist))
                  ;; get the preview section from the current file
                  (preview (my-blog-get-preview file))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              ;; insert a horizontal line before every post, kill the first one
              ;; before saving
              (insert "-----\n")
              (let ((new-link (reduce (lambda (x y) (concat x "/" y)) (cdr (split-string link "/")))))
                (cond ((string-match-p regexp title)
                       (string-match regexp title)
                       ;; insert every post as headline
                       (insert (concat"* " (match-string 1 title)
                                      "[[file:" new-link "]["
                                      (match-string 2 title)
                                      "]]" (match-string 3 title) "\n")))
                      (t (insert (concat "* [[file:" new-link "][" title "]]\n"))))
                ;; add properties for `ox-rss.el' here
                (let ((rss-permalink (concat (file-name-sans-extension link) ".html"))
                      (rss-pubdate (format-time-string
                                    (car org-time-stamp-formats)
                                    (org-publish-find-date file))))
                  (org-set-property "RSS_PERMALINK" rss-permalink)
                  (org-set-property "PUBDATE" rss-pubdate))
                ;; insert the date, preview, & read more link
                (insert (concat date "\n\n"))
                (insert preview)
                (insert (concat "[[file:" new-link "][Read More...]]\n")))))))
      ;; kill the first hrule to make this look OK
      (goto-char (point-min))
      (let ((kill-whole-line t)) (kill-line))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(setq org-publish-project-alist
      `(("site" :components ("main" "main-static" "blogs" "styles"))
        ("main"
         :base-directory "~/org-site/"
         :base-extension "org"
         :publishing-directory "~/org-site/www/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil ;; important!!

         :html-link-home "/"
         :html-head nil ;; cleans up anything that would have been in there.
         :html-head-extra ,my-blog-extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
         :html-home/up-format ""
         :html-link-up ""
         :html-link-home ""
         ;; :auto-preamble t
         :html-postamble nil
         :html-preamble ,my-html-preamble
         )
        ("blogs"
         :base-directory "~/org-site/blog"
         :base-extension "org"
         :publishing-directory "~/org-site/www/blog"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil ;; important!!

         :html-link-home "/"
         :html-head nil ;; cleans up anything that would have been in there.
         :html-head-extra ,my-blog-extra-head
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil
         :html-home/up-format ""
         :html-link-up ""
         :html-link-home ""
         ;; :auto-preamble t
         :html-postamble nil
         :html-preamble ,my-html-preamble

         ;; sitemap - list of blog articles
         :auto-sitemap t
         :sitemap-filename "blog.org"
         :sitemap-title ""

         :title "Blog Posts"
         ;; custom sitemap generator function
         :sitemap-function my-blog-sitemap
         :sitemap-sort-files anti-chronologically
         :sitemap-date-format "Published: %a %b %d %Y"
         )
        ("main-static"
         :base-directory "~/org-site/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/org-site/www/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("styles"
         :base-directory "~/org-site/styles"
         :base-extension ".*"
         :publishing-directory "~/org-site/www/styles/"
         :publishing-function org-publish-attachment
         ;; :completion-function insert-css
         )
        ))

;; Package Safety
(setq-default tls-checktrust t)
(setq-default gnutls-verify-error t)
(let ((trustfile "/etc/ssl/cert.pem"))
  (setq-default tls-program
        `(,(format  "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)
          ,(format "openssl s_client -connect %%h:%%p -CAfile %s -no_ssl2 -ign_eof" trustfile)))
  (setq-default gnutls-trustfiles (list trustfile)))
(let ((bad-hosts
       (loop for bad
             in `("https://wrong.host.badssl.com/"
                  "https://self-signed.badssl.com/")
             if (condition-case e
                    (url-retrieve
                     bad (lambda (retrieved) t))
                  (error nil))
             collect bad)))
  (if bad-hosts
      (error (format "tls misconfigured; retrieved %s ok"
                     bad-hosts))
    (url-retrieve "https://badssl.com"
                  (lambda (retrieved) t))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(battery-status-function (quote battery-linux-sysfs))
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "2cf7f9d1d8e4d735ba53facdc3c6f3271086b6906c4165b12e4fd8e3865469a6" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "750153eac49be640ea0d01754b4178756129e8fc6cbfc75312b0f5a5c96e29bf" "990690b46d1d999ac9c92e0228fb362e5486a6c1e48325e19784ca75f0e5cc1d" "9e6e8b2377c0a176f702934794a1e7b8909a46147790b52e1be94ac7bb0bf333" "93b3b86e65d36de17a7a9d45c8797ea1a1134a1f997824daf439ac0ae2f60426" "4ab95b35f7720043592b49d890003874aa1954a3cf299edde13657c6a9182d85" "e1876e272a7e7a82a6196818a5f50551910dbdffcba557de5cdb71c7307b1144" "7557aa0d3854c7e910121ba2ef94f4c4e70de7d32ddebb609719f545f7f7be0d" "0c9cd73bf12f4bea0009c9fe520d362180c1fcf72d7590b484c0f20e20d109dc" "366f94b5c9428b25dbc2ed7f80cd96314b7124acab404e30d201ebe9aac0ff9d" default)))
 '(eww-download-directory "~/Downloads")
 '(fci-rule-color "#073642")
 '(fill-column 80)
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(org-agenda-files (quote ("~/org/Schedule.org")))
 '(org-s5-theme-file nil)
 '(package-selected-packages
   (quote
    (2048-game modalka iy-go-to-char material-theme fsharp-mode geiser use-package buffer-move string-inflection solarized-theme github-modern-theme nodejs-repl rjsx-mode jsx-mode omnisharp racket-mode exec-path-from-shell ## typescript-mode json-mode web-mode yasnippet avy ac-emoji markdown-mode org-plus-contrib pdf-tools emojify emms-player-mpv emms image+ twittering-mode nlinum multiple-cursors mic-paren magit highlight-parentheses helm flycheck fill-column-indicator column-enforce-mode auto-complete ample-theme ace-window)))
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
