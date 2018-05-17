;; .emacs --- Emacs customizations

;;; Commentary:
;;
;; My Emacs customizations.  Is there more to say?
;;
;; Packges:
;;
;; ac-emoji
;; ace-window
;; auto-complete
;; emms
;; emms-player
;; emojify
;; flycheck
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
;; projectile-mode

;;; Code:

(load "~/.emacs.d/func.el")

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
(column-number-mode 1)
;; (line-number-mode 1)
;;(set-frame-font "DejaVu Sans Mono")
;; (display-time-mode t)
;; (display-battery-mode t)
(fringe-mode 1)   ; Shrink fringes to 1 ;; (set-face-attribute 'default nil :height 120)
(global-hl-line-mode t)
;; (set-face-background 'hl-line "#3C3C3C")      ; for ample-theme
;; (set-face-background 'hl-line "#f3f3fc") ;for plan9 theme
(set-face-background 'hl-line "#c1dde6") ;for whiteboard theme
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
(setq tab-always-indent 'complete)
(setq initial-scratch-message "")


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

;; (add-hook 'after-init-hook #'global-emojify-mode) ;gimme emojis EVERYWHERE! 🖕

(setq backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
	 '(("." . "~/.backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

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
						;; (nlinum-mode 1)
						(auto-complete-mode 1)
						;; (fci-mode 1)
            (modalka-mode 1)
            (smartparens-mode 1)
						(flycheck-mode 1)))

(add-hook 'html-mode-hook
					(lambda ()
						;; (nlinum-mode 1)
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
						;; (nlinum-mode 1)
            (modalka-mode 1)
						(auto-fill-mode 1)
						(flyspell-mode 1)))

(add-hook 'markdown-mode-hook
					(lambda ()
						;; (column-enforce-mode 1)
						(auto-fill-mode 1)
						;; (nlinum-mode 1)
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

(add-hook 'org-mode-hook
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
;; For easy emoji finding
(global-set-key (kbd "C-x E a") 'emojify-apropos-emoji)
(global-set-key (kbd "M-d") 'forward-delete-word-no-kill-ring)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word-no-kill-ring)
(global-set-key (kbd "M-D") 'kill-word)
(global-set-key (kbd "M-S-<backspace>") 'backward-kill-word)
;; To help with my right wrist
(global-set-key (kbd "M-7") 'backward-delete-word-no-kill-ring)
(global-set-key (kbd "C-7") 'backward-delete-char-untabify)
(global-set-key (kbd "M-&") 'backward-kill-word)
(global-set-key (kbd "M-N") 'newline)
(global-set-key (kbd "C-x K") 'kill-and-delete-window)


(use-package emojify
  :hook ((text-mode     . emojify-mode)
         (org-mode      . emojify-mode)
         (markdown-mode . emojify-mode)
         (erc-mode      . emojify-mode)))


(use-package buffer-move
  :bind
  ("<C-S-up>"    . buf-move-up)
  ("<C-S-down>"  . buf-move-down)
  ("<C-S-left>"  . buf-move-left)
  ("<C-S-right>" . buf-move-right))


;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)


(use-package helm
  :bind
  ("C-x p f" . helm-projectile)
  ("C-x C-G" . helm-projectile)
  ("M-x"     . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("C-x b"   . helm-buffers-list)
  ("C-x H a" . helm-apropos)
  :config
  (setq-default helm-M-x-fuzzy-match                  t
                helm-bookmark-show-location           t
                helm-buffers-fuzzy-matching           t
                helm-completion-in-region-fuzzy-match t
                helm-file-cache-fuzzy-match           t
                helm-imenu-fuzzy-match                t
                helm-mode-fuzzy-match                 t
                helm-locate-fuzzy-match               t
                helm-quick-update                     t
                helm-recentf-fuzzy-match              t
                helm-semantic-fuzzy-match             t))



;;; Others
(setq-default ispell-program-name "/usr/local/bin/aspell")
;; (setq inferior-R-program-name "/usr/bin/R")
(setq-default erc-nick "nuclearkev")


(use-package slime-autoloads
  :config
  (setq-default inferior-lisp-program "/usr/local/bin/sbcl"))
(use-package slime
  :config
  (slime-setup)
  (setq slime-contribs '(slime-fancy)))
;;(slime-setup '(slime-company))
;;(global-company-mode)

(use-package auto-complete
  :hook
  ((prog-mode . auto-complete-mode)
   (html-mode . auto-complete-mode)
   (slime-mode . auto-complete-mode)
   (slime-repl-mode . auto-complete-mode))
  :config
  (add-to-list 'ac-modes 'slime-repl-mode))
(use-package ac-slime
  :hook
  ((slime-mode . set-up-slime-ac)
   (slime-repl-mode . set-up-slime-ac)))


;; Racket
;; Very laggy
(use-package racket-mode
  :config
  (setq-default racket-program "/usr/local/bin/racket")
  :bind
  ("C-c C-l" . racket-run))


(use-package fsharp-mode
  :disabled
  :config
  (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
  (setq-default fsharp-compiler "/usr/local/bin/fsharpc"))


(use-package modalka
  :config
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
  (modalka-define-kbd "c e" "C-c C-e")
  (modalka-define-kbd "c j" "C-c C-j")
  (define-key modalka-mode-map (kbd "c G") #'helm-projectile-grep)
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (define-key modalka-mode-map (kbd "f") #'forward-char)
  (modalka-define-kbd "g g" "M-g g")
  (modalka-define-kbd "g m" "M-G G")
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
  (define-key modalka-mode-map (kbd "h D") #'kill-word)
  (define-key modalka-mode-map (kbd "h v") #'scroll-down-command)
  (modalka-define-kbd "h >" "M->")
  (modalka-define-kbd "h <" "M-<")
  (define-key modalka-mode-map (kbd "h w") #'kill-ring-save)
  (define-key modalka-mode-map (kbd "h <backspace>") #'backward-delete-word-no-kill-ring)
  (define-key modalka-mode-map (kbd "h S-<backspace>") #'backward-kill-word)
  (define-key modalka-mode-map (kbd "h z") #'zap-to-char)
  (define-key modalka-mode-map (kbd "h 4") #'ispell-word)
  (define-key modalka-mode-map (kbd "h 5") #'query-replace)
  (define-key modalka-mode-map (kbd "h C") #'capitalize-word)
  (define-key modalka-mode-map (kbd "h u") #'undo-tree-visualize)
  (define-key modalka-mode-map (kbd "h L") #'downcase-word)
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
  ;; (define-key modalka-mode-map (kbd "m") #'newline)
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
  (define-key modalka-mode-map (kbd "x p w") #'pwd)
  (modalka-define-kbd "x m d" "C-x M d")
  (modalka-define-kbd "x u" "C-x u")
  (modalka-define-kbd "x o" "C-x o")
  (modalka-define-kbd "x k" "C-x k")
  (modalka-define-kbd "x b" "C-x b")
  (modalka-define-kbd "x d" "C-x d")
  (modalka-define-kbd "x (" "C-x (")
  (modalka-define-kbd "x )" "C-x )")
  (modalka-define-kbd "x e" "C-x e")
  (modalka-define-kbd "x e" "C-x e")
  (define-key modalka-mode-map (kbd "x l") #'nlinum-mode)
  (modalka-define-kbd "x H a" "C-x H a")
  (define-key modalka-mode-map (kbd "x SPC") #'rectangle-mark-mode)
  (define-key modalka-mode-map (kbd "x 8") #'calc)
  (define-key modalka-mode-map (kbd "x 7") #'calendar-mode)
  (define-key modalka-mode-map (kbd "y") #'yank)
  (define-key modalka-mode-map (kbd "z") #'zap-to-char-edit)
  (define-key modalka-mode-map (kbd "h z") #'zap-to-char)
  (define-key modalka-mode-map (kbd "SPC") #'set-mark-command)
  (define-key modalka-mode-map (kbd "/") #'undo-tree-undo)
  (define-key modalka-mode-map (kbd "-") #'negative-argument)
  ;; (define-key modalka-mode-map (kbd "H <SPC>") #'mark-sexp)
  (define-key modalka-mode-map (kbd "S-<SPC>") #'mark-sexp)
  (define-key modalka-mode-map (kbd "]") #'forward-sexp)
  (define-key modalka-mode-map (kbd "[") #'backward-sexp)
  ;; (define-key modalka-mode-map (kbd ";") #'iy-go-to-char)
  (define-key modalka-mode-map (kbd ";") #'iy-go-to-char-correctly)
  (define-key modalka-mode-map (kbd ":") #'iy-go-to-char-backward)
  (define-key modalka-mode-map (kbd ">") #'iy-go-to-or-up-to-continue)
  (define-key modalka-mode-map (kbd "<") #'iy-go-to-or-up-to-continue-backward)
  (define-key modalka-mode-map (kbd ".") #'repeat)
  (define-key modalka-mode-map (kbd "A") #'beginning-of-line-edit)
  (define-key modalka-mode-map (kbd "E") #'end-of-line-edit)
  (define-key modalka-mode-map (kbd "F") #'forward-word-edit)
  (define-key modalka-mode-map (kbd "B") #'backward-word-edit)
  (define-key modalka-mode-map (kbd "m") #'replace-char)
  (define-key modalka-mode-map (kbd "D") #'delete-forward-word-edit)
  (define-key modalka-mode-map (kbd "S-<backspace>") #'delete-backward-word-edit)
  (setq-default cursor-type '(bar . 1)
                modalka-cursor-type 'box))


;; eshell
;; Run "alias sudo 'eshell/sudo $*'" sudo to work right
(use-package em-tramp
  :init
  (setq-default eshell-prefer-lisp-functions t
                eshell-prefer-lisp-variables t
                password-cache t ; enable password caching
                password-cache-expiry 300 ; for 5 minutes (time in secs)
                tramp-histfile-override "/dev/null"))


(use-package projectile
      :ensure t
      :init
      (projectile-mode t)
      :config
      (setq projectile-enable-caching t))


;; IDK if this even works
;; use web-mode for .jsx files
(setq-default flycheck-javascript-eslint-executable "/usr/local/bin/eslint")
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))

;;; Haskell
(use-package haskell-interactive-mode
  :hook (haskell-mode . interactive-haskell-mode))
(use-package haskell-process)
(use-package hyai
  :init
  (setq-default hyai-basic-offset 2)
  :hook (haskell-mode . hyai-mode))
(use-package haskell-mode
  :config
  (haskell-indentation-mode -1)
  (haskell-indent-mode -1))

;;; hledger
(use-package hledger-mode
  :bind
  ("C-c C-e" . hledger-jentry)
  ("C-c C-j" . hledger-run-command)
  :init
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
  (setq-default hledger-currency-string "$"
                hledger-jfile "~/personal/money/kev.journal"))


(use-package darkroom)


(use-package undo-tree
  :config
  (global-undo-tree-mode))



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


;; Package Safety
(setq-default tls-checktrust t)
(setq-default gnutls-verify-error t)
(let ((trustfile "/etc/ssl/cert.pem"))
  (setq-default tls-program
        `(,(format  "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)
          ,(format "openssl s_client -connect %%h:%%p -CAfile %s -no_ssl2 -ign_eof" trustfile)))
  (setq-default gnutls-trustfiles (list trustfile)))
;; (let ((bad-hosts
;;        (loop for bad
;;              in `("https://wrong.host.badssl.com/"
;;                   "https://self-signed.badssl.com/")
;;              if (condition-case e
;;                     (url-retrieve
;;                      bad (lambda (retrieved) t))
;;                   (error nil))
;;              collect bad)))
;;   (if bad-hosts
;;       (error (format "tls misconfigured; retrieved %s ok"
;;                      bad-hosts))
;;     (url-retrieve "https://badssl.com"
;;                   (lambda (retrieved) t))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(battery-status-function (quote battery-linux-sysfs))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes
   (quote
    ("2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "44c566df0e1dfddc60621711155b1be4665dd3520b290cb354f8270ca57f8788" "2cf7f9d1d8e4d735ba53facdc3c6f3271086b6906c4165b12e4fd8e3865469a6" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "750153eac49be640ea0d01754b4178756129e8fc6cbfc75312b0f5a5c96e29bf" "990690b46d1d999ac9c92e0228fb362e5486a6c1e48325e19784ca75f0e5cc1d" "9e6e8b2377c0a176f702934794a1e7b8909a46147790b52e1be94ac7bb0bf333" "93b3b86e65d36de17a7a9d45c8797ea1a1134a1f997824daf439ac0ae2f60426" "4ab95b35f7720043592b49d890003874aa1954a3cf299edde13657c6a9182d85" "e1876e272a7e7a82a6196818a5f50551910dbdffcba557de5cdb71c7307b1144" "7557aa0d3854c7e910121ba2ef94f4c4e70de7d32ddebb609719f545f7f7be0d" "0c9cd73bf12f4bea0009c9fe520d362180c1fcf72d7590b484c0f20e20d109dc" "366f94b5c9428b25dbc2ed7f80cd96314b7124acab404e30d201ebe9aac0ff9d" default)))
 '(eww-download-directory "~/Downloads")
 '(fci-rule-color "#073642")
 '(fill-column 80)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files (quote ("~/org/Schedule.org")))
 '(org-s5-theme-file nil)
 '(package-selected-packages
   (quote
    (ac-slime clojure-mode nix-mode purescript-mode hledger-mode hyai restclient fireplace elm-mode modalka iy-go-to-char fsharp-mode geiser use-package buffer-move string-inflection nodejs-repl rjsx-mode jsx-mode omnisharp racket-mode exec-path-from-shell ## typescript-mode json-mode web-mode yasnippet avy ac-emoji markdown-mode org-plus-contrib pdf-tools emojify emms-player-mpv emms image+ nlinum multiple-cursors mic-paren magit highlight-parentheses helm flycheck fill-column-indicator column-enforce-mode auto-complete ace-window)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "bower_components" "elm-stuff")))
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(smtpmail-smtp-server "stmp.openmailbox.org")
 '(smtpmail-smtp-service 25)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

(provide '.emacs)

;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "black")))))
