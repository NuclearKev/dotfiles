(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(column-number-mode t)
(line-number-mode -1)
(display-time-mode t)
(fringe-mode 1)   ; Shrink fringes to 1 pixel
(set-face-attribute 'default nil :height 160)
(global-hl-line-mode t)
(set-face-background hl-line-face "grey15")
(ido-mode 1)
(setq-default tab-width 2)
(setq confirm-kill-emacs #'y-or-n-p)	;Asks if you wish to leave emacs

;; Disable Linum-mode for certain modes
(add-hook 'term-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'slime-repl-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'calc-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'calc-trail-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'eww-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'twittering-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'erc-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'eshell-mode-hook
	  '(lambda () (linum-mode 0)))
(add-hook 'inferior-ess-mode-hook
	  '(lambda () (linum-mode 0)))

;; Start flyspell for text and latex mode
(add-hook 'text-mode-hook
	  '(lambda () (flyspell-mode t)))
(add-hook 'latex-mode-hook
	  '(lambda () (flyspell-mode t)))
(add-hook 'erc-mode-hook
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


;; Enable Auto-Complete-mode globally
(add-hook 'after-init-hook 'global-auto-complete-mode)

;; Enable a line at the 80 character column for certain modes
(setq fci-rule-column 80)

(add-hook 'text-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'latex-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'emacs-lisp-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'common-lisp-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'lisp-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'sh-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'R-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'C-mode-hook
	  '(lambda () (fci-mode t)))
(add-hook 'rust-mode-hook
	  '(lambda () (fci-mode t)))

;; Personal Keybindings
(global-set-key (kbd "M-C c") 'comment-region)
(global-set-key (kbd "M-C C") 'capitalize-word)
(global-set-key (kbd "M-C u") 'uncomment-region)
(global-set-key (kbd "M-#")   'dictionary-search)
(global-set-key (kbd "M-<f1>")   'calc)
(global-set-key (kbd "M-<f2>")   'calendar)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g M-f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-G G") 'magit-status)

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

;; Mac only
(setq mac-command-key-is-meta t
      mac-command-modifier 'meta
      ns-function-modifier 'control)

(setq ispell-program-name "/usr/local/bin/ispell")
(setq inferior-R-program-name "/usr/local/bin/R")
(setq erc-nick "NuclearKev")  

;;; Set up to use melpa packages
;;; You may need to comment this out to get certain packaes (like ace-window)
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

;;;; Below are configurations for EXWM

;; Emacs server is not required to run EXWM but it has some interesting uses
(server-start)

;; Add paths
(add-to-list 'load-path "~/.emacs.d/elpa/xelb-0.4/")
(add-to-list 'load-path "~/.emacs.d/elpa/exwm-0.1/")
(add-to-list 'load-path "~/.emacs.d/elpa/cl-generic-0.2/")

;; Load EXWM
(require 'exwm)

;; Fix problems with Ido
(require 'exwm-config)
(exwm-config-ido)

;; Sets there to be 10 workspaces
(setq exwm-workspace-number 10)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; `exwm-input-set-key' allows you to set a global key binding (available in
;; any case). Following are a few examples.
;; + We always need a way to go back to line-mode from char-mode
(exwm-input-set-key (kbd "s-r") 'exwm-reset)
;; + Bind a key to switch workspace interactively
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
;; + Set shortcuts to switch to a certain workspace.
(exwm-input-set-key (kbd "s-0")
                    (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-1")
                    (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-2")
                    (lambda () (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-3")
                    (lambda () (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-4")
                    (lambda () (interactive) (exwm-workspace-switch 4)))
(exwm-input-set-key (kbd "s-5")
                    (lambda () (interactive) (exwm-workspace-switch 5)))
(exwm-input-set-key (kbd "s-6")
                    (lambda () (interactive) (exwm-workspace-switch 6)))
(exwm-input-set-key (kbd "s-7")
                    (lambda () (interactive) (exwm-workspace-switch 7)))
(exwm-input-set-key (kbd "s-8")
                    (lambda () (interactive) (exwm-workspace-switch 8)))
(exwm-input-set-key (kbd "s-9")
                    (lambda () (interactive) (exwm-workspace-switch 9)))

;; + Application launcher ('M-&' also works if the output buffer does not
;;   bother you). Note that there is no need for processes to be created by
;;   Emacs.
(exwm-input-set-key (kbd "s-<SPC>")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

(exwm-input-set-key (kbd "s-l")
                    (lambda () (interactive) (start-process "" nil "slock")))

;; The following example demonstrates how to set a key binding only available
;; in line mode. It's simply done by first push the prefix key to
;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
;; The example shorten 'C-c q' to 'C-q'.
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.
(exwm-input-set-simulation-keys
 '(([?\C-b] . left)
   ([?\C-f] . right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)))

;; Do not forget to enable EXWM. It will start by itself when things are ready.
(exwm-enable)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(browse-url-browser-function (quote eww-browse-url))
 '(custom-enabled-themes (quote (grandshell)))
 '(custom-safe-themes
   (quote
    ("f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "dc54983ec5476b6187e592af57c093a42790f9d8071d9a0163ff4ff3fbea2189" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "b959f70a09f7ae16812bfc5bec2fd6b21081bee1f68686cdd80b3045bfc27b21" "693f5a81a3728c2548efb4118c81941933cf0f7b614f9f3133101395e5830152" default)))
 '(fci-rule-color "#383838")
 '(jabber-account-list nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
