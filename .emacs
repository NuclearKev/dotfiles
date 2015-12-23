;;; Start up stuff
(setq column-number-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(display-time-mode t)

;; Shrink fringes to 1 pixel
(fringe-mode 1)

;; Display time and battery
(setq display-time-default-load-average nil)
(display-time-mode t)
(display-battery-mode t)

;; Disable Linum-mode for certain packages/files
(add-hook 'term-mode-hook
  '(lambda () (linum-mode 0)))
(add-hook 'calc-mode-hook
  '(lambda () (linum-mode 0)))
(add-hook 'eww-mode-hook
  '(lambda () (linum-mode 0)))


;; You are strongly encouraged to enable `ido-mode' (or something similar) to
;; alter to default behavior of 'C-x b', or you will take great pains to switch
;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
;; proposal however)
;; You may also want to call `exwm-enable-ido-workaround' later (see below)
(ido-mode 1)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section)
(server-start)

;;; Keybindings
(global-set-key (kbd "C-x o") 'ace-window) ;if installed
(global-set-key (kbd "M-C c") 'comment-region)
(global-set-key (kbd "M-C C") 'capitalize-word)
(global-set-key (kbd "M-C u") 'uncomment-region)
(global-set-key (kbd "M-#")   'dictionary-search) ;if installed
(global-set-key (kbd "M-*")   'calc)

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
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
;;(slime-setup '(slime-company))
;;(global-company-mode)


;;;; Below are configurations for EXWM

;; Add paths
(add-to-list 'load-path "~/.emacs.d/elpa/xelb-0.3/")
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

;;; If you want a theme, get it yourself ;;;
