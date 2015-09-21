(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Set Your lisp system and, optionally, some contribs
(setq inferior-lisp-program "/usr/bin/sbcl")
;(setq slime-contribs '(slime-fancy))

;; Keybindings
(global-set-key (kbd "M-p") 'ace-window)
(global-set-key (kbd "M-C c") 'comment-region)
(global-set-key (kbd "M-C u") 'capitalize-word)
(global-set-key (kbd "M-C C") 'uncomment-region)

;; Start-up stuff
(setq column-number-mode t)
