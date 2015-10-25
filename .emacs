;;; Start up stuff
(setq column-number-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(display-time-mode t)

;;; Keybindings
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-C c") 'comment-region)
(global-set-key (kbd "M-C C") 'capitalize-word)
(global-set-key (kbd "M-C u") 'uncomment-region)
(global-set-key (kbd "C-g")   'goto-line)
(global-set-key (kbd "C-u")   'keyboard-escape-quit)
(global-set-key (kbd "C-.")   'universal-argument)

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

;;; Theme setup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["black" "#E2434C" "#86B187" "#E0D063" "#84C452" "#E18CBB" "#8AC6F2" "white"])
 '(custom-enabled-themes (quote (badger)))
 '(custom-safe-themes (quote ("e6ca248a7326dbd5956a5d81ecc4dd1ac160a0ccded9df6a52f640b08a8b8f34" "5aa1843bfb10d672ed7348bbbb7f48d18069a52dacdf93516ece32f5a1ffea7e" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "fc9a20f684f622018eb71b840c72ec9c761b60519944426ef8bcb8127fefe491" default)))
 '(fci-rule-color "#343d46")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#bf616a") (40 . "#DCA432") (60 . "#ebcb8b") (80 . "#B4EB89") (100 . "#89EBCA") (120 . "#89AAEB") (140 . "#C189EB") (160 . "#bf616a") (180 . "#DCA432") (200 . "#ebcb8b") (220 . "#B4EB89") (240 . "#89EBCA") (260 . "#89AAEB") (280 . "#C189EB") (300 . "#bf616a") (320 . "#DCA432") (340 . "#ebcb8b") (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
