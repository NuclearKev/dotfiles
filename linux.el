;;; linuxFunctions --- functions for linux systems
;;; Commentary:
;;; Functions that only work on linux systems.

;;; Code:

(setq output-dir "~/Desktop/") ;make sure to have the '/' at the end

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

(provide 'linux)
;;; linux.el ends here
