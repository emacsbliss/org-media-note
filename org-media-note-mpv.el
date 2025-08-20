;;; org-media-note-mpv.el --- MPV integration for org-media-note -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
;;;; Requirements

(require 'org-media-note-core)

;;;; Variables

(defvar org-media-note-last-play-speed 1.0
  "Last play speed in mpv.")

(defvar org-media-note-last-volume 100.0
  "Last Volume in mpv.")

;;;; Commands
(defun org-media-note-play-smart (arg)
  "Open media file in mpv based on the current context.
If ARG argument is provided, force playing from beginning."
  (interactive "P")
  (cl-multiple-value-bind (file-or-url start-time end-time)
      (org-media-note--get-media-info)
    (when file-or-url
      (org-media-note--follow-link file-or-url
                                   (if arg nil start-time)
                                   (if arg nil end-time)))))

(defun org-media-note-seek (direction)
  "Seek in the given DIRECTION according to the configured method and value."
  (interactive)
  (let ((was-pause (eq (mpv-get-property "pause") t))
        (backward? (eq direction 'backward)))
    (cl-case org-media-note-seek-method
      (seconds (mpv-run-command "seek"
                                (if backward?
                                    (- org-media-note-seek-value)
                                  org-media-note-seek-value)
                                "relative"))
      (percentage (mpv-run-command "seek"
                                   (if backward?
                                       (- org-media-note-seek-value)
                                     org-media-note-seek-value)
                                   "relative-percent"))
      (frames (progn
                (dotimes (_ org-media-note-seek-value)
                  (mpv-run-command (if backward? "frame-back-step" "frame-step"))))
              (sleep-for 0.3) ;; without this, frame seek cannot resume play
              (when (not was-pause)
                (mpv-run-command "set_property" "pause" "no"))))))

(defun org-media-note-change-speed-by (speed-step)
  "Modify playing media's speed by SPEED-STEP."
  (let ((current-speed (mpv-get-property "speed")))
    (mpv-speed-set (+ current-speed speed-step))))

(defun org-media-note-mpv-toggle-speed ()
  "Toggle playback speed of media."
  (interactive)
  (let ((current-speed (mpv-get-property "speed")))
    (if (= current-speed 1.0)
        (mpv-speed-set org-media-note-last-play-speed)
      (setq org-media-note-last-play-speed current-speed)
      (mpv-speed-set 1))))

(defun org-media-note-change-volume-by (step)
  "Set playing volume by STEP."
  (mpv-run-command "add" "volume" step)
  (setq org-media-note-last-volume (mpv-get-property "volume")))

(defun org-media-note-mpv-toggle-volume ()
  "Toggle plaback volume of media."
  (interactive)
  (let ((current-volume (mpv-get-property "volume")))
    (if (= 100.0 current-volume)
        (mpv-set-property "volume" org-media-note-last-volume)
      (progn
        (setq org-media-note-last-volume current-volume)
        (mpv-set-property "volume" 100)))))


;;;; Footer
(provide 'org-media-note-mpv)
;;; org-media-note-mpv.el ends here
