;;; org-media-note.el --- Taking video and audio notes with Org-mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Package-Requires: ((emacs "27.1") mpv pretty-hydra)

;;; Commentary:

;; Take notes for video and audio files.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
;;;; Requirements
(require 'mpv)
(require 'org)
(require 'pretty-hydra)
(require 'org-attach)

(declare-function org-timer-secs-to-hms "org-timer")
(declare-function org-timer-hms-to-secs "org-timer")
(declare-function org-attach-dir "org-attach")

;;;; Customization

(defgroup org-media-note nil
  "Taking video and audio notes with Org-mode."
  :group 'org
  :prefix "org-media-note-")

(defcustom org-media-note-screenshot-save-method 'directory
  "The way images should be stored.
1. directory: save to `org-media-note-screenshot-image-dir'
2. attach: save to corresponding org-attach-dir."
  :type '(choice
          (const :tag "Directory" directory)
          (const :tag "Attachment" attach)))

(defcustom org-media-note-screenshot-link-type-when-save-in-attach-dir 'file
  "Use file: link or attachment: link when `org-media-note-screenshot-save-method' is attach?
file link is more general while attachment link is more concise."
  :type '(choice
          (const :tag "file:" file)
          (const :tag "attachment:" attach)))

(defcustom org-media-note-screenshot-image-dir org-directory
  "Default dir to save screenshots when `org-media-note-screenshot-save-method' is set to directory."
  :type 'string)

(defcustom org-media-note-save-screenshot-p nil
  "Whether to save screenshot."
  :type 'boolean)

(defcustom org-media-note-screenshot-with-sub t
  "When saving screenshots, whether to save subtitles."
  :type 'boolean)

(defcustom org-media-note-use-refcite-first nil
  "When non-nil, use refcite instead of file path when taking notes if possible."
  :type 'boolean)

(defcustom org-media-note-display-inline-images t
  "When non-nil, display inline images in org buffer after insert screenshot."
  :type 'boolean)

(defcustom org-media-note-pause-after-insert-link nil
  "When non-nil, pause the media after inserting timestamp link."
  :type 'boolean
  )

(defcustom org-media-note-timestamp-link-format "%timestamp"
  "Timestamp Link text.  Allows the following substitutions:
%filename :: name of the media file
%timestamp :: current media timestamp (hms)
%duration :: length of the media file (hms)
%file-path :: path of the media file"
  :type 'string)

(defcustom org-media-note-ab-loop-link-format "%ab-loop-a-%ab-loop-b"
  "AB-loop Link text.  Allows the following substitutions:
%filename :: name of the media file
%ab-loop-a :: timestamp of point a of ab loop (hms)
%ab-loop-b :: timestamp of point b of ab loop (hms)
%duration :: length of the media file (hms)
%file-path :: path of the media file"
  :type 'string)

(defcustom org-media-note-cursor-start-position 'after
  "After inserting a link, should the cursor move to the point
before the link, or the point after?"
  :type 'symbol
  :options '(before after))

(defcustom org-media-note-link-prefix ""
  "Whether to prefix the link text with any text that is
not part of the link.  Most common use is to insert a space
that is not part of the link if the user sets 
`org-media-note-cursor-start-position' to 'before, they likely
want a space that is not part of the link itself."
  :type 'string)

;;;; Variables

(defconst org-media-note--video-types '("avi" "rmvb" "ogg" "ogv" "mp4" "mkv" "mov" "webm" "flv" "ts" "mpg"))
(defconst org-media-note--audio-types '("flac" "mp3" "wav"))

(defvar org-media-note-last-play-speed 1.0
  "Last play speed in mpv.")

(defvar org-media-note-last-volume 100.0
  "Last Volume in mpv.")

;;;; Commands
;;;;; Hydra

(defun org-media-note--hydra-title ()
  "Title for `org-media-note-hydra'"
  (let ((file-path (mpv-get-property "path"))
        (ref-key (org-media-note--current-org-ref-key))
        (icon (if (featurep 'all-the-icons)
                  (all-the-icons-material "ondemand_video")
                ""))
        speed
        current-hms
        total-hms
        duration
        remaining-hms
        bib-entry
        title)
    (if file-path
        ;; Title when mpv is playing media
        (progn
          (setq speed (mpv-get-property "speed"))
          (setq volume (mpv-get-property "volume"))
          (setq current-hms (org-media-note--get-current-hms))
          (setq duration (mpv-get-property "duration"))
          (setq total-hms (org-timer-secs-to-hms (round duration)))
          (setq remaining-hms (org-timer-secs-to-hms (round (mpv-get-property "playtime-remaining"))))
          (s-concat icon
                    " org-media-note: "
                    current-hms
                    " / "
                    total-hms
                    "\t Volume: "
                    (number-to-string volume)
                    "\t Speed: "
                    (number-to-string speed)
                    "\t Remaining: "
                    remaining-hms
                    "\n\t❯ "
                    (if (org-media-note-ref-cite-p)
                        (progn
                          (setq bib-entry (bibtex-completion-get-entry ref-key))
                          (setq title (bibtex-completion-get-value "title" bib-entry))
                          (format "%s (%s)" title ref-key))
                      (if (org-media-note--online-video-p file-path)
                          (format "%s (%s)"
                                  (mpv-get-property "media-title")
                                  file-path)
                        file-path))))
      ;; Title when no media is playing
      (concat icon " org-media-note"))))


(pretty-hydra-define org-media-note-hydra
  (:color red
   :title (org-media-note--hydra-title)
   :hint nil)
  ("File"
   (("o l" org-media-note-mpv-smart-play
     (if (org-media-note-ref-cite-p)
         (format "Open %s"
                 (org-media-note--current-org-ref-key))
       "Open local file")
     :width 20)
    ("o o" org-media-note-mpv-play-online-video
     "Open online file")
    ("j"
     (mpv-cycle-property "sub")
     "toggle subtitles")
    ("T"
     (mpv-cycle-property "ontop")
     "toggle ontop")
    ("c"
     (org-media-note-change-speed-by 0.1)
     "increase speed")
    ("x"
     (org-media-note-change-speed-by -0.1)
     "decrease speed")
    ("z" org-media-note-mpv-toggle-speed "reset speed"))
   "Playback"
   (("<SPC>" mpv-pause "Play/Pause")
    ("l"
     (mpv-run-command "ab-loop")
     (let ((time-a (mpv-get-property "ab-loop-a"))
           (time-b (mpv-get-property "ab-loop-b")))
       (if (org-media-note--ab-loop-p)
           (format "Clear A-B loop (%s - %s)"
                   (org-media-note--seconds-to-hms time-a)
                   (org-media-note--seconds-to-hms time-b))
         (if (numberp time-a)
             (format "Set B of A-B loop (%s - )"
                     (org-media-note--seconds-to-hms time-a))
           "Set A of A-B loop")))
     :width 35)
    ("<left>" mpv-seek-backward "Back 5s")
    ("<right>" mpv-seek-forward "Forward 5s")
    ("C-<left>"
     (mpv-run-command "sub-seek" -1)
     "Previous subtitle")
    ("C-<right>"
     (mpv-run-command "sub-seek" 1)
     "Next subtitle"))
   "Volume"
   (("+"
     (org-media-note-change-volume-by 5)
     "Up")
    ("-"
     (org-media-note-change-volume-by -5)
     "Down")
    ("0" org-media-note-mpv-toggle-volume "toggle")
    ("m"
     (mpv-cycle-property "mute")
     "(un)mute"))
   "Note"
   (("i" org-media-note-insert-link "Insert timestamp")
    ("S" org-media-note-insert-screenshot "Insert Screenshot")
    ("s" org-media-note-insert-sub-text "Insert subtitle")
    ("I p" org-media-note-insert-note-from-pbf
     "Import from pbf")
    ("I n" org-media-note-insert-note-from-noted
     "Import from Noted"))
   "Toggle"
   (("t m" org-media-note-mode "Auto insert media item"
     :toggle t)
    ("t c" org-media-note-toggle-refcite "Use ref key instead of absolute path"
     :toggle org-media-note-use-refcite-first)
    ("t p" org-media-note-toggle-pause-after-insertion
     "Pause media after insert link" :toggle org-media-note-pause-after-insert-link)
    ("t s" org-media-note-toggle-save-screenshot
     "Auto save screenshot" :toggle org-media-note-save-screenshot-p)
    ("t S" org-media-note-toggle-screenshot-with-sub
     "Screenshot with subtitles" :toggle org-media-note-screenshot-with-sub))))

;;;;; Utils

(defun org-media-note--millisecs-to-hms (millisecs)
  (org-timer-secs-to-hms (round (/ (string-to-number millisecs) 1000))))

(defun org-media-note--get-duration-hms ()
  "Get the current media duration in format h:mm:ss"
  (org-media-note--seconds-to-hms (mpv-get-duration)))

(defun org-media-note--get-current-hms ()
  "Get current media timestamp in format h:mm:ss"
  (org-media-note--seconds-to-hms (mpv-get-playback-position)))

(defun org-media-note--seconds-to-hms (secs)
  "Convert secs (float or int) to hms (string)"
  (org-timer-secs-to-hms (round secs))
  )

(defun org-media-note--current-org-ref-key ()
  (org-entry-get (point) "Custom_ID"))

(defun org-media-note-ref-cite-p ()
  "Whether to use refcite link instead of file path."
  (and (org-media-note--current-org-ref-key)
       org-media-note-use-refcite-first))


(defun org-media-note--online-video-p (path)
  (string-match "^http" path)
  )

(defun org-media-note--current-media-type ()
  "Get current playing media type."
  (let* ((file-path (mpv-get-property "path")))
    (if (org-media-note--online-video-p file-path)
        "video"  ;; TODO audio?
      (org-media-note--file-media-type file-path)
        )
    ))

(defun org-media-note--file-media-type (file-path)
  "Get file media type."
  (let* ((file-ext (if file-path
                       (file-name-extension file-path))))
    (org-media-note--get-media-type file-ext)))

(defun org-media-note--get-media-type (file-ext)
  (cond
   ((member file-ext org-media-note--video-types) "video")
   ((member file-ext org-media-note--audio-types) "audio")
   (t nil)))
;;;;; Add note
(defun org-media-note-insert-sub-text ()
  (interactive)
  (let ((sub-text (condition-case nil
                      (mpv-get-property "sub-text")
                    (error nil))))
    (if sub-text
        (insert sub-text)
      (message "No subtitles found in current file."))))
;;;;;; media note item
(defun org-insert-item--media-note-item (orig-fn &rest args)
  "When item begins with media link, insert playback position."
  (interactive "P")
  (let ((itemp (org-in-item-p))
        (pos (point)))
    ;; If cursor isn't is a list or if list is invisible, return nil.
    (unless (or (not itemp)
                (save-excursion
                  (goto-char itemp)
                  (org-invisible-p)))
      (if (save-excursion
            (goto-char itemp)
            (org-at-item-meida-item-p))
          (progn
            (org-media-note-item)
            t)))))


(defun org-media-note-item (&optional arg)
  "Insert a item with link to media file."
  (interactive "P")
  (let ((itemp (org-in-item-p))
        (pos (point)))
    (cond
     ;; In a media note list and media file is open: insert with `org-list-insert-item',
     ((and itemp
           (goto-char itemp)
           (org-at-item-meida-item-p)
           (mpv-get-property "path"))
      (let* ((struct (org-list-struct))
             (prevs (org-list-prevs-alist struct))
             (s (concat (org-media-note--link)
                        " ")))
        (setq struct (org-list-insert-item pos struct prevs nil
                                           s))
        (org-list-write-struct struct
                               (org-list-parents-alist struct))
        (looking-at org-list-full-item-re)
        (move-end-of-line 1)
        (if org-media-note-save-screenshot-p
            (org-media-note-insert-screenshot))
        (when org-media-note-pause-after-insert-link
          (mpv-pause))))
     ;; In a list of another type, don't break anything: throw an error.
     (t (error (concat "No playing media file now. Please open the media file"
                       "first if you want to insert media note,"
                       "\nor turn off "))))))

(defun org-media-note-insert-link ()
  "Insert current mpv timestamp link into Org-mode note."
  (interactive)
  (let ((point (point)))
    (insert
     org-media-note-link-prefix
     (format "%s "
             (org-media-note--link)))
    (when (eq org-media-note-cursor-start-position 'before)
      (goto-char point))
    (when org-media-note-pause-after-insert-link
      (mpv-pause))
    ))

(defun org-media-note--link-formatter (string map)
  "MAP is an alist in the form of '((PLACEHOLDER . REPLACEMENT))
STRING is the original string.  Each placeholder can be a string, 
symbol, or number. REPLACEMENT can be a string, a number, symbol, 
or function. Replace all occurrences of %placeholder with replacement
and return a new string.

For example:
(let ((input-string  \"Words,  %test1%test2 more words %test1.\")
      (map '((test1 . \"asdf\")
             (test2 . \"zxcv\"))))
  (org-media-note--link-formatter input-string map))

Returns:
\"Words,  asdfzxcv more words asdf.\""
  (cl-loop for (holder . replacement) in map
	   when replacement
	   do (setq string
		    (replace-regexp-in-string
		     (concat "%"
			     (pcase holder
			       ((pred symbolp) (symbol-name holder))
			       ((pred stringp) holder)
			       ((pred numberp) (number-to-string holder))
			       ((pred functionp) (funcall replacement))))
		     (pcase replacement
		       ((pred symbolp) (symbol-name holder))
		       ((pred stringp) replacement)
		       ((pred numberp) (number-to-string replacement))
		       ((pred functionp) (funcall replacement))
		       (_ ""))
		     string))
	   finally return string))

(defun org-media-note--ab-loop-p ()
  "Whether in ab-loop?"
  (let ((time-a (mpv-get-property "ab-loop-a"))
        (time-b (mpv-get-property "ab-loop-b"))
        (pos (mpv-get-playback-position))
        )
    (and (numberp time-a)
         (numberp time-b)
         (<= time-a pos)
         (<= pos time-b)
         )))

(defun org-media-note--link ()
  "Return media link."
  (let* ((file-path (mpv-get-property "path"))
         (link-type (if (org-media-note-ref-cite-p)
                        (concat (org-media-note--current-media-type)
                                "cite")
                      (org-media-note--current-media-type)))
         (filename (mpv-get-property "media-title"))
         (duration (org-media-note--get-duration-hms))
         (timestamp (org-media-note--get-current-hms)))
    (if (org-media-note--ab-loop-p)
        ;; ab-loop link
        (let ((time-a (org-media-note--seconds-to-hms (mpv-get-property "ab-loop-a")))
              (time-b (org-media-note--seconds-to-hms (mpv-get-property "ab-loop-b"))))
          (format "[[%s:%s#%s-%s][%s]]"
                  link-type
                  (if (org-media-note-ref-cite-p)
                      (org-media-note--current-org-ref-key)
                    (if (org-media-note--online-video-p file-path)
                        file-path
                      (org-media-note--format-file-path file-path)))
                  time-a
                  time-b
                  (org-media-note--link-formatter org-media-note-ab-loop-link-format
                                                  `(("filename" . ,filename)
                                                    ("duration" . ,duration)
                                                    ("ab-loop-a" . ,time-a)
                                                    ("ab-loop-b" . ,time-b)
                                                    ("file-path" . ,file-path)))))
      ;; timestamp link
      (format "[[%s:%s#%s][%s]]"
              link-type
              (if (org-media-note-ref-cite-p)
                  (org-media-note--current-org-ref-key)
                (if (org-media-note--online-video-p file-path)
                    file-path
                  (org-media-note--format-file-path file-path)))
              timestamp
              (org-media-note--link-formatter org-media-note-timestamp-link-format
                                              `(("filename" . ,filename)
                                                ("duration" . ,duration)
                                                ("timestamp" . ,timestamp)
                                                ("file-path" . ,file-path)))))))

(defun org-at-item-meida-item-p ()
  "Is point at a line starting a plain list item with a media-note link?"
  (or (org-list-at-regexp-after-bullet-p "\\(\\[\\[video.+\\)")
      (org-list-at-regexp-after-bullet-p "\\(\\[\\[audio.+\\)")))

;;;;;; screenshot
(defun org-media-note--format-file-name (name)
  (let (new-name)
    (setq new-name (replace-regexp-in-string " - " "-" name))
    (setq new-name (replace-regexp-in-string ":" "_" name))
    (replace-regexp-in-string " " "_" new-name)))

(defun org-media-note-insert-screenshot ()
  "Insert current mpv screenshot into Org-mode note."
  (interactive)
  (let* ((image-file-name (org-media-note--format-file-name (concat (file-name-base (mpv-get-property "path"))
                                                                    "-"
                                                                    (org-media-note--get-current-hms)
                                                                    ".jpg")))  ;; TODO let user customize this
         (image-target-path (cond
                             ((eq org-media-note-screenshot-save-method
                                  'attach)
                              (expand-file-name image-file-name
                                                (org-attach-dir t)))
                             ((eq org-media-note-screenshot-save-method
                                  'directory)
                              (expand-file-name image-file-name org-media-note-screenshot-image-dir)))))
    (if org-media-note-screenshot-with-sub
        (mpv-run-command "screenshot-to-file" image-target-path)
      (mpv-run-command "screenshot-to-file" image-target-path
                       "video"))
    (if (and (eq org-media-note-screenshot-save-method
                 'attach)
             (eq org-media-note-screenshot-link-type-when-save-in-attach-dir
                 'attach))
        (insert (format "[[attachment:%s]] "
                        (file-relative-name image-target-path
                                            (org-attach-dir))))
      (insert (format "[[file:%s]] "
                      (org-media-note--format-file-path image-target-path)
                      )))
    (org-media-note--display-inline-images)))

(defun org-media-note--format-file-path (path)
  "Convert PATH into the format defined by `org-link-file-path-type'"
  (cond
   ((eq org-link-file-path-type 'absolute)
    (abbreviate-file-name (expand-file-name path)))
   ((eq org-link-file-path-type 'noabbrev)
    (expand-file-name path))
   ((eq org-link-file-path-type 'relative)
    (file-relative-name path))
   ((eq org-link-file-path-type 'adaptive)
    (save-match-data (if (string-match (concat "^"
                                               (regexp-quote (expand-file-name (file-name-as-directory default-directory))))
                                       (expand-file-name path))
                         ;; We are linking a file with relative path name.
                         (substring (expand-file-name path)
                                    (match-end 0))
                       (abbreviate-file-name (expand-file-name path)))))
   ((functionp org-link-file-path-type)
    (funcall org-link-file-path-type
             (expand-file-name path)))))

(defun org-media-note--display-inline-images ()
  (cond
   ((eq org-media-note-display-inline-images t)
    ;; TODO beteer way?
    (sleep-for 0.1)
    (org-display-inline-images))))

;;;;; Link Follow
(defun org-media-note-link-follow (link)
  "Open video and audio links, supported formats:
1. video:example.mkv#0:02:13: jump to 0:02:13
2. video:example.mkv#0:02:13-0:02:20: jump to 0:02:13 and loop between 0:02:13 and 0:02:20
"
  (let* ((splitted (split-string link "#"))
         (file-path (nth 0 splitted))
         (timestamps (split-string (nth 1 splitted)
                                   "-"))
         (time-a (int-to-string (org-timer-hms-to-secs (nth 0 timestamps))))
         (time-b (if (= (length timestamps) 2)
                     (int-to-string (org-timer-hms-to-secs (nth 1 timestamps))))))
    (org-media-note--follow-link file-path time-a
                                 time-b)))

(defun org-media-note--follow-link (file-path time-a time-b)
  (let ((file-path (if (org-media-note--online-video-p file-path)
                       file-path
                     (expand-file-name file-path))))
    (if (not (string= file-path
                      (mpv-get-property "path")))
        ;; file-path is not playing
        (if time-b
            (mpv-start file-path
                       (concat "--start=+" time-a)
                       (concat "--ab-loop-a=" time-a)
                       (concat "--ab-loop-b=" time-b))
          (mpv-start file-path
                     (concat "--start=+" time-a)))
      ;; file-path is playing
      ;; TODO clear a-b loop when only one timestamp?
      (progn
        (when time-b
          (mpv-set-property "ab-loop-a" time-a)
          (mpv-set-property "ab-loop-b" time-b))
        (mpv-seek time-a)))))

;;;;; Media Control
(defun org-media-note-change-speed-by (speed-step)
  "Set playing speed."
  (let ((current-speed (mpv-get-property "speed")))
    (mpv-speed-set (+ current-speed speed-step))))

(defun org-media-note-mpv-toggle-speed ()
  (interactive)
  (let ((current-speed (mpv-get-property "speed")))
    (if (= 1.0 current-speed)
        (mpv-speed-set org-media-note-last-play-speed)
      (progn
        (setq org-media-note-last-play-speed current-speed)
        (mpv-speed-set 1)))))

(defun org-media-note-change-volume-by (step)
  "Set playing speed."
  (mpv-run-command "add" "volume" step)
  (setq org-media-note-last-volume (mpv-get-property "volume")))

(defun org-media-note-mpv-toggle-volume ()
  (interactive)
  (let ((current-volume (mpv-get-property "volume")))
    (if (= 100.0 current-volume)
        (mpv-set-property "volume" org-media-note-last-volume)
      (progn
        (setq org-media-note-last-volume current-volume)
        (mpv-set-property "volume" 100)))))

(defun org-media-note-mpv-smart-play ()
  "Open local media file in mpv:
1. When there's just one media file in attach dir, or found media file by key, play this file im mpv;
2. When there're multiple media files in attach dir, open the attach dir to select;
3. Else, open the current dir of note file to selet"
  (interactive)
  (let* ((key (org-media-note--current-org-ref-key))
         (attach-dir (if (org-attach-dir)
                         (format "%s/"
                                 (org-attach-dir))))
         (media-files-in-attach-dir (org-media-note--media-files-in-dir attach-dir))
         (number-of-media-files (length media-files-in-attach-dir)))
    (if (org-media-note-ref-cite-p)
        (mpv-play (org-media-note-get-media-file-by-key key))
      (if media-files-in-attach-dir
          (if (= 1 number-of-media-files)
              (mpv-play (car media-files-in-attach-dir))
            (mpv-play (read-file-name "File to play: " attach-dir)))
        (mpv-play (read-file-name "File to play: "))))))

(defun org-media-note-mpv-play-online-video ()
  "Open online media file in mpv."
  (interactive)
  (let ((video-url (read-string "Url to play: ")))
    (if (org-media-note--online-video-p video-url)
        (mpv-start video-url)
      (error (format "'%s' is not a valid url!" video-url))
        )
    )
  )


(defun org-media-note--media-files-in-dir (dir)
  "Get supported media file list in dir."
  (if dir
      (directory-files dir
                       'full
                       (rx (eval (cons 'or (append org-media-note--video-types org-media-note--audio-types)))
                           eos))
    nil))


;;;;; Import From other apps

(defun org-media-note-insert-note-from-pbf ()
  (interactive)
  (let ((key (org-media-note--current-org-ref-key)) pbf-file
        media-link-type
        media-file)
    (if (org-media-note-ref-cite-p)
        (progn
          (setq source-media (org-media-note-get-media-file-by-key key))
          (setq media-link-type (format "%scite"
                                        (org-media-note--file-media-type source-media)))
          (setq media-file key)
          (setq pbf-file (concat (file-name-sans-extension source-media)
                                 ".pbf")))
      (progn
        ;; TODO need more test
        (setq media-file (read-file-name "Find media file:"))
        (setq media-link-type (org-media-note--file-media-type media-file))
        (setq pbf-file (concat (file-name-sans-extension media-file)
                               ".pbf"))))
    (message pbf-file)
    (if (not (file-exists-p pbf-file))
        ;; TODO need more test
        (setq pbf-file (read-file-name "Find pbf file:")))
    (insert (org-media-note--convert-from-pbf pbf-file
                                              media-link-type media-file))
    (if (y-or-n-p "Delete the PBF File?")
        (delete-file pbf-file))))

(defun org-media-note-insert-note-from-noted ()
  (interactive)
  (let ((key (org-media-note--current-org-ref-key)) noted-txt
        media-link-type
        media-file)
    (setq noted-txt (read-file-name "Find exported Noted txt:"))
    (if (org-media-note-ref-cite-p)
        (progn
          (setq media-file key)
          (setq media-link-type (concat (org-media-note--file-media-type (org-media-note-get-media-file-by-key key))
                                        "cite")))
      (progn
        ;; TODO  need more test
        (setq media-file (read-file-name "Find media file:"))
        (setq media-link-type (org-media-note--file-media-type media-file))))
    (insert (org-media-note--convert-from-noted noted-txt
                                                media-link-type media-file))
    (if (y-or-n-p "Delete Noted txt?")
        (delete-file noted-txt))))

(defun org-media-note--convert-from-noted (noted-file media-link-type media-file)
  ;; (with-current-buffer (get-buffer-create "*RESULTS*")
  (with-temp-buffer
    (insert-file-contents noted-file)
    (replace-string "￼"
                    ""
                    nil
                    (point-min)
                    (point-max))
    ;; replace unordered list
    (replace-regexp "[•◦▫]"
                    "-"
                    nil
                    (point-min)
                    (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)-[[:blank:]]+"
                              nil t)
      (let* ((hms (buffer-substring (match-beginning 1)
                                    (match-end 1)))
             (blank-indent (buffer-substring (match-beginning 2)
                                             (match-end 2))))
        (replace-match (concat blank-indent "- [[" media-link-type
                               ":" media-file "#" hms "][" hms "]] ")
                       t)))
    ;; replace ordered list

    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)\\([[:digit:]]\\. \\)"
                              nil t)
      (let* ((hms (buffer-substring (match-beginning 1)
                                    (match-end 1)))
             (blank-indent (buffer-substring (match-beginning 2)
                                             (match-end 2)))
             (number-bullet (buffer-substring (match-beginning 3)
                                              (match-end 3))))
        (replace-match (concat blank-indent number-bullet "[[" media-link-type
                               ":" media-file "#" hms "][" hms "]] ")
                       t)))
    ;; replace timestamped text

    (goto-char (point-min))
    (while (re-search-forward "^\\[\\([:0-9]+\\)\\]\\([[:blank:]]+\\)"
                              nil t)
      (let* ((hms (buffer-substring (match-beginning 1)
                                    (match-end 1)))
             (blank-indent (buffer-substring (match-beginning 2)
                                             (match-end 2))))
        (replace-match (concat blank-indent "- [[" media-link-type
                               ":" media-file "#" hms "][" hms "]] ")
                       t)))
    ;; format

    (replace-regexp "\\]\\] +"
                    "]] "
                    nil
                    (point-min)
                    (point-max))
    (buffer-string)))

(defun org-media-note--convert-from-pbf (pbf-file media-link-type media-file)
  ;; FOR DEBUG
  ;; (with-current-buffer (get-buffer-create "*RESULTS*")
  (with-temp-buffer
    (insert-file-contents pbf-file)
    (replace-string "[Bookmark]\n"
                    ""
                    nil
                    (point-min)
                    (point-max))
    (replace-regexp "^[[:digit:]]+=$"
                    ""
                    nil
                    (point-min)
                    (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^[[:digit:]]+=\\([[:digit:]]+\\)\\*\\([^\\*]+\\)\\*.+"
                              nil t)
      (let* ((millisecs (buffer-substring (match-beginning 1)
                                          (match-end 1)))
             (note (buffer-substring (match-beginning 2)
                                     (match-end 2)))
             (hms (org-media-note--millisecs-to-hms millisecs)))
        (replace-match (format "- [[%s:%s#%s][%s]] %s" media-link-type
                               media-file hms hms note)
                       t)))
    (buffer-string)))

;;;;; Customize Org link
(org-link-set-parameters "video"
                         :follow 'org-media-note-link-follow)

(org-link-set-parameters "audio"
                         :follow 'org-media-note-link-follow)

;;;;; Minor Mode

;;;###autoload
(define-minor-mode org-media-note-mode
  "Toggle `org-media-note-mode'.
When enabled, insert media note.
"
  :init-value t
  :global t
  (if org-media-note-mode
      (advice-add 'org-insert-item :before-until #'org-insert-item--media-note-item)
    (advice-remove 'org-insert-item #'org-insert-item--media-note-item)))


(defun org-media-note-toggle-refcite ()
  (interactive)
  (if org-media-note-use-refcite-first
      (setq org-media-note-use-refcite-first nil)
    (setq org-media-note-use-refcite-first t)))

(defun org-media-note-toggle-pause-after-insertion ()
  (interactive)
  (if org-media-note-pause-after-insert-link
      (setq org-media-note-pause-after-insert-link nil)
    (setq org-media-note-pause-after-insert-link t)))


(defun org-media-note-toggle-save-screenshot ()
  (interactive)
  (if org-media-note-save-screenshot-p
      (setq org-media-note-save-screenshot-p nil)
    (setq org-media-note-save-screenshot-p t)))

(defun org-media-note-toggle-screenshot-with-sub ()
  (interactive)
  (if org-media-note-screenshot-with-sub
      (setq org-media-note-screenshot-with-sub nil)
    (setq org-media-note-screenshot-with-sub t)))

;;;; Footer
(provide 'org-media-note)
;;; org-media-note.el ends here
