;;; org-media-note-cite.el --- Integrate org-media-note with reference -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Package-Requires: ((emacs "27.1") (bibtex-completion "0"))


;;; Commentary:

;;; Code:
;;;; Requirements
(require 'seq)
(require 'bibtex-completion)

(require 'org-media-note-core)

;;;; Customization
(defcustom org-media-note-cite-bibliography bibtex-files
  "List of BibTeX files that are searched for entry keys."
  :type '(repeat (choice (const :tag "bibtex-file-path" bibtex-file-path)
                         directory file)))

(defcustom org-media-note-cite-format-fn #'org-media-note-cite-format-entry
  "Function to format citation entries."
  :type 'function)

;;;; Commands
;;;;; Help echo
(defun org-media-note-cite--help-echo (_window _object position)
  "Return help-echo for ref link at POSITION."
  (save-excursion
    (goto-char position)
    (let ((s (org-media-note-cite-link-message)))
      (with-temp-buffer
        (insert s)
        (fill-paragraph)
        (buffer-string)))))

(defun org-media-note-cite-link-message ()
  "Print a minibuffer message about the link that point is on."
  (interactive)
  ;; the way links are recognized in org-element-context counts blank spaces
  ;; after a link and the closing brackets in literal links. We don't try to get
  ;; a message if the cursor is on those, or if it is on a blank line.
  (when (not (or (looking-at " ") ;looking at a space
                 (looking-at "^$") ;looking at a blank line
                 (looking-at "]") ;looking at a bracket at the end
                 (looking-at "$"))) ;looking at the end of the line.
    (save-restriction (widen)
                      (when (eq major-mode 'org-mode)
                        (let* ((object (org-element-context))
                               (type (org-element-property :type object)))
                          (save-excursion
                            (cond
                             ((or (string= type "videocite")
                                  (string= type "audiocite"))
                              (let* ((media-note-link (org-element-property :path object))
                                     (ref-cite-key (car (split-string media-note-link "#")))
                                     (hms (cdr (split-string media-note-link "#"))))
                                (format "%s @ %s"
					(funcall org-media-note-cite-format-fn ref-cite-key)
                                        hms))))))))))


(defun org-media-note-cite-format-entry (key)
  "Returns a formatted bibtex entry for KEY."
  (let ((entry (ignore-errors (bibtex-completion-get-entry key))))
    (if (null entry)
        "!!! No entry found !!!"
      (let* ((series (bibtex-completion-get-value "series" entry))
	     (title (bibtex-completion-get-value "title" entry))
             )
	(if series
	    (format "%s: %s" series title)
	  title)))))

(defun org-media-note-cite-display-message-in-eldoc (&rest _)
  "Display media's cite link message when `eldoc' enabled."
  (org-media-note-cite-link-message))


;;;;; Link Follow
(defun org-media-note-cite--open (link)
  "Open videocite and audiocite LINKs, supported formats:
1. videocite:course.104#0:02:13: jump to 0:02:13
2. videocite:course.104#0:02:13-0:02:20: jump to 0:02:13 and loop between 0:02:13 and 0:02:20"
  (cl-multiple-value-bind (key time-a time-b)
      (org-media-note--split-link link)
    (let ((path (org-media-note-cite--path key)))
      (cond
       ((not path)
        (error "Cannot find media file for this Key"))
       (t (org-media-note--follow-link path time-a time-b))))))

(defun org-media-note-cite--path (key)
  "Get media file or URL by KEY."
  (or (org-media-note-cite--file-path key)
      (org-media-note-cite--url key)))

(defun org-media-note-cite--file-path (key)
  "Get media file by KEY."
  (let* ((files (bibtex-completion-find-pdf key))
	 (video-files (org-media-note--filter-by-extensions files org-media-note--video-types))
	 (audio-files (org-media-note--filter-by-extensions files org-media-note--audio-types)))
    (cond
     ;; TODO when multiple media files?
     (video-files (file-truename (nth 0 video-files)))
     (audio-files (file-truename (nth 0 audio-files)))
     (t nil))))

(defun org-media-note--filter-by-extensions (file-list extensions)
    "Filter files from FILE-LIST whose extensions belong to EXTENSIONS."
    (let ((result '())
          (ext-list (if (stringp extensions)
                        (list extensions)
                      extensions)))
      (dolist (file file-list)
	(let ((raw-ext (file-name-extension file)))
	  (when (stringp raw-ext)
	    (let ((ext (downcase raw-ext)))
	      (when (member ext ext-list)
		(push file result))))))
      (nreverse result)))

(defun org-media-note-cite--url (key)
  "Get URL by KEY."
  (if key
      (let ((entry (bibtex-completion-get-entry1 key t)))
        (bibtex-completion-get-value "url" entry))))
;;;;; Setup

;;;###autoload
(defun org-media-note-cite-setup ()
  "Set org link parameters for video/audiocite links."
  (dolist (link '("videocite" "audiocite"))
    (org-link-set-parameters link
                             :follow 'org-media-note-cite--open
                             :help-echo #'org-media-note-cite--help-echo))

  ;; Display media link description in minibuffer when cursor is over it.
  (advice-add #'org-eldoc-documentation-function
              :before-until #'org-media-note-cite-display-message-in-eldoc))

;;;; Footer
(provide 'org-media-note-cite)
;;; org-media-note-cite.el ends here
