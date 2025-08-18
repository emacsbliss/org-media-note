;;; org-media-note-cite.el --- Integrate org-media-note with reference -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note


;;; Commentary:

;;; Code:
;;;; Requirements
(require 'seq)

(require 'org-media-note-core)

;;;; Customization
(defcustom org-media-note-cite-bibliography bibtex-files
  "List of BibTeX files that are searched for entry keys."
  :type '(repeat (choice (const :tag "bibtex-file-path" bibtex-file-path)
                         directory file)))

(defcustom org-media-note-cite-format-fn #'org-media-note-cite-format-entry
  "Function to format citation entries."
  :type 'function)

(defcustom org-media-note-cite-priority 'file-first
  "Priority strategy when both local files and URLs are available for a cite key.
- 'file-first: Prefer local files over URLs (default)
- 'url-first: Prefer URLs over local files  
- 'ask: Always ask user to choose"
  :type '(choice (const :tag "File first" file-first)
                 (const :tag "URL first" url-first)
                 (const :tag "Always ask" ask)))

;;;; Commands
;;;;; bib handling
(defun org-media-note-cite-get-entry (key)
  "Get the value of an entry's FIELD with KEY.  Return string."
  (cond
   ((fboundp 'citar-get-entry)
    (citar-get-entry key))
   ((fboundp 'bibtex-completion-get-entry)
    (bibtex-completion-get-entry key)
   (t (message "Either citar or bibtex-completion should be installed!")))))

(defun org-media-note-cite-get-value (key field)
  "Get the value of an entry's FIELD with KEY.  Return string."
  (cond
   ((fboundp 'citar-get-value)
    (citar-get-value field key))
   ((fboundp 'bibtex-completion-get-value)
    (bibtex-completion-get-value field
				 (org-media-note-cite-get-entry key))
   (t (message "Either citar or bibtex-completion should be installed!")))))

(defun org-media-note-cite-get-files (key)
  "Get the value of an entry's FIELD with KEY.  Return string."
  (cond
   ((fboundp 'citar-get-files)
    (gethash key (citar-get-files key)))
   ((fboundp 'bibtex-completion-find-pdf-in-field)
    (bibtex-completion-find-pdf-in-field key)
   (t (message "Either citar or bibtex-completion should be installed!")))))


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
  (let ((entry (ignore-errors (org-media-note-cite-get-entry key))))
    (if (null entry)
        "!!! No entry found !!!"
      (let* ((series (org-media-note-cite-get-value key "series"))
	     (title (org-media-note-cite-get-value key "title"))
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
  "Get media file or URL by KEY according to `org-media-note-cite-priority'."
  (when key
    (let ((file-path (org-media-note-cite--file-path key))
          (url-path (org-media-note-cite--url key)))
      (cond
       ;; No files or URLs found
       ((and (not file-path) (not url-path)) nil)
       
       ;; Only one option available
       ((and file-path (not url-path)) file-path)
       ((and url-path (not file-path)) url-path)
       
       ;; Both available - apply priority strategy
       ((eq org-media-note-cite-priority 'file-first)
        (or file-path url-path))
       
       ((eq org-media-note-cite-priority 'url-first)
        (or url-path file-path))
       
       ((eq org-media-note-cite-priority 'ask)
        (let* ((choices (list (format "Local file: %s" (file-name-nondirectory file-path))
                              (format "URL: %s" url-path)))
               (choice (org-media-note--select 
                        (format "Multiple media sources found for key '%s':" key)
                        choices)))
          (if (string-prefix-p "Local file:" choice) 
              file-path 
            url-path)))
       
       ;; Fallback to file-first behavior
       (t (or file-path url-path))))))

(defun org-media-note-cite--file-path (key)
  "Get media file by KEY."
  (let* ((files (org-media-note-cite-get-files key))
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
      (org-media-note-cite-get-value key "url")))

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
