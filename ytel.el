;;; ytel.el --- Query YouTube via Invidious -*- lexical-binding: t; -*-

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Author: Gabriele Rastello
;;      Stefan Huchler
;; Version: 0.1.0
;; Keywords: youtube matching multimedia
;; URL: https://github.com/grastello/ytel
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:

;; This package provide a major mode to search YouTube videos via an elfeed-like
;; buffer.  Information about videos displayed in this buffer can be extracted
;; and manipulated by user-defined functions to do various things such as:
;; - playing them in some video player
;; - download them
;; The limit is the sky.
;;
;; ytel works by querying YouTube via the Invidious apis (learn more on that here:
;; https://github.com/omarroth/invidious).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'ring)

(defgroup ytel ()
  "An Emacs Youtube \"front-end\"."
  :group 'comm)

(defvar ytel-sort-options (ring-convert-sequence-to-ring
			   '(relevance rating upload_date view_count))
  "Availible sort options.")

(defvar ytel-date-options (ring-convert-sequence-to-ring '(hour today week month year))
  "Availible date options.")

(defvar ytel-invidious-api-url "https://invidio.us"
  "Url to an Invidious instance.")

(defvar ytel-invidious-default-query-fields "author,lengthSeconds,title,videoId,authorId,viewCount,published"
  "Default fields of interest for video search.")

(defvar ytel-videos '()
  "List of videos currently on display.")

(defvar ytel-published-date-time-string "%Y-%m-%d"
  "Time-string used to render the published date of the video.
See `format-time-string' for information on how to edit this variable.")

(defvar-local ytel-sort-criterion 'relevance
  "Criterion to date limit the results of the search query.")

(defvar-local ytel-date-criterion 'year
  "Criterion to date limit the results of the search query.")

(defvar-local ytel-current-page 1
  "Current page of the current `ytel-search-term'")

(defvar-local ytel-search-term ""
  "Current search string as used by `ytel-search'")

(defvar ytel-author-name-reserved-space 20
  "Number of characters reserved for the channel's name in the *ytel* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long).")

(defvar ytel-title-video-reserved-space 100
  "Number of characters reserved for the video title in the *ytel* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long).")

(defface ytel-video-published-face
  '((((class color) (background light)) (:foreground "#a0a"))
    (((class color) (background dark))  (:foreground "#7a7")))
  "Face used for the video published date.")

(defface ytel-channel-name-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used for channel names.")

(defface ytel-video-length-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for the video length.")

(defface ytel-video-view-face
  '((((class color) (background light)) (:foreground "#00a"))
    (((class color) (background dark))  (:foreground "#aa7")))
  "Face used for the video views.")

(defvar ytel-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'ytel-quit)
    (define-key map "h" #'describe-mode)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "d" #'ytel-rotate-date)
    (define-key map "D" #'ytel-rotate-date-backwards)
    (define-key map "r" #'ytel-rotate-sort)
    (define-key map "R" #'ytel-rotate-sort-backwards)
    (define-key map "s" #'ytel-search)
    (define-key map ">" #'ytel-search-next-page)
    (define-key map "<" #'ytel-search-previous-page)
    map)
  "Keymap for `ytel-mode'.")

(define-derived-mode ytel-mode tabulated-list-mode "ytel"
  "Major Mode for ytel.
Key bindings:
\\{ytel-mode-map}"
  (setq-local revert-buffer-function #'ytel--draw-buffer))

(defun ytel-quit ()
  "Quit ytel buffer."
  (interactive)
  (quit-window))

(defun ytel--format-author (name)
  "Format a channel NAME to be inserted in the *ytel* buffer."
  (propertize name 'face 'ytel-channel-name-face))

(defun ytel--format-video-length (seconds)
  "Given an amount of SECONDS, format it nicely to be inserted in the *ytel* buffer."
  (let ((formatted-string (concat (format-seconds "%.2h" seconds)
				  ":"
				  (format-seconds "%.2m" (mod seconds 3600))
				  ":"
				  (format-seconds "%.2s" (mod seconds 60)))))
    (propertize formatted-string 'face 'ytel-video-length-face)))

(defun ytel--format-video-views (views)
  "Format video VIEWS to be inserted in the *ytel* buffer."
  (propertize (number-to-string views) 'face 'ytel-video-view-face))

(defun ytel--format-video-published (published)
  "Format video PUBLISHED date to be inserted in the *ytel* buffer."
  (propertize (format-time-string ytel-published-date-time-string (seconds-to-time published))
	      'face 'ytel-video-published-face))

(defun ytel--create-entry (video)
  "Creates tabulated-list VIDEO entry"
  (list (assoc-default 'videoId video)
	(vector (ytel--format-video-published (assoc-default 'published video))
		(ytel--format-author (assoc-default 'author video))
		(ytel--format-video-length (assoc-default 'lengthSeconds video))
		(assoc-default 'title video)
		(ytel--format-video-views (assoc-default 'viewCount video)))))

(defun ytel--draw-buffer (&optional _arg _noconfirm)
  "Draw a list of videos.
Optional argument _ARG revert expects this param.
Optional argument _NOCONFIRM revert expects this param."
  (interactive)
  (let* ((search-string (propertize ytel-search-term 'face 'ytel-video-published-face))
	 (page-number (propertize (number-to-string ytel-current-page)
				  'face 'ytel-video-published-face))
	 (date-limit (propertize (symbol-name ytel-date-criterion)
				 'face 'ytel-video-published-face))
	 (sort-strings '(upload_date "date" view_count "views"
				     rating "rating" relevance "relevance"))
	 (sort-limit (propertize (plist-get sort-strings ytel-sort-criterion)
				  'face 'ytel-video-published-face)))
    (setq tabulated-list-format `[("Date" 10 t)
				  ("Author" ,ytel-author-name-reserved-space t)
				  ("Length" 8 t) ("Title"  ,ytel-title-video-reserved-space t)
				  ("Views" 10 t . (:right-align t))])
    (setf ytel-videos (ytel--query ytel-search-term ytel-current-page))
    (rename-buffer (format "ytel: %s" search-string))
    (setq-local mode-line-misc-info `(("page:" ,page-number)
				      (" date:" ,date-limit)
				      (" sort:" ,sort-limit)))
    (setq tabulated-list-entries (mapcar 'ytel--create-entry ytel-videos))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun ytel--query (string n)
  "Query youtube for STRING, return the Nth page of results."
  (let ((videos (ytel--API-call "search" `(("q" ,string)
					   ("date" ,(symbol-name ytel-date-criterion))
                                           ("sort_by" ,(symbol-name ytel-sort-criterion))
					   ("page" ,n)
					   ("fields" ,ytel-invidious-default-query-fields)))))
    videos))

(defun ytel-search (query)
  "Search youtube for `QUERY', and redraw the buffer."
  (interactive "sSearch terms: ")
  (setf ytel-current-page 1)
  (let* ((query-words (split-string query))
	 (terms (seq-group-by (lambda (elem)
				(s-contains-p ":" elem))
			      query-words)))
    (setf ytel-search-term
	  (s-join " " (assoc-default nil terms)))
    (if-let ((date (seq-find
		    (lambda (s) (s-starts-with-p "date:" s) )
		    (assoc-default t terms))))
	(setf ytel-date-criterion (intern (substring date 5)))
      (setf ytel-date-criterion 'year)))
  (ytel--draw-buffer))

(defun ytel-rotate-sort (&optional reverse)
  "Rotates through sort criteria.
Optional argument REVERSE reverses the direction of the rotation."
  (interactive)
  (let* ((circle (if reverse 'ring-previous 'ring-next)))
    (setf ytel-sort-criterion
	  (funcall circle ytel-sort-options ytel-sort-criterion)))
  (ytel--draw-buffer))

(defun ytel-rotate-sort-backwards ()
  "Rotates through sorting backwards."
  (interactive)
  (ytel-rotate-sort t))

(defun ytel-rotate-date (&optional reverse)
  "Rotates through date limit.
Optional argument REVERSE reverses the direction of the rotation."
  (interactive)
  (let* ((circle (if reverse 'ring-previous 'ring-next)))
    (setf ytel-date-criterion
	  (funcall circle ytel-date-options ytel-date-criterion)))
  (ytel--draw-buffer))

(defun ytel-rotate-date-backwards ()
  "Rotates through date limit backwards."
  (interactive)
  (ytel-rotate-date t))

(defun ytel-region-search ()
  "Search youtube for marked region."
  (interactive)
  (let* ((query
	  (buffer-substring-no-properties
	   (region-beginning)
	   (region-end)))
	 (ytel-search-term query))
    (ytel)
    (ytel-search query)))

(defun ytel-search-next-page ()
  "Switch to the next page of the current search.  Redraw the buffer."
  (interactive)
  (setf ytel-current-page (1+ ytel-current-page))
  (ytel--draw-buffer))

(defun ytel-search-previous-page ()
  "Switch to the previous page of the current search.  Redraw the buffer."
  (interactive)
  (when (> ytel-current-page 1)
    (setf ytel-current-page (1- ytel-current-page))
    (ytel--draw-buffer)))

(defun ytel-get-current-video ()
  "Get the currently selected video."
  (aref ytel-videos (1- (line-number-at-pos))))

(defun ytel-buffer ()
  "Name for the main ytel buffer."
  (get-buffer-create "*ytel*"))

;;;###autoload
(defun ytel ()
  "Enter ytel."
  (interactive)
  (switch-to-buffer (ytel-buffer))
  (unless (eq major-mode 'ytel-mode)
    (ytel-mode))
  (when (seq-empty-p ytel-search-term)
    (call-interactively #'ytel-search)))

(defun ytel-video-id-fun (video)
  "Return VIDEO id."
  (assoc-default 'videoId video))

(defun ytel--API-call (method args)
  "Perform a call to the invidious API method METHOD passing ARGS.

Curl is used to perform the request.  An error is thrown if it exits with a non
zero exit code otherwise the request body is parsed by `json-read' and returned."
  (with-temp-buffer
    (let ((exit-code (call-process "curl" nil t nil
				   "--silent"
				   "-X" "GET"
				   (concat ytel-invidious-api-url
					   "/api/v1/" method
					   "?" (url-build-query-string args)))))
      (unless (= exit-code 0)
	(error "Curl had problems connecting to Invidious"))
      (goto-char (point-min))
      (json-read))))

(provide 'ytel)

;;; ytel.el ends here
