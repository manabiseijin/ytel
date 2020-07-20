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

(declare-function ytel-channel 'ytel-channel)
(declare-function ytel--get-playlist-videos 'ytel-playlist)


(defgroup ytel ()
  "An Emacs Youtube \"front-end\"."
  :group 'comm)

(defcustom ytel-sort-criterion 'relevance
  "Criterion to sort the results of the search query."
  :type 'symbol
  :options '(relevance rating upload_date view_count)
  :group 'ytel)

(defcustom ytel-type-of-results "video"
  "Set what type of results to get when making a search."
  :type 'string
  :options '("video" "playlist" "channel" "all")
  :group 'ytel)

(defcustom ytel-show-fancy-icons nil
  "If t, enable showing fancy icons in the search buffer."
  :type 'boolean
  :group 'ytel)

;; TODO: Try to add support using all-the-icons, or add images instead.
(defcustom ytel-icons '((video "Video" "✇")
			(playlist "Playlist" "🎞")
			(channel "Channel" "📺 ") ;; Added a space to this icon so everything is aligned
			(length "" "⌚:")
			(views "views" "👁")
			(subCount "subscribers" "🅯")
			(videoCount "videos" "▶"))
  "Icons for displaying items in buffer.  First string is inserted if `ytel-show-fancy-icons' is disabled."
  :type '(alist :value-type (group string string))
  :group 'ytel)

(defvar ytel--insert-functions '((video . ytel--insert-video)
				 (playlist . ytel--insert-playlist)
				 (channel . ytel--insert-channel)))

(defvar ytel--default-action-functions '((video . ytel--default-video-action)
					 (playlist . ytel--default-playlist-action)
					 (channel . ytel--default-channel-action))
  "Functions to call on an entry.  To modify an action, set the appropiate variable instead.")

(defvar ytel--default-video-action #'(lambda ()
				       (message (ytel-video-title (ytel-get-current-video))))
  "Action to open a video.  By default it just prints the title to the minibuffer.")

(defvar ytel--default-playlist-action #'ytel--open-playlist
  "Action to open a playlist.")

(defvar ytel--default-channel-action #'ytel--open-channel
  "Action to open a channel.")


(defvar ytel-invidious-api-url "https://invidio.us"
  "Url to an Invidious instance.")

(defvar ytel-default-video-query-fields "type,author,lengthSeconds,title,videoId,authorId,viewCount,published"
  "Default fields of interest for video search.")

(defvar ytel-default-channel-query-fields "type,author,authorId,subCount,videoCount"
  "Default fields of interest for channel search.")

(defvar ytel-default-playlist-query-fields "type,title,playlistId,author,authorId,videoCount"
  "Default fields of interest for playlist search.")

(defvar ytel-videos '()
  "List of videos currently on display.")

(defcustom ytel-published-date-time-string "%Y-%m-%d"
  "Time-string used to render the published date of the video.
See `format-time-string' for information on how to edit this variable."
  :type 'string
  :group 'ytel)

(defvar-local ytel-current-page 1
  "Current page of the current `ytel-search-term'")

(defvar-local ytel-search-term ""
  "Current search string as used by `ytel-search'")

(defcustom ytel-author-name-reserved-space 20
  "Number of characters reserved for the channel's name in the *ytel* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long)."
  :type 'integer
  :group 'ytel)

(defcustom ytel-title-video-reserved-space 100
  "Number of characters reserved for the video title in the *ytel* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long)."
  :type 'integer
  :group 'ytel)

(defcustom ytel-title-playlist-reserved-space 30
  "Number of characters reserved for the playlist title in the *ytel* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long)."
  :type 'integer
  :group 'ytel)

(defcustom ytel-name-channel-reserved-space 50
  "Number of characters reserved for the channel name in the *ytel* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long)."
  :type 'integer
  :group 'ytel)

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

(defface ytel-video-title-face
  '((((class color) (background light)) (:foreground "#000"))
    (((class color) (background dark))  (:foreground "#fff")))
  "Face used for the video title.")

(defface ytel-item-videoCount-face
  '((t :inherit ytel-video-view-face))
  "Face used for the videoCount of an entry.")

(defface ytel-item-subCount-face
  '((t :inherit ytel-video-published-face))
  "Face used for the subCount of an entry.")

(defface ytel-parameter-face
  '((t :inherit ytel-video-published-face))
  "Face used for the parameters of the current search.")

(defvar ytel-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'ytel-quit)
    (define-key map "h" #'describe-mode)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "s" #'ytel-search)
    (define-key map ">" #'ytel-search-next-page)
    (define-key map "<" #'ytel-search-previous-page)
    (define-key map "t" #'ytel-search-type)
    (define-key map "S" #'ytel-sort-videos)
    (define-key map "C" #'ytel-show-channels)
    (define-key map "P" #'ytel-show-playlists)
    (define-key map "V" #'ytel-show-videos)
    (define-key map "Y" #'ytel-yank-channel-feed)
    (define-key map "A" #'ytel--open-channel)
    (define-key map (kbd "RET") #'ytel-open-entry)
    map)
  "Keymap for `ytel-mode'.")

(define-derived-mode ytel-mode text-mode
  "ytel-mode"
  "A major mode to query Youtube content through Invidious."
  :group 'ytel
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (make-local-variable 'ytel-videos))

(defun ytel-quit ()
  "Quit ytel buffer."
  (interactive)
  (quit-window))

(defun ytel--format-author (name)
  "Format a channel NAME to be inserted in the *ytel* buffer."
  (let* ((n (string-width name))
	 (extra-chars (- n ytel-author-name-reserved-space))
	 (formatted-string (if (<= extra-chars 0)
			       (concat name
				       (make-string (abs extra-chars) ?\ )
				       "   ")
			     (concat (seq-subseq name 0 ytel-author-name-reserved-space)
				     "..."))))
    (propertize formatted-string 'face 'ytel-channel-name-face)))

(defun ytel--format-title (title)
  "Format a video TITLE to be inserted in the *ytel* buffer."
  (let* ((n (string-width title))
	 (extra-chars (- n ytel-title-video-reserved-space))
	 (formatted-string (if (<= extra-chars 0)
			       (concat title
				       (make-string (abs extra-chars) ?\ )
				       "   ")
			     (concat (seq-subseq title 0 ytel-title-video-reserved-space)
				     "..."))))
    (propertize formatted-string 'face 'ytel-video-title-face)))

(defun ytel--format-playlist-title (title)
  "Format a playlist TITLE to be inserted in the *ytel* buffer."
  (let* ((n (string-width title))
	 (extra-chars (- n ytel-title-playlist-reserved-space))
	 (formatted-string (if (<= extra-chars 0)
			       (concat title
				       (make-string (abs extra-chars) ?\ )
				       "   ")
			     (concat (seq-subseq title 0 ytel-title-playlist-reserved-space)
				     "..."))))
    (propertize formatted-string 'face 'ytel-video-title-face)))

(defun ytel--format-channel-name (name)
  "Format a channel NAME to be inserted in the *ytel* buffer."
  (let* ((n (string-width name))
	 (extra-chars (- n ytel-name-channel-reserved-space))
	 (formatted-string (if (<= extra-chars 0)
			       (concat name
				       (make-string (abs extra-chars) ?\ )
				       "   ")
			     (concat (seq-subseq name 0 ytel-name-channel-reserved-space)
				     "..."))))
    (propertize formatted-string 'face 'ytel-channel-name-face)))

(defun ytel--format-video-length (seconds)
  "Given an amount of SECONDS, format it nicely to be inserted in the *ytel* buffer."
  (let ((formatted-string (concat (ytel--get-icon 'length)
				  (format-seconds "%.2h" seconds)
				  ":"
				  (format-seconds "%.2m" (mod seconds 3600))
				  ":"
				  (format-seconds "%.2s" (mod seconds 60)))))
    (propertize formatted-string 'face 'ytel-video-length-face)))

(defun ytel--format-video-views (views)
  "Format video VIEWS to be inserted in the *ytel* buffer."
  (propertize (format "[%s: %d]" (ytel--get-icon 'views) views) 'face 'ytel-video-view-face))

(defun ytel--format-video-published (published)
  "Format video PUBLISHED date to be inserted in the *ytel* buffer."
  (propertize (format-time-string ytel-published-date-time-string (seconds-to-time published))
	      'face 'ytel-video-published-face))

(defun ytel--format-videoCount (videoCount)
  "Format video VIDEOCOUNT to be inserted in the *ytel* buffer."
  (propertize (format "[%s: %d]" (ytel--get-icon 'videoCount) videoCount) 'face 'ytel-item-videoCount-face))

(defun ytel--format-subCount (subCount)
  "Format video SUBCOUNT to be inserted in the *ytel* buffer."
  (propertize (format "%s: %-10d" (ytel--get-icon 'subCount) subCount) 'face 'ytel-item-subCount-face))

(defun ytel--format-type (type)
  "Insert an icon of TYPE into buffer."
  (if ytel-show-fancy-icons
      (propertize (format "%-2s: " (ytel--get-icon type)) 'face 'ytel-video-title-face)
    (propertize (format "%-10s: " (ytel--get-icon type)) 'face 'ytel-video-title-face)))

(defun ytel--get-icon (item)
  "Get the icon for ITEM from `ytel-icons'."
  (let* ((getmarks (assoc-default item ytel-icons)))
    (if ytel-show-fancy-icons
	(second getmarks)
      (car getmarks))))

(defun ytel--insert-entry (entry)
  "Insert an ENTRY of the form according to its type."
  (let* ((type (if (not (equal ytel-type-of-results "all"))
		   (intern ytel-type-of-results)
		 (cond ((ytel-video-p entry) 'video)
		       ((ytel-playlist-p entry) 'playlist)
		       ((ytel-channel-p entry) 'channel)
		       (t (error "Invalid entry type")))))
	 (func (cdr (assoc type ytel--insert-functions))))
    (when (equal ytel-type-of-results "all")
      (insert (ytel--format-type type)))
    (funcall func entry)))

(defun ytel--insert-video (video)
  "Insert VIDEO in the current buffer."
  (insert  (ytel--format-video-published (ytel-video-published video))
	   " "
	   (ytel--format-author (ytel-video-author video))
	   " "
	   (ytel--format-video-length (ytel-video-length video))
	   " "
	   (ytel--format-title (ytel-video-title video))
	   " "
	   (ytel--format-video-views (ytel-video-views video))))

					;TODO: Format playlist and channel entries in buffer
(defun ytel--insert-playlist (playlist)
  "Insert PLAYLIST in the current buffer."
  (insert (ytel--format-playlist-title (ytel-playlist-title playlist))
	  " "
	  (ytel--format-author (ytel-playlist-author playlist))
	  " "
	  (ytel--format-videoCount (ytel-playlist-videoCount playlist))))

(defun ytel--insert-channel (channel)
  "Insert CHANNEL in the current buffer."
  (insert (ytel--format-channel-name (ytel-channel-author channel))
	  " "
	  (ytel--format-subCount (ytel-channel-subCount channel))
	  " "
	  (ytel--format-videoCount (ytel-channel-videoCount channel))))

(defun ytel--draw-buffer ()
  "Draws the ytel buffer i.e. clear everything and write down all videos in `ytel-videos'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setf header-line-format (concat (propertize (capitalize ytel-type-of-results) 'face 'ytel-parameter-face)
				     " results for "
				     (propertize ytel-search-term 'face 'ytel-parameter-face)
				     ", page "
				     (propertize (number-to-string ytel-current-page) 'face 'ytel-parameter-face)
				     ", sorted by: "
				     (propertize (symbol-name ytel-sort-criterion) 'face 'ytel-parameter-face)))
    (seq-do (lambda (v)
	      (ytel--insert-entry v)
	      (insert "\n"))
	    ytel-videos)
    (goto-char (point-min))))

(defun ytel-enable-fancy-icons ()
  "Enable fancy icons in the *ytel* buffer, using `ytel-icons'."
  (interactive)
  (setf ytel-show-fancy-icons t))

(defun ytel-disable-fancy-icons ()
  "Disable fancy icons in the *ytel* buffer, using `ytel-icons'."
  (interactive)
  (setf ytel-show-fancy-icons nil))

(defun ytel-toggle-fancy-icons ()
  "Toggle display of fancy-icons in the *ytel* buffer, using `ytel-icons'."
  (interactive)
  (setf ytel-show-fancy-icons (not ytel-show-fancy-icons)))

(defun ytel-search (query)
  "Search youtube for `QUERY', and redraw the buffer."
  (interactive "sSearch terms: ")
  (switch-to-buffer "*ytel*")
  (setf ytel-current-page 1)
  (setf ytel-search-term query)
  (setf ytel-videos (ytel--process-results (ytel--query query ytel-current-page)))
  (ytel--draw-buffer))

(defun ytel-search-next-page ()
  "Switch to the next page of the current search.  Redraw the buffer."
  (interactive)
  (setf ytel-videos (ytel--process-results (ytel--query ytel-search-term
							(1+ ytel-current-page))))
  (setf ytel-current-page (1+ ytel-current-page))
  (ytel--draw-buffer))

(defun ytel-search-previous-page ()
  "Switch to the previous page of the current search.  Redraw the buffer."
  (interactive)
  (when (> ytel-current-page 1)
    (setf ytel-videos (ytel--process-results (ytel--query ytel-search-term
							  (1- ytel-current-page))))
    (setf ytel-current-page (1- ytel-current-page))
    (ytel--draw-buffer)))

(defun ytel-search-type (&optional arg)
  "Ask for what type of results to display, and search.
If ARG is given, make a new search."
  (interactive "P")
  (when arg
    (setf ytel-search-term (read-string "Search terms: ")))
  (setf ytel-current-page 1)
  (setf ytel-type-of-results (completing-read "Show: " (get 'ytel-type-of-results 'custom-options)))
  (setf ytel-videos (ytel--process-results (ytel--query ytel-search-term ytel-current-page)))
  (ytel--draw-buffer))

(defun ytel-show-videos (&optional arg)
  "Show videos for the current search.
If ARG is given, make a new search."
  (interactive "P")
  (when arg
    (setf ytel-search-term (read-string "Search terms: ")))
  (setf ytel-current-page 1)
  (setf ytel-type-of-results "video")
  (setf ytel-videos (ytel--process-results (ytel--query ytel-search-term ytel-current-page)))
  (ytel--draw-buffer))

(defun ytel-show-channels (&optional arg)
  "Show channels for the current search.
If ARG is given, make a new search."
  (interactive "P")
  (when arg
    (setf ytel-search-term (read-string "Search terms: ")))
  (setf ytel-current-page 1)
  (setf ytel-type-of-results "channel")
  (setf ytel-videos (ytel--process-results (ytel--query ytel-search-term ytel-current-page)))
  (ytel--draw-buffer))

(defun ytel-show-playlists (&optional arg)
  "Show playlists for the current search.
If ARG is given, make a new search."
  (interactive "P")
  (when arg
    (setf ytel-search-term (read-string "Search terms: ")))
  (setf ytel-current-page 1)
  (setf ytel-type-of-results "playlist")
  (setf ytel-videos (ytel--process-results (ytel--query ytel-search-term ytel-current-page)))
  (ytel--draw-buffer))

(defun ytel-sort-videos ()
  "Sort videos from the current search from page 1, according to values of `ytel-sort-criterion'."
  (interactive)
  (setf ytel-sort-criterion (intern (completing-read "Sort videos by (default value is relevance): " (get 'ytel-sort-criterion 'custom-options))))
  (setf ytel-current-page 1)
  (setf ytel-videos (ytel--process-results (ytel--query ytel-search-term ytel-current-page)))
  (ytel--draw-buffer))

(defun ytel-get-current-video ()
  "Get the currently selected video."
  (aref ytel-videos (1- (line-number-at-pos))))

(defun ytel-yank-channel-feed (&optional arg)
  "Yank channel's Invidious RSS feed for the current video at point.
If ARG is given, format it as a Youtube RSS feed."
  (interactive "P")
  (let* ((entry (ytel-get-current-video))
	 (author (funcall (ytel--get-author-function entry) entry))
	 (authorId (funcall (ytel--get-authorId-function entry) entry))
	 (url (if arg
		  (concat ytel-invidious-api-url "/feed/channel/" authorId)
		(concat "https://www.youtube.com/feeds/videos.xml?channel_id=" authorId))))
    (kill-new url)
    (message "Copied RSS feed for: %s - %s" author url)))

(defun ytel--get-entry-type (entry)
  "Return the type of ENTRY."
  (if (not (equal ytel-type-of-results "all"))
      (intern ytel-type-of-results)
    (cond ((ytel-video-p entry) 'video)
	  ((ytel-playlist-p entry) 'playlist)
	  ((ytel-channel-p entry) 'channel)
	  (t (error "Invalid entry type")))))

(defun ytel--get-author-function (entry)
  "Get the author for ENTRY."
  (let* ((type (ytel--get-entry-type entry)))
    (pcase type
      ('video #'ytel-video-author)
      ('playlist #'ytel-playlist-author)
      ('channel #'ytel-channel-author)
      (_ (error "Invalid entry type")))))

(defun ytel--get-authorId-function (entry)
  "Get the author for ENTRY."
  (let* ((type (ytel--get-entry-type entry)))
    (pcase type
      ('video #'ytel-video-authorId)
      ('playlist #'ytel-playlist-authorId)
      ('channel #'ytel-channel-authorId)
      (_ (error "Invalid entry type")))))

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

;; Youtube interface stuff below.
(cl-defstruct (ytel-video (:constructor ytel-video--create)
			  (:copier nil))
  "Information about a Youtube video."
  (title     "" :read-only t)
  (id        0  :read-only t)
  (author    "" :read-only t)
  (authorId  "" :read-only t)
  (length    0  :read-only t)
  (views     0  :read-only t)
  (published 0 :read-only t))

;; Maybe type should be part of the struct.
(cl-defstruct (ytel-channel (:constructor ytel-channel--create)
			    (:copier nil))
  "Information about a Youtube channel."
  (author     "" :read-only t)
  (authorId   "" :read-only t)
  (subCount   0  :read-only t)
  (videoCount 0  :read-only t))

(cl-defstruct (ytel-playlist (:constructor ytel-playlist--create)
			     (:copier nil))
  "Information about a Youtube playlist."
  (title      "" :read-only t)
  (playlistId "" :read-only t)
  (author     "" :read-only t)
  (authorId   "" :read-only t)
  (videoCount 0  :read-only t))

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

(defun ytel--query (string n)
"Query youtube for STRING, return the Nth page of results."
(let ((results (ytel--API-call "search" `(("q" ,string)
					  ("sort_by" ,(symbol-name ytel-sort-criterion))
					  ("type" ,ytel-type-of-results)
					  ("page" ,n)
					  ("fields" ,(pcase ytel-type-of-results
						       ("video" ytel-default-video-query-fields)
						       ("playlist" ytel-default-playlist-query-fields)
						       ("channel" ytel-default-channel-query-fields)
						       ("all" (concat ytel-default-channel-query-fields ;; I mean, it does get the job done... fix later.
								      ","
								      ytel-default-playlist-query-fields
								      ","
								      ytel-default-video-query-fields))))))))
  results))

(defun ytel--process-results (results &optional type)
  "Process RESULTS and turn them into objects, is TYPE is not given, get it from RESULTS."
  (dotimes (i (length results))
    (let* ((v (aref results i))
	   (type (or type (assoc-default 'type v))))
      (aset results i  (pcase type
			 ("video" (ytel-video--create :title     (assoc-default 'title v)
						      :author    (assoc-default 'author v)
						      :authorId  (assoc-default 'authorId v)
						      :length    (assoc-default 'lengthSeconds v)
						      :id        (assoc-default 'videoId v)
						      :views     (assoc-default 'viewCount v)
						      :published (assoc-default 'published v)))
			 ("playlist" (ytel-playlist--create :title      (assoc-default 'title v)
							    :playlistId (assoc-default 'playlistId v)
							    :author     (assoc-default 'author v)
							    :authorId   (assoc-default 'authorId v)
							    :videoCount (assoc-default 'videoCount v)))
			 ("channel" (ytel-channel--create :author     (assoc-default 'author v)
							  :authorId   (assoc-default 'authorId v)
							  :subCount   (assoc-default 'subCount v)
							  :videoCount (assoc-default 'videoCount v)))))))
  results)

(defun ytel-open-entry ()
  "Open the entry at point depending on it's type."
  (interactive)
  (let* ((entry (ytel-get-current-video))
	 (type (ytel--get-entry-type entry)))
    (funcall (symbol-value (assoc-default type ytel--default-action-functions)))))

(defun ytel--open-channel ()
  "Fetch the channel page for the entry at point."
  (interactive)
  (let* ((entry (ytel-get-current-video))
	 (author (funcall (ytel--get-author-function entry) entry))
	 (authorId (funcall (ytel--get-authorId-function entry) entry)))
    (require 'ytel-channel)
    (ytel-channel author authorId)))

(defun ytel--open-playlist ()
  "Open the contents of the entry at point, if it's a playlist."
  (interactive)
  (require 'ytel-playlist)
  (ytel--get-playlist-videos))

(provide 'ytel)

;;; ytel.el ends here
