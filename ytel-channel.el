(require 'ytel)


(defcustom ytel-channel-sort-criterion "newest"
  "Sort videos by 'newest', 'oldest', or 'popular', as used by `ytel-channel-search'."
  :type 'string
  :options '("newest" "oldest" "popular")
  :group 'ytel-channel)

(defun ytel-channel-mode ()
  "Mode for displaying ytel-channel-videos.
\\{ytel-channel-mode-map}"
  (interactive)
  (buffer-disable-undo)
  (use-local-map ytel-channel-mode-map)
  (make-local-variable 'ytel-videos)
  (make-local-variable 'ytel-channel-author)
  (setf major-mode 'ytel-channel-mode
	mode-name "ytel-channel"
	buffer-read-only t))

(defvar ytel-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'ytel--quit-channel-buffer)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map ">" #'ytel-channel-next-page)
    (define-key map "<" #'ytel-channel-previous-page)
    (define-key map "S" #'ytel-channel-sort-videos)
    map)
  "Keymap for `ytel-channel-mode'.")

(defun ytel--channel-query (uid n sort)
  "Query youtube for UID videos, return the Nth page of results, sorted bv SORT."
  (let ((videos (ytel--API-call (concat "channels/videos/" uid)
				`(("page" ,n)
				  ("sort_by" ,sort)
				  ("fields" ,ytel-invidious-default-query-fields)))))
    (dotimes (i (length videos))
      (let ((v (aref videos i)))
	(aset videos i
	      (ytel-video--create :title     (assoc-default 'title v)
				  :author    (assoc-default 'author v)
				  :authorId  (assoc-default 'authorId v)
				  :length    (assoc-default 'lengthSeconds v)
				  :id        (assoc-default 'videoId v)
				  :views     (assoc-default 'viewCount v)
				  :published (assoc-default 'published v)))))
    videos))

(defun ytel-channel (author authorId)
  (get-buffer-create author)
  (switch-to-buffer author)
  (unless (eq major-mode 'ytel-channel-mode)
    (ytel-channel-mode))
  (setf ytel-channel-author author)
  (setf ytel-search-term authorId)
  (ytel-channel-get authorId))

(defun ytel-channel-get (authorId)
  "Fetch videos from AUTHORID"
  (setf ytel-current-page 1)
  (setf ytel-videos (ytel--channel-query authorId ytel-current-page ytel-channel-sort-criterion))      
  (ytel--draw-channel-buffer))

(defun ytel-channel-next-page ()
  "Fetch videos from AUTHORID"
  (interactive)
  (setf ytel-current-page (1+ ytel-current-page))
  (setf ytel-videos (ytel--channel-query ytel-search-term ytel-current-page ytel-channel-sort-criterion))      
  (ytel--draw-channel-buffer))

(defun ytel-channel-previous-page ()
  "Fetch videos from AUTHORID"
  (interactive)
  (when (> ytel-current-page 1)
    (setf ytel-current-page (1- ytel-current-page))
    (setf ytel-videos (ytel--channel-query ytel-search-term ytel-current-page ytel-channel-sort-criterion))      
    (ytel--draw-channel-buffer)))

(defun ytel-channel-sort-videos ()
  "Sort videos from the current channel, either by newest (default), oldest, or popular"
  (interactive)
  (setf ytel-channel-sort-criterion (completing-read "Sort videos by (default value is newest): " '("newest" "oldest" "popular")))
  (setf ytel-current-page 1)
  (setf ytel-videos (ytel--channel-query ytel-search-term ytel-current-page ytel-channel-sort-criterion))
  (ytel--draw-channel-buffer))

(defun ytel--insert-channel-video (video)
  "Insert `VIDEO' in the current buffer."
  (insert (ytel--format-video-published (ytel-video-published video))
	  " "
	  (ytel--format-title (ytel-video-title video))
	  " "
	  (ytel--format-video-length (ytel-video-length video))
	  " "
	  (ytel--format-video-views (ytel-video-views video))))

(defun ytel--draw-channel-buffer ()
  "Draws the ytel channel buffer i.e. clear everything and write down all videos in `ytel-videos'."
  (let ((inhibit-read-only t)s
	(current-line      (line-number-at-pos)))
    (erase-buffer)
    (setq header-line-format (concat "Displaying videos from " (propertize ytel-channel-author 'face 'ytel-video-published-face)
				     ", page "
				     (number-to-string ytel-current-page)
				     ", sorted by: "
				     ytel-channel-sort-criterion))
    (seq-do (lambda (v)
	      (ytel--insert-channel-video v)
	      (insert "\n"))
	    ytel-videos)
    (goto-char (point-min))))

(defun ytel--quit-channel-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'ytel-channel)
