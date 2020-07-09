;;; ytel-playlist.el --- Auxiliary mode to display playlist results



(require 'ytel)

;;; Code:

(defun ytel--process-playlist-videos (videos)
  "Process VIDEOS fetched from a playlist."
  (dotimes (i (length videos))
    (let* ((v (aref videos i)))
      (aset videos i  (ytel-video--create :title     (assoc-default 'title v)
					  :author    (assoc-default 'author v)
					  :authorId  (assoc-default 'authorId v)
					  :length    (assoc-default 'lengthSeconds v)
					  :id        (assoc-default 'videoId v)))))
  videos)

(defun ytel--get-playlist-videos ()
  "Fetch the videos of the current playlist."
  (let* ((entry (ytel-get-current-video))
	 (ytel-type-of-results "video"))
    (if (ytel-playlist-p entry)
	(progn
	  (switch-to-buffer (ytel-playlist-title entry))
	  (unless (eq major-mode 'ytel-mode)
	    (ytel-mode))
	  (let* ((results (ytel--API-call (concat "playlists/" (ytel-playlist-playlistId entry)) '(("fields" "videos")
												   ("page" ,ytel-current-page)))))
	    (setf ytel-videos (ytel--process-playlist-videos (assoc-default 'videos results)))
	    (ytel--draw-buffer))))))

(provide 'ytel-playlist)

;;; ytel-playlist.el ends here
