;;; ytel-playlist.el --- Auxiliary mode to display playlist results
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

;;; Commentary:

;; This package provide a major mode to view a playlist via an elfeed-like
;; buffer.  Information about videos displayed in this buffer can be extracted
;; and manipulated by user-defined functions to do various things such as:
;; - playing them in some video player
;; - download them
;; The limit is the sky.
;; Currently under development


(require 'ytel)

;;; Code:

(defvar ytel-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" #'describe-mode)
    (define-key map "q" #'ytel--quit-playlist-buffer)
    (define-key map ">" #'ytel-playlist-next-page)
    (define-key map "<" #'ytel-playlist-previous-page)
    (define-key map (kbd "RET") #'ytel-open-entry)
    map)
  "Keymap for `ytel-playlist-mode'.")

(define-derived-mode ytel-playlist-mode text-mode
  "ytel-playlist-mode"
  "Mode for displaying ytel-playlists.
\\{ytel-playlist-mode-map}"
  (buffer-disable-undo)
  (use-local-map ytel-playlist-mode-map)
  (make-local-variable 'ytel-videos)
  (make-local-variable 'ytel-playlist-title)
  (make-local-variable 'ytel-playlistId)
  (setq-local ytel-type-of-results "video")
  (setf buffer-read-only t))

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

(defun ytel-playlist--insert-entry (video)
  "Insert VIDEO into the playlist buffer."
  (insert (ytel--format-author (ytel-video-author video))
	  " "
	  (ytel--format-video-length (ytel-video-length video))
	  " "
	  (ytel--format-title (ytel-video-title video))))

(defun ytel--get-playlist-videos ()
  "Fetch the videos of the current playlist."
  (let* ((entry (ytel-get-current-video)))
    (if (ytel-playlist-p entry)
	(progn
	  (switch-to-buffer (ytel-playlist-title entry))
	  (unless (eq major-mode 'ytel-playlist-mode)
	    (ytel-playlist-mode))
	  (setf ytel-playlistId (ytel-playlist-playlistId entry))
	  (setf ytel-playlist-title (ytel-playlist-title entry))
	  (ytel-playlist--query ytel-playlistId ytel-current-page)
	  (ytel-playlist--draw-buffer)))))

(defun ytel-playlist-previous-page ()
  "Go to the previous page of playlist."
  (interactive)
  (setf ytel-current-page (1- ytel-current-page))
  (ytel-playlist--query ytel-playlistId ytel-current-page)
  (ytel-playlist--draw-buffer))

(defun ytel-playlist-next-page ()
  "Go to the next page of playlist."
  (interactive)
  (setf ytel-current-page (1+ ytel-current-page))
  (ytel-playlist--query ytel-playlistId ytel-current-page)
  (ytel-playlist--draw-buffer))

(defun ytel-playlist--draw-buffer ()
  "Draw buffer for the current playlist."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq header-line-format (concat "Displaying videos from " (propertize ytel-playlist-title 'face 'ytel-video-published-face)
				     ", page "
				     (propertize (number-to-string ytel-current-page) 'face 'ytel-video-published-face)))
    (seq-do (lambda (v)
	      (ytel-playlist--insert-entry v)
	      (insert "\n"))
	    ytel-videos)
    (goto-char (point-min))))

(defun ytel-playlist--query (playlistID page)
  "Query Invidious for videos from PLAYLISTID on PAGE."
  (let* ((results (ytel--API-call (concat "playlists/" ytel-playlistId) '(("fields" "videos")
									  ("page" ,ytel-current-page)))))
    (setf ytel-videos (ytel--process-playlist-videos (assoc-default 'videos results)))))

(defun ytel--quit-playlist-buffer ()
  "Deletes the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))


(provide 'ytel-playlist)

;;; ytel-playlist.el ends here
