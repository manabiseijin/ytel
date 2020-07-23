;;; ytel-channel.el --- Auxiliary major mode for ytel -*- lexical-binding: t; -*-

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
;; This package provides a major mode to view channels in an elfeed style buffer.
;; Information about videos displayed in this buffer can be extracted
;; and manipulated by user-defined functions to do various things such as:
;; - playing them in some video player
;; - download them
;; The limit is the sky.
;; Currently under development.

;;; Code:

(require 'ytel)

(defcustom ytel-channel-sort-criterion "newest"
  "Sort videos by 'newest', 'oldest', or 'popular', as used by `ytel-channel-search'."
  :type 'string
  :options '("newest" "oldest" "popular")
  :group 'ytel-channel)

(defvar ytel-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "h" #'describe-mode)
    (define-key map "q" #'ytel--quit-channel-buffer)
    (define-key map ">" #'ytel-channel-next-page)
    (define-key map "<" #'ytel-channel-previous-page)
    (define-key map "S" #'ytel-channel-sort-videos)
    (define-key map (kbd "RET") #'ytel-open-entry)
    map)
  "Keymap for `ytel-channel-mode'.")

(define-derived-mode ytel-channel-mode ytel-mode
  "ytel-channel-mode"
  "Mode for displaying ytel-channel-videos.
\\{ytel-channel-mode-map}"
  (buffer-disable-undo)
  (make-local-variable 'ytel-videos)
  (make-local-variable 'ytel-channel-author)
  (setq-local ytel-type-of-results "video")
  (setf	buffer-read-only t))

(defun ytel--channel-query (uid n sort)
  "Query youtube for UID videos, return the Nth page of results, sorted bv SORT."
  (let ((videos (ytel--API-call (concat "channels/videos/" uid)
				`(("page" ,n)
				  ("sort_by" ,sort)
				  ("fields" ,ytel-default-video-query-fields)))))
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
  "Displays videos from AUTHOR in buffer, using AUTHORID."
  (get-buffer-create author)
  (switch-to-buffer author)
  (unless (eq major-mode 'ytel-channel-mode)
    (ytel-channel-mode))
  (setf ytel-channel-author author)
  (setf ytel-search-term authorId)
  (ytel-channel-get authorId))

(defun ytel-channel-get (authorId)
  "Fetch videos from AUTHORID."
  (setf ytel-current-page 1)
  (setf ytel-videos (ytel--channel-query authorId ytel-current-page ytel-channel-sort-criterion))
  (ytel--draw-channel-buffer))

(defun ytel-channel-next-page ()
  "Fetch videos from AUTHORID."
  (interactive)
  (setf ytel-current-page (1+ ytel-current-page))
  (setf ytel-videos (ytel--channel-query ytel-search-term ytel-current-page ytel-channel-sort-criterion))
  (ytel--draw-channel-buffer))

(defun ytel-channel-previous-page ()
  "Fetch videos from AUTHORID."
  (interactive)
  (when (> ytel-current-page 1)
    (setf ytel-current-page (1- ytel-current-page))
    (setf ytel-videos (ytel--channel-query ytel-search-term ytel-current-page ytel-channel-sort-criterion))
    (ytel--draw-channel-buffer)))

(defun ytel-channel-sort-videos ()
  "Sort videos from the current channel, either by newest (default), oldest, or popular."
  (interactive)
  (setf ytel-channel-sort-criterion (completing-read "Sort videos by (default value is newest): " (get 'ytel-channel-sort-criterion 'custom-options)))
  (setf ytel-current-page 1)
  (setf ytel-videos (ytel--channel-query ytel-search-term ytel-current-page ytel-channel-sort-criterion))
  (ytel--draw-channel-buffer))

(defun ytel--insert-channel-video (video)
  "Insert VIDEO in the current buffer."
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
    (setq header-line-format (concat "Displaying videos from " (propertize ytel-channel-author 'face 'ytel-parameter-face)
				     ", page "
				     (propertize (number-to-string ytel-current-page) 'face 'ytel-parameter-face)
				     ", sorted by: "
				     (propertize ytel-channel-sort-criterion 'face 'ytel-parameter-face)))
    (seq-do (lambda (v)
	      (ytel--insert-channel-video v)
	      (insert "\n"))
	    ytel-videos)
    (goto-char (point-min))))

(defun ytel--quit-channel-buffer ()
  "Deletes the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'ytel-channel)

;;; ytel-channel.el ends here
