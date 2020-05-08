;;; ytel.el --- Query Youtube -*- lexical-binding: t; -*-

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

;; This package provide a major mode to search Youtube videos via an elfeed-like
;; buffer.  Information about videos displayed in this buffer can be extracted
;; and manipulated by user-defined functions to do various things such as:
;; - playing them in some video player
;; - download them
;; The limit is the sky.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(defgroup ytel ()
  "An Emacs Youtube \"front-end\"."
  :group 'comm)

(defvar ytel-invidious-api-url "https://invidio.us"
  "Url to an invidious instance.")

(defvar ytel-invidious-default-query-fields "author,lengthSeconds,title,videoId"
  "Default fields of interest for video search.")

(defvar ytel-videos '()
  "List of videos currently on display.")

(defvar ytel-author-name-reserved-space 20
  "Number of characters reserved for the channel's name in the *ytel* buffer.
Note that there will always 3 extra spaces for eventual dots (for names that are
too long).")

(defvar ytel-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'ytel-quit)
    (define-key map "h" #'describe-mode)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "s" #'ytel-search)
    (define-key map "S" #'ytel-search-replace)
    (define-key map "r" #'ytel-remove-current-video)
    map)
  "Keymap for `ytel-mode'.")

(defun ytel-mode ()
  "Major mode for querying youtube and display results.

\\{ytel-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map ytel-mode-map)
  (setq major-mode       'ytel-mode
	mode-name        "ytel-mode"
	truncate-lines   t
	buffer-read-only t)
  (buffer-disable-undo)
  (make-local-variable 'ytel-videos)
  (hl-line-mode)

  (run-mode-hooks 'ytel-mode-hook))

(defun ytel-quit ()
  "Quit ytel buffer."
  (interactive)
  (quit-window))

(defface ytel-channel-name-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used for channel names.")

(defun ytel--format-author (name)
  "Format a channel NAME to be inserted in the *ytel* buffer."
  (let* ((n (length name))
	 (extra-chars (- n ytel-author-name-reserved-space))
	 (formatted-string (if (<= extra-chars 0)
			       (concat name
				       (make-string (abs extra-chars) ?\ )
				       "   ")
			     (concat (seq-subseq name 0 ytel-author-name-reserved-space)
				     "..."))))
    (propertize formatted-string 'face 'ytel-channel-name-face)))

(defface ytel-video-length-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used for the video length.")

(defun ytel--format-video-length (seconds)
  "Given an amount of SECONDS, format it nicely to be inserted in the *ytel* buffer."
  (let ((formatted-string (concat (format-seconds "%.2h" seconds)
				 ":"
				 (format-seconds "%.2m" (mod seconds 3600))
				 ":"
				 (format-seconds "%.2s" (mod seconds 60)))))
    (propertize formatted-string 'face 'ytel-video-length-face)))

(defun ytel--insert-video (video)
  "Insert `VIDEO' in the current buffer.

The formatting is actually terrible, but this is not final."
  (insert (ytel--format-author (ytel-video-author video))
	  " "
	  (ytel--format-video-length (ytel-video-length video))
	  " "
	  (ytel-video-title video)))

(defun ytel--draw-buffer (&optional restore-point)
  "Draws the ytel buffer i.e.
clear everything and write down all videos in `ytel-videos'.
If RESTORE-POINT is 't then restore the cursor line position."
  (let ((inhibit-read-only t)
	(current-line      (line-number-at-pos)))
    (erase-buffer)
    (seq-do #'(lambda (v)
		(ytel--insert-video v)
		(insert "\n"))
	    ytel-videos)
    (goto-char (point-min))
    (when restore-point
      	(forward-line (1- current-line)))))

(defun ytel-search (query)
  "Search youtube for `QUERY', append results to `ytel-videos' and redraw the buffer."
  (interactive "sSearch terms: ")
  (setf ytel-videos (vconcat ytel-videos
			     (ytel--query query)))
  (ytel--draw-buffer t))

(defun ytel-search-replace (query)
  "Search youtube for `QUERY' and override `ytel-videos' with the results.
Redraw the buffer."
  (interactive "sSearch terms: ")
  (setf ytel-videos (ytel--query query))
  (ytel--draw-buffer))

(defun ytel-remove-current-video ()
  "Remove the currently selected video and redraw the buffer."
  (interactive)
  (let ((i (1- (line-number-at-pos))))
      (setf ytel-videos (vconcat (seq-subseq ytel-videos 0 i)
				 (seq-subseq ytel-videos (1+ i))))
      (ytel--draw-buffer t)))

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
    (ytel-mode)))

;; Youtube interface stuff below.
(cl-defstruct ytel-video
  "Information about a Youtube video."
  (title  "" :read-only t)
  (id     0  :read-only t)
  (author "" :read-only t)
  (length 0  :read-only t))

(defun ytel--hexify-args (args)
  "Transform a list ARGS of conses into a percent-encoded string."
  (cond ((null args)
	 "")
	((= (length args) 1)
	 (concat (url-hexify-string (caar args))
		 "="
		 (url-hexify-string (cdar args))))
	(t
	 (concat (url-hexify-string (caar args))
		 "="
		 (url-hexify-string (cdar args))
		 "&"
		 (ytel--hexify-args (cdr args))))))

(defun ytel--API-call (method args)
  "Perform a call to the ividious API method METHOD passing ARGS.

Curl is used to perform the request.  An error is thrown if it exits with a non
zero exit code otherwise the request body is parsed by `json-read' and returned."
  (with-temp-buffer
    (let ((exit-code (call-process "curl" nil t nil
				   "--silent"
				   "-X" "GET"
				   (concat ytel-invidious-api-url
					   "/api/v1/" method
					   "?" (ytel--hexify-args args)))))
      (unless (= exit-code 0)
	(error "Curl had problems connecting to Invidious"))
      (goto-char (point-min))
      (json-read))))

(defun ytel--query (string)
  "Query youtube for STRING."
  (let ((videos (ytel--API-call "search" `(("q" .      ,string)
					   ("fields" . ,ytel-invidious-default-query-fields)))))
    (dotimes (i (length videos))
      (let ((v (aref videos i)))
	(aset videos i
	      (make-ytel-video :title  (assoc-default 'title v)
			       :author (assoc-default 'author v)
			       :length (assoc-default 'lengthSeconds v)
			       :id     (assoc-default 'videoId v)))))
    videos))

(provide 'ytel)

;;; ytel.el ends here
