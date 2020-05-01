;;; ytel.el --- Query Youtube from Emacs

;; Version: 0.1.0
;; Author: Gabriele Rastello
;; Keywords: youtube search
;; URL: https://github.com/grastello/yt.el
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "25.3"))

;; This file is NOT part of GNU Emacs.

;;; Code:
(defvar ytel-invidious-api-url "https://invidio.us"
  "Url to an invidious instance.")

(defvar ytel-invidious-default-query-fields "author,lengthSeconds,title,videoId"
  "Default fields of interest for video search.")

(defvar ytel-videos '()
  "List of videos currently on display.")

(defvar ytel-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'ytel-quit)
    (define-key map "h" #'describe-mode)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "s" #'ytel-search)
    map)
  "Keymap for ytel-mode.")

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
  (hl-line-mode))

(defun ytel-quit ()
  "Quit ytel buffer."
  (interactive)
  (quit-window))

(defun ytel--insert-video (video)
  "Insert `video' in the current buffer.

The formatting is actually terrible, but this is not final."
  (insert (assoc-default 'author video)
	  " | "
	  (number-to-string (/ (assoc-default 'lengthSeconds video)
			       60.0))
	  "m | "
	  (assoc-default 'title video)))

(defun ytel--draw-buffer ()
  "Draws the ytel buffer i.e. clear everything and write down all videos in
ytel-videos."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (seq-do #'(lambda (v)
		(ytel--insert-video v)
		(insert "\n"))
	    ytel-videos))
  (goto-char (point-min)))

(defun ytel-search (query)
  "Searches youtube for `query', updates `ytel-videos' and redraw the buffer."
  (interactive "sSearch terms: ")
  (setf ytel-videos (ytel--query query))
  (ytel--draw-buffer))

(defun ytel-get-current-video ()
  "Get the currently selected video."
  (aref ytel-videos (1- (line-number-at-pos))))

(defun ytel-buffer ()
  (get-buffer-create "*ytel*"))

;;;###autoload
(defun ytel ()
  "Enter ytel."
  (interactive)
  (switch-to-buffer (ytel-buffer))
  (unless (eq major-mode 'ytel-mode)
    (ytel-mode)))

;; Youtube interface stuff below.
(defun ytel--hexify-args (args)
  "Transform a list of conses into a percent-encoded string."
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

(ert-deftest ytel--hexify-args-test ()
  "Test the `hexify-args' function."
  (should (equal (ytel--hexify-args '())
		 ""))
  (should (equal (ytel--hexify-args '(("pretty" . "1")))
		 "pretty=1"))
  (should (equal (ytel--hexify-args '(("pretty" . "1") ("fields" . "version")))
		 "pretty=1&fields=version")))

(defun ytel--API-call (method args)
  "Perform a call to the ividious API method method passing args.

Curl is used to perform the request. An error is thrown if it exits with a non
zero exit code otherwise the request body is parsed by `json-read' and returned."
  (with-temp-buffer
    (let ((exit-code (call-process "curl" nil t nil
				   "--silent"
				   "-X" "GET"
				   (concat ytel-invidious-api-url
					   "/api/v1/" method
					   "?" (ytel--hexify-args args)))))
      (unless (= exit-code 0)
	(error "Curl had problems connecting to Invidious."))
      (goto-char (point-min))
      (json-read))))

(defun ytel--query (string)
  "Query youtube for string."
  (ytel--API-call "search" `(("q" .      ,string)
			     ("fields" . ,ytel-invidious-default-query-fields))))

(provide 'ytel)
;;; yt.el ends here
