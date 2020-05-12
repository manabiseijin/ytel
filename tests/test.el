;;; The tests

(require 'ert)

(ert-deftest ytel--format-author-test ()
  "Test the `format-author' function."
  (should (equal (length (ytel--format-author "channel name"))
		 (+ 3 ytel-author-name-reserved-space)))
  (should (equal (length (ytel--format-author "very very long channel name"))
		 (+ 3 ytel-author-name-reserved-space))))

(ert-deftest ytel--format-video-test ()
  "test the `format-video' test."
  (should (equal (ytel--format-video-length 60)
		 "00:01:00"))
  (should (equal (ytel--format-video-length 72)
		 "00:01:12"))
  (should (equal (ytel--format-video-length 134)
		 "00:02:14"))
  (should (equal (ytel--format-video-length 3600)
		 "01:00:00"))
  (should (equal (ytel--format-video-length 5100)
		 "01:25:00"))
  (should (equal (ytel--format-video-length 5430)
		 "01:30:30")))
