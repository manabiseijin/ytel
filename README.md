# ytel
`ytel` is an experimental Youtube "frontend" for Emacs. It's goal is to allow the user to collect the results of a Youtube search in an elfeed-like buffer and then manipulate them with Emacs Lisp. The gif below shows that `ytel` can be used to play videos in an external player, to learn how to emulate it refer to the [usage](#usage) section below.

<p align="center">
  <img src="https://github.com/gRastello/ytel/blob/master/pic/demonstration.gif">
</p>

This project was inspired by [elfeed](https://github.com/skeeto/elfeed/) and [Invidious](https://github.com/omarroth/invidious) (it does indeed use the Invidious APIs).

## Installation
This project is, in my opinion, too young to be in Melpa so you have to download the `ytel.el` file in some place that Emacs can reach (i.e. in a directory that's in your `load-path`) and then add `(require 'ytel)` somewhere in your configuration (`init.el` or org-babel or whatever you use).

### Dependencies
While `ytel` does not depend on any Emacs package it does depend on `curl` so, if you happen not to have it, install it through your package manager (meme distros aside it is probably in your repos).

## Usage
Once everything is loaded `M-x ytel` creates a new buffer and puts it in `ytel-mode`. This major mode has just a few bindings (for now):

| key | binding         |
|-----|-----------------|
| n   | `next-line`     |
| p   | `previous-line` |
| q   | `ytel-quit`     |
| s   | `ytel-search`   |

Pressing `s` will prompt for some search terms and populate the buffer once the results are available. Once this has the important side-effect of updating the buffer-local variable `ytel-vieos` that will then contain an array of all the videos on display. One can access this variable as-is or use the predefined function `ytel-get-current-video` that returns the video at point. Videos returned by `ytel-get-current-video` are pseudo-alists with keys `title`, `secondsLength`, `videoId` and `author`.

With this information we can implement a function to stream a video in `mpv` as follows:
```elisp
(defun ytel-watch ()
    "Stream video at point in mpv."
    (interactive)
    (let* ((video (ytel-get-current-video))
     	   (id    (assoc-default 'videoId video)))
      (start-process "ytel mpv" nil
		     "mpv"
		     (concat "https://www.youtube.com/watch?v=" id))
		     "--ytdl-format=bestvideo[height<=?720]+bestaudio/best")
      (message "Starting streaming..."))
```

And bind it to a key in `ytel-mode` with
```elisp
(define-key ytel-mode-map "y" #'ytel-watch)
```

This is of course just an example. You can similarly implement functions to:
- open a video in the browser,
- download a video,
- download just the audio of a video,
by relying on the correct external tool.

## Potential problems and potential fixes
`ytel` does not use the official Youtube APIs but relies on the [Invidious](https://github.com/omarroth/invidious) APIs (that in turn circumvent Youtube ones). The variable `ytel-invidious-api-url` points to the invidious instance (by default `https://invidio.us`) to use; you might not need to ever touch this, but if [invidio.us](https://invidio.us) goes down keep in mind that you can choose [another instance](https://github.com/omarroth/invidious#invidious-instances). Moreover the default Invidious instance is generally speaking stable, but sometimes your `ytel-search` might hang; in that case `C-g` and retry.

## TODO
Here's a bunch of things that ought to be done someday:
- [X] make the `ytel` buffer more visually appealing,
- [ ] add functionality to delete videos from the buffer,
- [ ] add functionality to add the results of a new search to the buffer, without resetting what's already there,
- [ ] maybe videos should be represented as a structures (plain alists is very lazy),
- [ ] calls to the Invidious API are performed synchronously, this might prove to be very stupid.

## FAQ

#### Why?
One can easily subscribe to Youtube channels via an RSS feed and access it in Emacs via [elfeed](https://github.com/skeeto/elfeed/) but sometimes I want to search Youtube for videos of people I don't necessarily follow (e.g. for searching a tutorial, or music, or wasting some good time) and being able to do that without switching to a browser is nice.

#### What about [helm-youtube](https://github.com/maximus12793/helm-youtube) and [ivy-youtube](https://github.com/squiter/ivy-youtube)?
First of all those packages require you to get a Google API key, while `ytel` uses the [Invidious](https://github.com/omarroth/invidious) APIs that in turn do not use the official Google APIs.

Moreover those packages are designed to select a Youtube search result and play it directly in your browser while `ytel` is really a way to collect search results in an `elfeed`-like buffer and make them accessible to the user via functions such as `ytel-get-current-video` so the user gets to decide what to to with them.
