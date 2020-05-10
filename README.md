# ytel
`ytel` is an experimental YouTube "frontend" for Emacs. It's goal is to allow the user to collect the results of a YouTube search in an elfeed-like buffer and then manipulate them with Emacs Lisp. The gif below shows that `ytel` can be used to play videos in an external player, to learn how to emulate it refer to the [usage](#usage) section below.

<p align="center">
  <img src="https://github.com/gRastello/ytel/blob/master/pic/demonstration.gif">
</p>

This project was inspired by [elfeed](https://github.com/skeeto/elfeed/) and [Invidious](https://github.com/omarroth/invidious) (it does indeed use the Invidious APIs).

## Installation
This project is on its way to Melpa, but for now you'll have to download the `ytel.el` file in some place that Emacs can reach (i.e. in a directory that's in your `load-path`) and then add `(require 'ytel)` somewhere in your configuration (`init.el` or org-babel or whatever you use).

### Dependencies
While `ytel` does not depend on any Emacs package it does depend on `curl` so, if you happen not to have it, install it through your package manager (meme distros aside it is probably in your repos).

## Usage
Once everything is loaded `M-x ytel` creates a new buffer and puts it in `ytel-mode`. This major mode has just a few bindings (for now):

| key | binding                     |
|-----|-----------------------------|
| n   | `next-line`                 |
| p   | `previous-line`             |
| q   | `ytel-quit`                 |
| s   | `ytel-search`               |
| S   | `ytel-search-replace`       |
| r   | `ytel-delete-current-video` |

Pressing `s` will prompt for some search terms and populate the buffer once the results are available. Performing more searches with `s` will add videos to the buffer; videos can be removed by pressing `r` and a "fresh" search can be run with `S`. One can access information about a video via the function `ytel-get-current-video` that returns the video at point. Videos returned by `ytel-get-current-video` are cl-structures so you can access their fields with the `ytel-video-*` functions. Currently videos have four fields:

| field           | description                     |
|-----------------|---------------------------------|
| `id`            | the video's id                  |
| `title`         | the video's title               |
| `author`        | name of the author of the video |
| `lengthSeconds` | length of the video in seconds  |

With this information we can implement a function to stream a video in `mpv` (provided you have `youtube-dl` installed) as follows:
```elisp
(defun ytel-watch ()
    "Stream video at point in mpv."
    (interactive)
    (let* ((video (ytel-get-current-video))
     	   (id    (ytel-video-id video)))
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
`ytel` does not use the official YouTube APIs but relies on the [Invidious](https://github.com/omarroth/invidious) APIs (that in turn circumvent YouTube ones). The variable `ytel-invidious-api-url` points to the invidious instance (by default `https://invidio.us`) to use; you might not need to ever touch this, but if [invidio.us](https://invidio.us) goes down keep in mind that you can choose [another instance](https://github.com/omarroth/invidious#invidious-instances). Moreover the default Invidious instance is generally speaking stable, but sometimes your `ytel-search` might hang; in that case `C-g` and retry.

## TODO
Here's a bunch of things that ought to be done someday:
- [X] make the `ytel` buffer more visually appealing,
- [X] add functionality to delete videos from the buffer,
- [X] add functionality to add the results of a new search to the buffer, without resetting what's already there,
- [X] maybe videos should be represented as a structures (plain alists is very lazy),
- [ ] calls to the Invidious API are performed synchronously, this might prove to be very stupid.

## Contributing
Feel free to open an issue or send a pull request. I'm quite new to writing Emacs packages so any help is appreciated.

## FAQ

#### Why?
One can easily subscribe to YouTube channels via an RSS feed and access it in Emacs via [elfeed](https://github.com/skeeto/elfeed/) but sometimes I want to search YouTube for videos of people I don't necessarily follow (e.g. for searching a tutorial, or music, or wasting some good time) and being able to do that without switching to a browser is nice.

#### What about [helm-youtube](https://github.com/maximus12793/helm-youtube) and [ivy-youtube](https://github.com/squiter/ivy-youtube)?
First of all those packages require you to get a Google API key, while `ytel` uses the [Invidious](https://github.com/omarroth/invidious) APIs that in turn do not use the official Google APIs.

Moreover those packages are designed to select a YouTube search result and play it directly in your browser while `ytel` is really a way to collect search results in an `elfeed`-like buffer and make them accessible to the user via functions such as `ytel-get-current-video` so the user gets to decide what to to with them.
