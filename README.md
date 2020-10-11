# ytel
`ytel` is an experimental YouTube "frontend" for Emacs. It's goal is to allow the user to collect the results of a YouTube search in an elfeed-like buffer and then manipulate them with Emacs Lisp. The gif below shows that `ytel` can be used to play videos in an external player, to learn how to emulate it refer to the [usage](#usage) section below.

<p align="center">
  <img src="https://github.com/gRastello/ytel/blob/master/pic/demonstration.gif">
</p>

This project was inspired by [elfeed](https://github.com/skeeto/elfeed/) and [Invidious](https://github.com/omarroth/invidious) (it does indeed use the Invidious APIs).

## Installation
This project is on [MELPA](https://melpa.org/): you should be able to `M-x package-install RET ytel`. Another option is to clone this repository under your `load-path`.

### Dependencies
While `ytel` does not depend on any Emacs package it does depend on `curl` so, if you happen not to have it, install it through your package manager (meme distros aside it is probably in your repos).

## Usage
Once everything is loaded `M-x ytel` creates a new buffer and puts it in `ytel-mode`. This major mode has just a few bindings (for now):

| key          | binding                      |
|--------------|------------------------------|
| <key>n</key> | `next-line`                  |
| <key>p</key> | `previous-line`              |
| <key>q</key> | `ytel-quit`                  |
| <key>d</key> | `ytel-rotate-date`           |
| <key>D</key> | `ytel-rotate-date-backwards` |
| <key>r</key> | `ytel-rotate-sort`           |
| <key>R</key> | `ytel-rotate-sort-backwards` |
| <key>t</key> | `ytel-display-full-title`    |
| <key>s</key> | `ytel-search`                |
| <key>></key> | `ytel-search-next-page`      |
| <key><</key> | `ytel-search-previous-page`  |

Pressing `s` will prompt for some search terms and populate the buffer once the results are available. One can access information about a video via the function `ytel-get-current-video` that returns the video at point.

You can create a buffer or file with content like that with a optional date limiter:
```
Linux date:week
Emacs date:today
```

mark a line and start ytel-region-search on them, so that you don't have to remember and don't have to manually input all your searches. Also you can keep open 1 buffer per search and operate them in parralel.

You can implement a function to stream a video in `mpv` (provided you have `youtube-dl` installed) as follows:
```elisp
(defun ytel-watch ()
    "Stream video at point in mpv."
    (interactive)
    (let* ((video (ytel-get-current-video))
     	   (id    (ytel-video-id-fun video)))
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

It is also possible to customize the sorting criterion of the results by setting the variable `ytel-sort-criterion` to one of the following symbols `relevance`, `rating`, `upload_date` or `view_count`.
The default value is `relevance`.

## Potential problems and potential fixes
`ytel` does not use the official YouTube APIs but relies on the [Invidious](https://github.com/omarroth/invidious) APIs (that in turn circumvent YouTube ones). The variable `ytel-invidious-api-url` points to the invidious instance (by default `https://invidio.us`) to use; you might not need to ever touch this, but if [invidio.us](https://invidio.us) goes down keep in mind that you can choose [another instance](https://github.com/omarroth/invidious#invidious-instances). Moreover the default Invidious instance is generally speaking stable, but sometimes your `ytel-search` might hang; in that case `C-g` and retry.

## Contributing
Feel free to open an issue or send a pull request. I'm quite new to writing Emacs packages so any help is appreciated.

## FAQ

#### Why?
One can easily subscribe to YouTube channels via an RSS feed and access it in Emacs via [elfeed](https://github.com/skeeto/elfeed/) but sometimes I want to search YouTube for videos of people I don't necessarily follow (e.g. for searching a tutorial, or music, or wasting some good time) and being able to do that without switching to a browser is nice.

#### What about [helm-youtube](https://github.com/maximus12793/helm-youtube) and [ivy-youtube](https://github.com/squiter/ivy-youtube)?
First of all those packages require you to get a Google API key, while `ytel` uses the [Invidious](https://github.com/omarroth/invidious) APIs that in turn do not use the official Google APIs.

Moreover those packages are designed to select a YouTube search result and play it directly in your browser while `ytel` is really a way to collect search results in an `elfeed`-like buffer and make them accessible to the user via functions such as `ytel-get-current-video` so the user gets to decide what to to with them.
