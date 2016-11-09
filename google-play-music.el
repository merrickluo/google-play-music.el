;;; google-play-music.el --- gpmdp control              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  A.I.

;; Author: A.I. <merrick@luois.me>
;; Keywords: extensions
;; Version: 0.1

;;; Commentary:

;; hahaha

;;; Code:
(require 'websocket)
(require 'json)

(defvar google-play-music-current-track "not started")

(setq gpmdp
     (websocket-open
      "ws://localhost:5672"
      :on-message (lambda (_websocket frame)
                    (dispatch-message (websocket-frame-text frame))
                    )))

(defun dispatch-message(msg)
  (setq jmsg (let ((json-object-type 'plist))
               (json-read-from-string msg)))
  (let ((channel (plist-get jmsg :channel))
        (payload (plist-get jmsg :payload)))
    (if (string= channel "connect")
        (onConnect payload))
    (if (string= channel "track")
        (track-changed payload))))

(defun track-changed (track)
  (setq google-play-music-current-track
        (concat
         (plist-get track :title)
         "-"
         (plist-get track :artist))))

(defconst ehlo
  '(:namespace "connect" :method: "connect" :arguments '(google-play-music.el)))

(json-encode ehlo)
(defun connect()
  (websocket-send-text gpmdp (json-encode ehlo))
  )
(defun onConnect(resp)
  (message "connect: %s", resp))


(provide 'google-play-music)
;;; google-play-music.el ends here

