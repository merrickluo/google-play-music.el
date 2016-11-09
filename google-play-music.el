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
                    (dispatch-message (websocket-frame-text frame)))))

(defun dispatch-message(msg)
  (setq jmsg (let ((json-object-type 'plist))
               (json-read-from-string msg)))
  (let ((channel (plist-get jmsg :channel))
        (payload (plist-get jmsg :payload)))
    (cond ((string= channel "time") nil)
          ((string= channel "connect") (connection-change payload))
          ((string= channel "track") (track-changed payload))
          (t (message "%S: %S" channel payload)))))

(defun track-changed (track)
  (setq google-play-music-current-track
        (concat
         (plist-get track :title)
         "-"
         (plist-get track :artist))))

(defun connect(&optional arg)
  (message "%S" arg)
  (let ((msg '(:namespace "connect" :method "connect")))
    (let* ((param (if (numberp arg) (number-to-string arg) arg))
          (parg (if param
                    (plist-put msg :arguments `("google-play-music.el", param))
                  (plist-put msg :arguments '("google-play-music.el")))))
      (message "%S" parg)
      (websocket-send-text gpmdp (json-encode parg)))))

(defun connection-change (resp)
  (interactive)
  (if (string= resp "CODE_REQUIRED")
      (let ((code (read-string "Enter Code: ")))
        (connect code))
    (connect resp)))

(connect)

(defun play-pause()
  (let ((msg '(:namespace "playback" :method "playPause" :requestID 1)))
    (websocket-send-text gpmdp (json-encode msg))))
(defun play-next()
  (let ((msg '(:namespace "playback" :method "forward" :requestID 1)))
    (websocket-send-text gpmdp (json-encode msg))))
(defun play-previous()
  (let ((msg '(:namespace "playback" :method "rewind" :requestID 1)))
    (websocket-send-text gpmdp (json-encode msg))))

(play-pause)
(play-next)
(play-previous)

(provide 'google-play-music)
;;; google-play-music.el ends here

