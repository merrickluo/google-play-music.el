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

(defvar gpmdp-current-track "not started")
(defvar gpmdp-ws nil)
(defvar gpmdp-started nil)

(defun gpm--dispatch-message(msg)
  (setq jmsg (let ((json-object-type 'plist))
               (json-read-from-string msg)))
  (let ((channel (plist-get jmsg :channel))
        (payload (plist-get jmsg :payload)))
    (cond ((string= channel "time") nil)
          ((string= channel "connect") (gpm--connection-change payload))
          ((string= channel "track") (gpm--track-changed payload)))))

(defun gpm--track-changed (track)
  (setq gpm-current-track
        (concat
         (plist-get track :title)
         "-"
         (plist-get track :artist))))

(defun gpm-start()
  (interactive)
  (unless gpmdp-started
    (setq gpmdp-ws
          (websocket-open
           "ws://localhost:5672"
           :on-open (lambda (websocket) (setq gpmdp-started t))
           :on-close (lambda (_websocket) (setq gpmdp-started nil))
           :on-message (lambda (_websocket frame)
                         (gpm--dispatch-message (websocket-frame-text frame)))))))

(defun gpm-connect(&optional arg)
  (unless gpmdp-started
    (gpm-start))

  (let ((msg '(:namespace "connect" :method "connect")))
    (let* ((param (if (numberp arg) (number-to-string arg) arg))
          (parg (if param
                    (plist-put msg :arguments `("google-play-music.el", param))
                  (plist-put msg :arguments '("google-play-music.el")))))
      (websocket-send-text gpmdp-ws (json-encode parg)))))

(defun gpm--connection-change (resp)
  (if (string= resp "CODE_REQUIRED")
      (let ((code (read-string "Enter Code: ")))
        (gpm-connect code))
    (gpm-connect resp)))

(defun gpm-play-pause (&rest args)
  (interactive)
  (let ((msg '(:namespace "playback" :method "playPause" :requestID 1)))
    (websocket-send-text gpmdp-ws (json-encode msg))))

(defun gpm-play-next()
  (interactive)
  (let ((msg '(:namespace "playback" :method "forward" :requestID 1)))
    (websocket-send-text gpmdp-ws (json-encode msg))))

(defun gpm-play-previous()
  (interactive)
  (let ((msg '(:namespace "playback" :method "rewind" :requestID 1)))
    (websocket-send-text gpmdp-ws (json-encode msg))))

(provide 'google-play-music)
;;; google-play-music.el ends here
