#!/usr/bin/racket

#lang racket

(require "../mpdclient/obj-main.rkt")

(define mpd (new mpd-client%))
(define conn (send mpd create-connection))
;;(displayln (send mpd mpd-list "title" "album" "Interstate 8" "artist" "Modest Mouse" ))
;;(displayln (send mpd playlist-info))
;;(displayln (send mpd pl-changes 0))

;; (displayln (send mpd command "search \"album\" \"EP\" \"artist\" \"American\""))
;; (displayln (send mpd fetch-response))


;;(send mpd parse-group "file" (send mpd list-all-info))


