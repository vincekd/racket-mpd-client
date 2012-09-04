#!/usr/bin/racket

#lang racket/base
(require racket/tcp)

;; get mpd port number
(define (get-port)
  (define port (getenv "MPD_PORT"))
  (cond [(not port) 6600]
		[else (string->number port)]))

;; get mpd host
(define (get-host)
  (define host (getenv "MPD_HOST"))
  (cond [(not host) "localhost"]
		[else host]))

;; create connection to mpd server
(define (create-mpd-connection host port)
  (tcp-connect host port))

;; send command to mpd
(define (mpd-command output cmd)
  (write cmd output)
  (newline output))

(define-values (input output)
  (create-mpd-connection (get-host) (get-port)))

;;(tcp-addresses output)

(mpd-command output "stop")


