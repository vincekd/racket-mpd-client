#!/usr/bin/racket

;; Racket Scheme MPD Client Library
;;
;; Copyright 2012 Vincent Dumas 
;; Distributed under the GPL3 license
;; 


;; TODOs: 1) put into object
;;        2) complete command library
;;        3) pass in a function for output --
;;           read line and return it from mpd-command



#lang racket/base
(require racket/list racket/tcp)

;;constants
(define *mpd-error-msg* "ACK ")
(define *mpd-success-msg* "OK")
(define *mpd-list-next* "list_OK")


;; get a mpd port number
(define (get-port)
  (define port (getenv "MPD_PORT"))
  (if (not port) 6600
      (string->number port)))

;; get a mpd host
(define (get-host)
  (define host (getenv "MPD_HOST"))
  (if (not host) "localhost" host))

;; create connection to mpd server
(define (create-mpd-connection host port)
  (with-handlers* ([exn:fail? (lambda (exn)
				(log-error (exn-message exn))
				(values #f #f))])
		  (let-values ([(i o) (tcp-connect host port)])
		    ;;sleep to allow time for connection
		    (sleep 1)
		    ;;make io non-blocking
		    (file-stream-buffer-mode i 'none)
		    (file-stream-buffer-mode o 'none)
		    ;;remove connection notice from buffer
		    (read-line i)
		    (values i o))))

;;close connection
(define (close-connection i o)
  (cond [(input-port? i) (close-input-port i)])
  (cond [(output-port? o) (close-output-port o)]))

;; send command to mpd
(define (mpd-command o cmd . args)
  (if (output-port? o)
      (let ([out cmd])
	(for-each (lambda (arg)
		    (set! out (string-append out " " arg))) args)
	(displayln out o)
	(flush-output o))
      (begin
	(log-error "No output port.") #f)))

(define (mpd-response-error str)
  (if (< (string-length str) (string-length *mpd-error-msg*)) #f
      (equal? (substring str 0 (string-length *mpd-error-msg*))
	      *mpd-error-msg*)))

(define (mpd-response i)
  (let ([str (read-line i)])
    (if (mpd-response-error str) (list str)
	(if (equal? *mpd-success-msg* str) empty
	    (append (list str) (mpd-response i))))))

(define (mpd-parse-response strlist)
  (map (lambda (str)
	 (let ([halfs (regexp-split #rx": " str)])
	   (cons (first halfs) (second halfs)))) strlist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;COMMANDS - Start defining commands;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;play
(define (mpd-play o)
  (mpd-command o "play"))

;;play id
(define (mpd-play-id o id)
  (mpd-command o "playid"
	       (if (number? id) (number->string id) id)))

;;play song at position
(define (mpd-play-pos o pos)
  (mpd-command o "play"
	       (if (number? pos) (number->string pos) pos)))

;;stop
(define (mpd-stop o)
  (mpd-command 0 "stop"))

;;pause
(define (mpd-pause o)
  (mpd-command o "pause"))

;;next
(define (mpd-next o)
  (mpd-command o "next"))

;;previous
(define (mpd-previous o)
  (mpd-command o "previous"))

;;repeat
(define (mpd-repeat o mode)
  (mpd-command o "repeat"
	       (if (number? mode) (number->string mode) mode)))

;;random
(define (mpd-random o mode)
  (mpd-command o "random"
	       (if (number? mode) (number->string mode) mode)))

;;shuffle
(define (mpd-shuffle o)
  (mpd-command o "shuffle"))

;;crossfade
(define (mpd-crossfade o secs)
  (mpd-command o "crossfade"
	       (if (number? secs) (number->string secs) secs)))

;;add
(define (mpd-add o path)
  (mpd-command o "add" path))

;;addid
(define (mpd-add-id o id)
  (mpd-command o "addid"
	       (if (number? id) (number->string id) id)))

;;delete song at position in playlist
(define (mpd-delete-song o pos)
  (mpd-command o "delete"
	       (if (number? pos) (number->string pos) pos)))

;;delete song with id from playlist
(define (mpd-delete-songid o id)
  (mpd-command o "deleteid"
	       (if (number? id) (number->string id) id)))

;;current song
(define (mpd-current-song o)
  (mpd-command o "currentsong"))

;;stats
(define (mpd-stats o)
  (mpd-command o "stats"))

;;status
(define (mpd-status o)
  (mpd-command o "status"))

;;find
(define (mpd-find o type str)
  (mpd-command o "find" type str))

;;searches type:title/artist/album/filename for str
(define (mpd-search o type str)
  (mpd-command o "search" type str))

;;set volume (0-100)
(define (mpd-set-volume o vol)
  (mpd-command o "setvol"
	       (if (number? vol) (number->string vol) vol)))

;;increment/decrement volume -- deprecated?
;; (define (mpd-volume o num)
;;   (mpd-command o "volume" num))

;;seek -- time format?
(define (mpd-seek o pos time)
  (mpd-command o "seek" pos time))

;;seek id -- time format?
(define (mpd-seek-id o id time)
  (mpd-command o "seekid" id time))

;;playlist information (optionally with song position)
(define (mpd-playlist-info o . pos)
  (mpd-command o "playlistinfo" (cond [(not (empty? pos)) (first pos)]
				      [else ""])))

;;playlist information with song id
(define (mpd-playlist-id-info o id)
  (mpd-command o "playlistid" o
	       (if (number? id) (number->string id) id)))

;;list artists, album, etc.
(define (mpd-list o type . args)
  (mpd-command o "list" type (cond [(not (empty? args)) (first args)]
				   [else ""])))

;;list all songs in directory (dir) recursively
(define (mpd-list-all o . dir)
  (mpd-command o "listall" (cond [(not (empty? dir)) (first dir)]
				 [else ""])))

;;list all song & info in direcotry (dir) recursively
(define (mpd-list-all-info o . dir)
  (mpd-command o "listallinfo" (cond [(not (empty? dir)) (first dir)]
				     [else ""])))

;;ls (unix cmd util style) dir from db
(define (mpd-ls-info o . dir)
  (mpd-command o "lsinfo" (cond [(not (empty? dir)) (first dir)]
				[else ""])))

;;clear
(define (mpd-clear o)
  (mpd-command o "clear"))

;;update
(define (mpd-update o)
  (mpd-command o "update"))

;;clear error
(define (mpd-clear-error o)
  (mpd-command o "clearerror"))

;; close mpd
(define (mpd-close o)
  (mpd-command o "close"))

;; kill mpd
(define (mpd-kill o)
  (mpd-command o "kill"))

;;list commands
(define (mpd-command-list o)
  (mpd-command o "commands"))


;;TODO- remove everything after this. Just for testing purposes

;; (exit)

(define-values (input output)
  (create-mpd-connection (get-host) (get-port)))

;;(mpd-list output "track")
(mpd-set-volume output "43")
;; (displayln (mpd-parse-response (mpd-response input)))
(mpd-response input)


(close-connection input output)


