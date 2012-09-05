#!/usr/bin/racket

#lang racket/base
(require racket/list racket/tcp)

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
  (with-handlers* ([exn:fail? (lambda (exn)
								(log-error (exn-message exn))
								(values #f #f))])
				  (let-values ([(i o) (tcp-connect host port)])
					;;make non-blocking
					(file-stream-buffer-mode i 'none)
					(file-stream-buffer-mode o 'line)
					(read-line i)
					(sleep 1)
					(values i o))))

;;close connection
(define (close-connection i o)
  (cond [(input-port? i) (close-input-port i)])
  (cond [(output-port? o) (close-output-port o)]))

;; send command to mpd
(define (mpd-command o cmd . args)
  (if (output-port? o)
	  (begin
		(display cmd o)
		(for-each (lambda (arg)
					(display #\space o)
					(display arg o)) args)
		(newline o)
		(flush-output o))
	  (begin
		(log-error "No output port.")
		#f)))

(define (mpd-response i)
  (let ([str (read-line i)])
	(if (equal? "OK" str) empty
		(append (list str) (mpd-response i)))))

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
  (mpd-command o "playid" id))

;;play song at position
(define (mpd-play-pos o pos)
  (mpd-command o "play" pos))

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
  (mpd-command o "repeat" mode))

;;random
(define (mpd-random o mode)
  (mpd-command o "random" mode))

;;shuffle
(define (mpd-shuffle o)
  (mpd-command o "shuffle"))

;;crossfade
(define (mpd-crossfade o secs)
  (mpd-command o "crossfade" secs))

;;add
(define (mpd-add o path)
  (mpd-command o "add" path))

;;addid
(define (mpd-add-id o id)
  (mpd-command o "addid" id))

;;delete song at position in playlist
(define (mpd-delete-song o pos)
  (mpd-command o "delete" pos))

;;delete song with id from playlist
(define (mpd-delete-songid o id)
  (mpd-command o "deleteid" id))

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
  (mpd-command o "setvol" vol))

;;increment/decrement volume -- deprecated?
;; (define (mpd-volume o num)
;;   (mpd-command o "volume" num))

;;seek
(define (mpd-seek o pos time)
  (mpd-command o "seek" pos time))

;;seek id
(define (mpd-seek-id o id time)
  (mpd-command o "seekid" id time))

;;update
(define (mpd-update o)
  (mpd-command o "update"))

;;clear
(define (mpd-clear o)
  (mpd-command o "clear"))

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

										;(exit)
;;TODO- remove everything after this. Just for testing purposes
(define-values (input output)
  (create-mpd-connection (get-host) (get-port)))

(mpd-command-list output)
(mpd-parse-response (mpd-response input))


(close-connection input output)


