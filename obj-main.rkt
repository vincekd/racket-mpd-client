#!/usr/bin/racket

;; Racket Scheme MPD Client Library
;;
;; Copyright 2012 Vincent Dumas 
;; Distributed under the GPL3 license
;; 


;; TODOs: -1) put into object
;;        2) complete command library
;;        3) pass in a function for output --
;;           read line and return it from mpd-command
;;	  4) use (byte-ready? input) to fetch data


#lang racket/base
(require racket/list racket/class racket/tcp)

;;object overwrites list?
(define racket-list list)

(define mpd-client%
  (class object%
	 (super-new)

	 ;;constants
	 (define *mpd-error-msg* "ACK ")
	 (define *error-length* (string-length *mpd-error-msg*))
	 (define *mpd-success-msg* "OK")
	 ;;(define *mpd-list-next* "list_OK")
	 (define *input* #f)
	 (define *output* #f)

	 ;;object vars
	 (define host (get-host))
	 (define port (get-port))

	 ;; Connection
	 ;; get a mpd port number
	 (define/public (get-port)
	   (define port (getenv "MPD_PORT"))
	   (if (not port) 6600
	       (string->number port)))

	 ;; get a mpd host
	 (define/public (get-host)
	   (define host (getenv "MPD_HOST"))
	   (if (not host) "localhost" host))

	 ;; create connection to mpd server
	 (define/public (create-connection . args)
	   (cond [(not (empty? args)) (begin
					(set! host (first args))
					(set! port (second args)))])
	   (with-handlers* ([exn:fail? (lambda (exn)
					 (log-error (exn-message exn))
					 #f)])
			   (let-values ([(i o) (tcp-connect host port)])
			     ;;sleep to allow time for connection
			     (sleep 1)
			     ;;make io non-blocking
			     (file-stream-buffer-mode i 'none)
			     (file-stream-buffer-mode o 'none)
			     (set! *input* i)
			     (set! *output* o)
			     (file-stream-buffer-mode *input* 'none)
			     (file-stream-buffer-mode *output* 'none)
			     ;;remove connection notice from buffer
			     (read-line i) #t)))

	 ;;close connection
	 (define/public (close-connection)
	   (cond [(input-port? *input*) (close-input-port *input*)])
	   (cond [(output-port? *output*) (close-output-port *output*)]))

	 ;;;IO
	 (define (check-error)
	   ;;check if error occured
	   (let ([msg (peek-string *error-length* 0 *input*)])
	     (cond [(and msg (equal? msg *mpd-error-msg*)) #t]
		   [else #f])))

	 (define (handle-error)
	   ;;TODO: finish this
	   ;;pull error msg from input port, parse, return (throw error?)
	   (let ([str (read-line *input*)])
	     (substring str *error-length*)))

	 ;; send command to mpd
	 (define/public (command cmd . args)
	   (if (output-port? *output*)
	       (let ([out cmd])
		 (for-each (lambda (arg)
			     (set! out (string-append out " " arg))) args)
		 (displayln out *output*)
		 (flush-output *output*)
		 (not (check-error)))
	       (begin
		 (log-error "No output port.") #f)))

	 (define (response-error str)
	   (if (< (string-length str) *error-length*) #f
	       (equal? (substring str 0 *error-length*)
		       *mpd-error-msg*)))

	 ;;-- returns list of output split by newline
	 (define/public (fetch-response)
	   (let ([str (read-line *input*)])
	     (cond [(response-error str) (racket-list str)]
		   [(equal? *mpd-success-msg* str) empty]
		   [else (append (racket-list str) (fetch-response))])))

	 ;;Utilities
	 

	 ;;-- takes in list of strings (like that produced by fetch-response)
	 ;;   and parses into pairs: ('file' . '/path/to/file'), etc.
	 (define/public (parse-response strlist)
	   (map (lambda (str)
		  (let ([halfs (regexp-split #rx": " str)])
		    (cons (first halfs) (second halfs)))) strlist))

	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMANDS - Start defining commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	 ;;play
	 (define/public (play)
	   (if (command "play") #t (handle-error)))

	 ;;play id
	 (define/public (play-id id)
	   (if (command "playid"
			(if (number? id) (number->string id) id))
	       #t (handle-error)))

	 ;;play song at position
	 (define/public (play-pos pos)
	   (if (command "play"
			(if (number? pos) (number->string pos) pos))
	        #t (handle-error)))
	 
	 ;;stop
	 (define/public (stop)
	   (if (command "stop") #t (handle-error)))

	 ;;pause
	 (define/public (pause)
	   (if (command "pause") #t (handle-error)))

	 ;;next
	 (define/public (next)
	   (if (command "next") #t (handle-error)))

	 ;;previous
	 (define/public (previous)
	   (if (command "previous") #t (handle-error)))

	 ;;repeat
	 (define/public (repeat mode)
	   (if (command "repeat"
			(if (number? mode) (number->string mode) mode))
	       #t (handle-error)))

	 ;;random
	 (define/public (random mode)
	   (if (command "random"
			(if (number? mode) (number->string mode) mode))
	       #t (handle-error)))

	 ;;shuffle
	 (define/public (shuffle)
	   (if (command "shuffle") #t (handle-error)))

	 ;;crossfade
	 (define/public (crossfade secs)
	   (command "crossfade"
		    (if (number? secs) (number->string secs) secs)))

	 ;;add
	 (define/public (add path)
	   (if (command "add" path) #t (handle-error)))

	 ;;addid
	 (define/public (add-id id)
	   (if (command "addid"
			(if (number? id) (number->string id) id))
	       #t (handle-error)))

	 ;;delete song at position in playlist
	 (define/public (delete-song pos)
	   (if (command "delete"
			(if (number? pos) (number->string pos) pos))
	       #t (handle-error)))

	 ;;delete song with id from playlist
	 (define/public (delete-songid id)
	   (if (command "deleteid"
			(if (number? id) (number->string id) id))
	       #t (handle-error)))

	 ;;current song
	 ;; returns list of strings
	 (define/public (current-song)
	   (if (command "currentsong") (fetch-response) (handle-error)))

	 ;;stats
	 ;; returns list of strings
	 (define/public (stats)
	   (if (command "stats") (fetch-response) (handle-error)))

	 ;;status
	 ;; returns list of strings
	 (define/public (status)
	   (if (command "status") (fetch-response) (handle-error)))

	 ;;find
	 ;; returns list of strings
	 (define/public (find type str)
	   (if (command "find" type str) (fetch-response) (handle-error)))

	 ;;searches type:title/artist/album/filename for str
	 ;; returns list of strings
	 (define/public (search type str)
	   (if (command "search" type str) (fetch-response) (handle-error)))

	 ;;set volume (0-100)
	 (define/public (set-volume vol)
	   (if (command "setvol"
			(if (number? vol) (number->string vol) vol))
	       #t (handle-error)))

	 ;;increment/decrement volume -- deprecated?
	 ;; (define/public (volume num)
	 ;;   (command "volume" num))

	 ;;seek -- time format?
	 (define/public (seek pos time)
	   (if (command "seek" pos time) #t (handle-error)))

	 ;;seek id -- time format?
	 (define/public (seek-id id time)
	   (if (command "seekid" id time) #t (handle-error)))

	 ;;playlist information (optionally with song position)
	 ;; returns list of strings
	 (define/public (playlist-info . pos)
	   (if (command "playlistinfo" (if (not (empty? pos)) (first pos) ""))
	       (fetch-response) (handle-error)))

	 ;;playlist information with song id
	 ;; returns list of strings
	 (define/public (playlist-id-info id)
	   (if (command "playlistid"
			(if (number? id) (number->string id) id))
	       (fetch-response) (handle-error)))

	 ;;list artists, album, etc.
	 ;; returns list of strings
	 (define/public (list type . args)
	   (if (command "list" type (if (not (empty? args)) (first args) ""))
	       (fetch-response) (handle-error)))

	 ;;list all songs in directory (dir) recursively
	 ;; returns list of strings
	 (define/public (list-all . dir)
	   (if (command "listall" (if (not (empty? dir)) (first dir) ""))
	       (fetch-response) (handle-error)))

	 ;;list all song & info in direcotry (dir) recursively
	 ;; returns list of strings
	 (define/public (list-all-info . dir)
	   (if (command "listallinfo" (if (not (empty? dir)) (first dir) ""))
	       (fetch-response) (handle-error)))

	 ;;ls (unix cmd util style) dir from db
	 ;; returns list of strings
	 (define/public (ls-info . dir)
	   (if (command "lsinfo" (if (not (empty? dir)) (first dir) ""))
	       (fetch-response) (handle-error)))

	 ;;list commands
	 ;; returns list of strings
	 (define/public (command-list)
	   (if (command "commands") (fetch-response) (handle-error)))

	 ;;clear
	 (define/public (clear)
	   (if (command "clear") #t (handle-error)))

	 ;;update
	 (define/public (update)
	   (if (command "update") #t (handle-error)))

	 ;;clear error
	 (define/public (clear-error)
	   (if (command "clearerror") #t (handle-error)))

	 ;; close mpd
	 (define/public (close)
	   (if (command "close") #t (handle-error)))

	 ;; kill mpd
	 (define/public (kill)
	   (if (command "kill") #t (handle-error)))
	 
	 ));;end mpd-client%


;;TODO- remove everything after this. Just for testing purposes


(define mpd (new mpd-client%))
(cond [(send mpd create-connection)
       (begin
	 (displayln (send mpd set-volume 50))
	 (displayln (send mpd parse-response (send mpd playlist-info))))])
;;(displayln (send mpd fetch-response))
;;(exit)

;; (define-values (input output)
;;   (create-mpd-connection (get-host) (get-port)))

;; ;;(mpd-list output "track")
;; (mpd-set-volume output "43")
;; ;; (displayln (mpd-parse-response (mpd-response input)))
;; (mpd-response input)


;; (close-connection input output)


