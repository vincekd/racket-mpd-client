#!/usr/bin/racket

#lang racket/base
(require racket/list racket/class racket/tcp)

;;object overwrites list?
(define racket-list list)

(define mpd-client%
  (class object%
	 (super-new)

	 ;;constants
	 (define *mpd-error-msg* "ACK ")
	 (define *mpd-success-msg* "OK")
	 (define *mpd-list-next* "list_OK")
	 (define *input* #f)
	 (define *output* #f)

	 ;;object vars
	 (define host (get-host))
	 (define port (get-port))


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

	 ;; send command to mpd
	 (define/public (command cmd . args)
	   (if (output-port? *output*)
	       (let ([out cmd])
		 (for-each (lambda (arg)
			     (set! out (string-append out " " arg))) args)
		 (displayln out *output*)
		 (flush-output *output*))
	       (begin
		 (log-error "No output port.") #f)))

	 (define (response-error str)
	   (if (< (string-length str) (string-length *mpd-error-msg*)) #f
	       (equal? (substring str 0 (string-length *mpd-error-msg*))
		       *mpd-error-msg*)))

	 (define/public (fetch-response)
	   (let ([str (read-line *input*)])
	   (cond [(response-error str) (racket-list str)]
	   	 [(equal? *mpd-success-msg* str) empty]
	   	 [else (append (racket-list str) (fetch-response))])))

	 (define/public (parse-response strlist)
	   (map (lambda (str)
		  (let ([halfs (regexp-split #rx": " str)])
		    (cons (first halfs) (second halfs)))) strlist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;COMMANDS - Start defining commands;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	 ;;play
	 (define/public (play)
	   (command "play"))

	 ;;play id
	 (define/public (play-id id)
	   (command "playid"
		    (if (number? id) (number->string id) id)))

	 ;;play song at position
	 (define/public (play-pos pos)
	   (command "play"
		    (if (number? pos) (number->string pos) pos)))
	 
	 ;;stop
	 (define/public (stop)
	   (command "stop"))

	 ;;pause
	 (define/public (pause)
	   (command "pause"))

	 ;;next
	 (define/public (next)
	   (command "next"))

	 ;;previous
	 (define/public (previous)
	   (command "previous"))

	 ;;repeat
	 (define/public (repeat mode)
	   (command "repeat"
		    (if (number? mode) (number->string mode) mode)))

	 ;;random
	 (define/public (random mode)
	   (command "random"
		    (if (number? mode) (number->string mode) mode)))

	 ;;shuffle
	 (define/public (shuffle)
	   (command "shuffle"))

	 ;;crossfade
	 (define/public (crossfade secs)
	   (command "crossfade"
		    (if (number? secs) (number->string secs) secs)))

	 ;;add
	 (define/public (add path)
	   (command "add" path))

	 ;;addid
	 (define/public (add-id id)
	   (command "addid"
		    (if (number? id) (number->string id) id)))

	 ;;delete song at position in playlist
	 (define/public (delete-song pos)
	   (command "delete"
		    (if (number? pos) (number->string pos) pos)))

	 ;;delete song with id from playlist
	 (define/public (delete-songid id)
	   (command "deleteid"
		    (if (number? id) (number->string id) id)))

	 ;;current song
	 (define/public (current-song)
	   (command "currentsong"))

	 ;;stats
	 (define/public (stats)
	   (command "stats"))

	 ;;status
	 (define/public (status)
	   (command "status"))

	 ;;find
	 (define/public (find type str)
	   (command "find" type str))

	 ;;searches type:title/artist/album/filename for str
	 (define/public (search type str)
	   (command "search" type str))

	 ;;set volume (0-100)
	 (define/public (set-volume vol)
	   (command "setvol"
		    (if (number? vol) (number->string vol) vol)))

	 ;;increment/decrement volume -- deprecated?
	 ;; (define/public (volume num)
	 ;;   (command "volume" num))

	 ;;seek -- time format?
	 (define/public (seek pos time)
	   (command "seek" pos time))

	 ;;seek id -- time format?
	 (define/public (seek-id id time)
	   (command "seekid" id time))

	 ;;playlist information (optionally with song position)
	 (define/public (playlist-info . pos)
	   (command "playlistinfo" (if (not (empty? pos)) (first pos) "")))

	 ;;playlist information with song id
	 (define/public (playlist-id-info id)
	   (command "playlistid"
			(if (number? id) (number->string id) id)))

	 ;;list artists, album, etc.
	 (define/public (list type . args)
	   (command "list" type (if (not (empty? args)) (first args) "")))

	 ;;list all songs in directory (dir) recursively
	 (define/public (list-all . dir)
	   (command "listall" (if (not (empty? dir)) (first dir) "")))

	 ;;list all song & info in direcotry (dir) recursively
	 (define/public (list-all-info . dir)
	   (command "listallinfo" (if (not (empty? dir)) (first dir) "")))

	 ;;ls (unix cmd util style) dir from db
	 (define/public (ls-info . dir)
	   (command "lsinfo" (if (not (empty? dir)) (first dir) "")))

	 ;;clear
	 (define/public (clear)
	   (command "clear"))

	 ;;update
	 (define/public (update)
	   (command "update"))

	 ;;clear error
	 (define/public (clear-error)
	   (command "clearerror"))

	 ;; close mpd
	 (define/public (close)
	   (command "close"))

	 ;; kill mpd
	 (define/public (kill)
	   (command "kill"))

	 ;;list commands
	 (define/public (command-list)
	   (command "commands"))



	 
	 ));;end mpd-client%

;;TODO- remove everything after this. Just for testing purposes


(define mpd (new mpd-client%))
(send mpd create-connection)
(send mpd playlist-info)
(displayln (send mpd fetch-response))
;;(exit)

;; (define-values (input output)
;;   (create-mpd-connection (get-host) (get-port)))

;; ;;(mpd-list output "track")
;; (mpd-set-volume output "43")
;; ;; (displayln (mpd-parse-response (mpd-response input)))
;; (mpd-response input)


;; (close-connection input output)


