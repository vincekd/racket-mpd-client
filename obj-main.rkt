#!/usr/bin/racket

;; Racket Scheme MPD Client Library
;;
;; Copyright 2012 Vincent Dumas 
;; Distributed under the GPLv3 license
;; 


;; TODOs: -
;;        1) complete command library
;;        http://mpd.wikia.com/wiki/MusicPlayerDaemonCommands
;;


#lang racket/base
(require racket/list racket/class racket/tcp)
(provide mpd-client%)

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
			     (set! *input* i)
			     (set! *output* o)
			     (file-stream-buffer-mode *input* 'none)
			     (file-stream-buffer-mode *output* 'none)
			     ;;remove connection notice from buffer
			     (response-error (read-line i)))))

	 ;;close connection
	 (define/public (close-connection)
	   (cond [(input-port? *input*) (close-input-port *input*)])
	   (cond [(output-port? *output*) (close-output-port *output*)]))

	 ;;;IO
	 (define (check-error)
	   ;;check if error occured
	   (define msg (string->bytes/utf-8 *mpd-error-msg*))
	   (peek-bytes-avail! msg 0 (port-progress-evt *input*)
			      *input* 0 (bytes-length msg))
	   (set! msg (bytes->string/utf-8 msg))
	   (cond [(and msg (equal? msg *error-length*)) #t]
		 [else #f]))
	     ;; (cond [(and msg (equal? msg *mpd-error-msg*)) #t]
	     ;; 	   [else #f])))

	 (define (handle-error)
	   ;;TODO: finish this
	   ;;pull error msg from input port, parse, return (throw error?)
	   (let ([str (read-line *input*)])
	     (displayln str)
	     (substring str *error-length*)))

	 ;; send command to mpd
	 (define/public (command cmd)
	   (if (output-port? *output*)
	       (begin 
		 ;;(displayln cmd)
		 (displayln cmd *output*)
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
	 ;;returns a list of strings, sorted alphabetically
	 (define/public (parse-response-list strlist)
	   (sort (filter (lambda (str)
		     (not (equal? str "")))
		     (map (lambda (str)
			  (second (regexp-split #rx": " str))) strlist))
		 string<?))

	 ;;parse groups of data
	 (define/public (parse-group delimiter inl)
	   (define newlist empty)
	   (define currhash empty)
	   (for-each (lambda (e)
		       (let ([splits (regexp-split #rx": " e)])
			 (when (equal? delimiter (first splits))
			       (begin
				 (when (not (list? currhash))
				       (set! newlist (append newlist
							     (list currhash))))
				 (set! currhash (make-hash))))
			 (hash-set! currhash (first splits) (second splits))))
		     inl) (append newlist (list currhash)))

	 ;;takes list of hashes as generated above and gets a track listing
	 ;; (define/public (get-track-list hl)
	 ;;   (map (lambda (h)
	 ;; 	  (string-append (hash-ref h "Track") " "
	 ;; 			 (hash-ref h "Title"))) hl))


	 ;;-- takes in list of strings (like that produced by fetch-response)
	 ;;   and parses into hash: ('file' . '/path/to/file'), etc.
	 (define/public (parse-response strlist)
	   (make-hash (map (lambda (str)
			     (define splits (regexp-split #rx": " str))
			     (cons (first splits) (second splits))) strlist)))

	 (define (sanitize any)
	   (cond [(number? any) (number->string any)]
		 [else any]))

	 ;;returns a string of the arguments or #f
	 (define (get-cmd-string cmd args)
	   (cond [(not (list? args)) (string-append cmd " \""
						    (sanitize args) "\"")]
		 [(empty? args) cmd]
		 [else 
		  (let ([out cmd])
		    (for-each
		     (lambda (arg)
		       (set! out (string-append out " \""
						(sanitize arg) "\""))) args)
		    out)]))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMANDS - Start defining commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	 ;;play
	 (define/public (play)
	   (if (command "play") #t (handle-error)))

	 ;;play id
	 (define/public (play-id id)
	   (if (command (get-cmd-string "playid" id)) #t (handle-error)))

	 ;;play song at position
	 (define/public (play-pos pos)
	   (if (command (get-cmd-string "play" pos)) #t (handle-error)))
	 
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
	   (if (command (get-cmd-string "repeat" mode)) #t (handle-error)))

	 ;;random
	 (define/public (random mode)
	   (if (command (get-cmd-string "random" mode)) #t (handle-error)))

	 ;;shuffle
	 (define/public (shuffle)
	   (if (command "shuffle") #t (handle-error)))

	 ;;crossfade
	 (define/public (crossfade secs)
	   (if (command (get-cmd-string "crossfade" secs)) #t (handle-error)))

	 ;;add
	 (define/public (add path)
	   (if (command (get-cmd-string "add" path))
	       (fetch-response) (handle-error)))

	 ;;addid
	 (define/public (add-id id)
	   (if (command (get-cmd-string "addid" id))
	       (fetch-response) (handle-error)))

	 ;;delete song at position in playlist
	 (define/public (delete-song pos)
	   (if (command (get-cmd-string "delete" pos)) #t (handle-error)))

	 ;;delete song with id from playlist
	 (define/public (delete-songid id)
	   (if (command (get-cmd-string "deleteid" id)) #t (handle-error)))

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
	 (define/public (find type str . args)
	   (if (command (get-cmd-string "find" (append (list type str) args)))
	       (fetch-response) (handle-error)))

	 ;;searches type:title/artist/album/filename for str
	 ;; returns list of strings
	 (define/public (search type str . args)
	   (if (command (get-cmd-string "search" (append (list type str) args)))
	       (fetch-response) (handle-error)))

	 ;;set volume (0-100)
	 (define/public (set-volume vol)
	   (if (command (get-cmd-string "setvol" vol)) #t (handle-error)))

	 ;;increment/decrement volume -- deprecated?
	 ;; (define/public (volume num)
	 ;;   (command "volume" num))

	 ;;seek -- time format?
	 (define/public (seek pos time)
	   (if (command (get-cmd-string "seek" (list pos time)))
	       #t (handle-error)))

	 ;;seek id -- time format?
	 (define/public (seek-id id time)
	   (if (command (get-cmd-string "seekid" (list id time)))
	       #t (handle-error)))

	 ;;playlist information (optionally with song position)
	 ;; returns list of strings
	 (define/public (playlist-info . pos)
	   (if (command (get-cmd-string "playlistinfo" pos))
	       (fetch-response) (handle-error)))

	 ;;playlist information with song id
	 ;; returns list of strings
	 (define/public (playlist-id-info id)
	   (if (command (get-cmd-string "playlistid" id))
	       (fetch-response) (handle-error)))

	 ;;list artists, album, etc.
	 ;; returns list of strings
	 (define/public (mpd-list type . args)
	   (if (command (get-cmd-string "list" (append (list type) args)))
	       (fetch-response) (handle-error)))

	 ;;list all songs in directory (dir) recursively
	 ;; returns list of strings
	 (define/public (list-all . dir)
	   (if (command (get-cmd-string "listall" dir))
	       (fetch-response) (handle-error)))

	 ;;list all song & info in direcotry (dir) recursively
	 ;; returns list of strings
	 (define/public (list-all-info . dir)
	   (if (command (get-cmd-string "listallinfo" dir))
	       (fetch-response) (handle-error)))

	 ;;ls (unix cmd util style) dir from db
	 ;; returns list of strings
	 (define/public (ls-info . dir)
	   (if (command (get-cmd-string "lsinfo" dir))
	       (fetch-response) (handle-error)))

	 ;;gets playlist changes since version (plv)
	 ;; returns list of strings
	 (define/public (pl-changes . plv)
	   (if (command (get-cmd-string "plchanges" plv))
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

	 (define/public (ping)
	   (if (command "ping") (fetch-response) (handle-error)))

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



