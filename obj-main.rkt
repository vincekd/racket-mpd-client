#!/usr/bin/racket

#lang racket/base
(require racket/class racket/tcp)

(define mpd-connection%
  (class object%
		 (super-new)
		 (field host)
		 (field port)
		 
		 ;; get mpd port number
		 (define (get-port)
		   (if port port
			   (begin
				 (define port (getenv "MPD_PORT"))
				 (cond [(not port) 6600]
					   [else (string->number port)]))))

		 ;; get mpd host
		 (define (get-host)
		   (if host host
			   (begin
				 (define host (getenv "MPD_HOST"))
				 (cond [(not host) "localhost"]
					   [else host]))))

		 ;; create connection to mpd server
		 (define (create-mpd-connection host port)
		   (with-handlers* ([exn:fail? (lambda (exn)
										 (log-error (exn-message exn))
										 (values #f #f))])
						   (let-values ([(i o) (tcp-connect host port)])
							 ;;make non-blocking
							 (file-stream-buffer-mode i 'none)
							 (file-stream-buffer-mode o 'line)
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
				 (write cmd o)
				 (newline o)
				 (flush-output o))
			   (begin
				 (log-error "No output port.")
				 #f)))

		 ;;COMMANDS - Start defining commands

		 ));;end mpd-connection%

;(exit)
;;TODO- remove everything after this. Just for testing purposes
;; (define-values (input output)
;;   (create-mpd-connection (get-host) (get-port)))

;; (mpd-command output (quote pause))

;; (close-connection input output)


