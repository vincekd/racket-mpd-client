#!/usr/bin/racket

#lang racket

;;(require racket/tcp)
(define listener (tcp-listen 9999 1 #f "localhost"))

(let-values ([(i o) (tcp-accept listener)])
  (read-line i))




