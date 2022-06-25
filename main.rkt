#lang racket/base
(provide table compile-pattern match-pattern match-pattern* match-in-directory file-position->lines)

(define table (make-hash))

(define compile-pattern
  (lambda (bytes)
    (let ((port (open-input-bytes bytes))
          (len (bytes-length bytes)))
      (hash-set! table #f len)
      (let loop ((state 0) (byte (read-byte port)))
        (cond ((= state len) (void))
              (else
               (hash-set! table (cons state byte) (add1 state))
               (let work ((index 1))
                 (define v (- state index))
                 (cond ((<= v 0) (void))
                       (else
                        (if (bytes=? (subbytes bytes 0 v) (subbytes bytes index (+ v index)))
                            (hash-ref! table (cons state (bytes-ref bytes v)) (add1 v))
                            (void))
                        (work (add1 index)))))
               (hash-ref! table (cons state (bytes-ref bytes 0)) 1)
               (loop (add1 state) (read-byte port))))))))

(define match-pattern
  (lambda (port)
    (define len (hash-ref table #f))
    (let loop ((state 0) (count 0))
      (cond
        ((= state len) (cons (- count len) (sub1 count)))
        ((eof-object? (peek-byte port)) #f)
        (else
         (define next (hash-ref table (cons state (read-byte port)) (lambda () #f)))
         (if next (loop next (add1 count)) (loop 0 (add1 count))))))))

(define match-pattern*
  (lambda (port)
    (let loop ((start 0) (result null))
      (cond ((match-pattern port) => (lambda (p) (loop (add1 (+ start (cdr p))) `(,@result ,(cons (+ start (car p)) (+ start (cdr p)))))))
            (else result)))))

(define match-in-directory
  (lambda (path)
    (parameterize ((current-directory path))
      (for/list ((file (in-directory)))
        (cons file (call-with-input-file file match-pattern*))))))

(define file-position->lines
  (lambda (file-position)
    (with-input-from-file (car file-position)
      (lambda ()
        (let loop ((indexes (cdr file-position))
                   (bytes (read-bytes-line))
                   (count 0)
                   (result null))
          (cond
            ((or (eof-object? bytes) (null? indexes)) result)
            (else
             (define end (+ count (bytes-length bytes)))
             (define next-indexes
               (let work ((indexes indexes))
                 (cond
                   ((null? indexes) indexes)
                   ((and (>= (caar indexes) count) (<= (cdar indexes) end))
                    (work (cdr indexes)))
                   (else indexes))))
             (loop next-indexes (read-bytes-line) (add1 end) (if (= (length next-indexes) (length indexes)) result `(,@result ,bytes))))))))))