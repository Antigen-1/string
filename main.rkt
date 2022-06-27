#lang racket/base
(require db/base db/sqlite3 racket/match racket/list racket/generator)
(provide table compile-pattern match-pattern match-pattern* match-in-directory file-position->lines save-pattern load-pattern make-inverted-index)

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

;;Sqlite3 is necessary.
(define save-pattern
  (lambda (path [mode 'create])
    (let ((connection (sqlite3-connect #:database path #:mode mode)))
      (query-exec connection "create table pattern (
      id integer primary key not null,
      o integer,
      c integer,
      r integer
    );")
      (match table ((hash-table (#f len) ((cons o c) r) ...)
                    (query-exec connection "insert into pattern values (-1 ,$1 ,null ,null);" len)
                    (map (lambda (id o c r) (query-exec connection "insert into pattern values ($1 ,$2 ,$3 ,$4);" id o c r)) (range (length o)) o c r))))))

(define load-pattern
  (lambda (path)
    (let ((connection (sqlite3-connect #:database path #:mode 'read-only)))
      (hash-clear! table)
      (hash-set! table #f (query-value connection "select o from pattern where id = -1;"))
      (match (query-rows connection "select o, c, r from pattern where id >= 0;")
        ((list (vector o c r) ...)
         (map (lambda (o c r) (hash-set! table (cons o c) r)) o c r))))))

;;Linux only.
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

;;Unix only and sqlite3 is needed.
(define make-inverted-index
  (lambda (directory database [mode 'create])
    (let ((connection (sqlite3-connect #:database database #:mode mode))
          (generator (generator () (let loop ((id 0)) (yield id) (loop (add1 id))))))
      (query-exec connection "create table inverted_index (
        id int primary key not null,
        word int,
        path text,
        start int,
        end int
      );")
      (parameterize ((current-directory directory))
        (with-input-from-file "/usr/share/dict/words"
          (lambda ()
            (let loop ((index 0) (word (read-bytes-line)))
              (cond ((eof-object? word) (void))
                    (else
                     (hash-clear! table)
                     (compile-pattern word)
                     (match (match-in-directory ".")
                       ((list (list path (cons start end) ...) ...)
                        (map
                         (lambda (path start end)
                           (let ((path-string (path->string (path->complete-path path))))
                             (let work ((start start) (end end))
                               (if (or (null? start) (null? end))
                                   (void)
                                   (begin
                                     (query-exec
                                      connection
                                      "insert into inverted_index values ($1, $2, $3, $4, $5);"
                                      (generator) word path-string (car start) (car end))
                                     (work (cdr start) (cdr end)))))))
                         path start end)))
                     (loop (add1 index) (read-bytes-line)))))))))))