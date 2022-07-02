;;Copyright 2022 ZhangHao
;;本程序是自由软件：你可以再分发之和/或依照由自由软件基金会发布的 GNU 通用公共许可证修改之，无论是版本 3 许可证，还是（按你的决定）任何以后版都可以。
;;发布该程序是希望它能有用，但是并无保障;甚至连可销售和符合某个特定的目的都不保证。请参看 GNU 通用公共许可证，了解详情。
;;你应该随程序获得一份 GNU 通用公共许可证的复本。如果没有，请看 <https://www.gnu.org/licenses/>。
#lang racket/base
(require db/base db/sqlite3 racket/match racket/list)
(provide table compile-pattern match-pattern match-pattern* match-in-directory file-position->lines save-pattern load-pattern getLongestSubbytes)

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

(define getLongestSubbytes
  (lambda (bytes1 bytes2 #:char=? [char=? char=?])
    (define len1 (bytes-length bytes1))
    (define len2 (bytes-length bytes2))
    (let loop ((i 0) (j 0) (result null))
      (define state1 (= len1 (add1 i)))
      (define state2 (= len2 (add1 j)))
      (define-values (next-i next-j)
        (cond ((and state1 state2) (values #f #f))
              (state2 (values (add1 i) 0))
              (else (values i (add1 j)))))
      (cond
        ((and (not next-i) (not next-j))
         (define maximum (apply max result))
         (map
          (lambda (index) (list maximum (quotient index len2) (remainder index len2)))
          (indexes-of result maximum =)))
        ((char=? (bytes-ref bytes1 i) (bytes-ref bytes2 j))
         (if (or (< (sub1 i) 0) (< (sub1 j) 0))
             (loop next-i next-j `(,@result 1))
             (loop next-i next-j `(,@result (add1 (list-ref result (+ (* len2 (sub1 i)) j)))))))
        (else (loop next-i next-j `(,@result 0)))))))

;;Sqlite3 is necessary.
(define save-pattern
  (lambda (path [table-name "pattern"] [mode 'create])
    (let ((connection (sqlite3-connect #:database path #:mode mode)))
      (query-exec connection (format "create table ~a (
      id integer primary key not null,
      o integer,
      c integer,
      r integer
    );"
                                     table-name))
      (match table ((hash-table (#f len) ((cons o c) r) ...)
                    (query-exec connection (format "insert into ~a values (-1 ,$1 ,null ,null);" table-name) len)
                    (map
                     (lambda (id o c r)
                       (query-exec connection (format "insert into ~a values ($1 ,$2 ,$3 ,$4);" table-name) id o c r))
                     (range (length o)) o c r))))))

(define load-pattern
  (lambda (path [table-name "pattern"])
    (let ((connection (sqlite3-connect #:database path #:mode 'read-only)))
      (hash-clear! table)
      (hash-set! table #f (query-value connection (format "select o from ~a where id = -1;" table-name)))
      (match (query-rows connection (format "select o, c, r from ~a where id >= 0;" table-name))
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