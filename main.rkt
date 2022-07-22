;;Copyright 2022 ZhangHao
;;本程序是自由软件：你可以再分发之和/或依照由自由软件基金会发布的 GNU 通用公共许可证修改之，无论是版本 3 许可证，还是（按你的决定）任何以后版都可以。
;;发布该程序是希望它能有用，但是并无保障;甚至连可销售和符合某个特定的目的都不保证。请参看 GNU 通用公共许可证，了解详情。
;;你应该随程序获得一份 GNU 通用公共许可证的复本。如果没有，请看 <https://www.gnu.org/licenses/>。
#lang racket/base
(require db/base db/sqlite3 racket/match racket/list)
(provide table compile-pattern match-pattern match-pattern* match-in-directory save-pattern load-pattern get-longest-common-subbytes
         longest-common-subsequence-length)

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

(define get-longest-common-subbytes
  (lambda (bytes1 bytes2 #:bytes->list [bytes->list bytes->list] #:byte=? [byte=? =] #:subbytes [subbytes subbytes])
    (define list2 (bytes->list bytes2))
    (define len2 (length list2))
    (let loop ((i 0) (o (make-list len2 0)) (r null) (m null) (l1 (bytes->list bytes1)) (l2 list2))
      (cond
        ((null? l1)
         (map
          (lambda (p)
            (define end (add1 (car p)))
            (define start (- end (cdr p)))
            (subbytes bytes1 start end))
          m))
        (else
         (define-values (next-i next-l1 next-l2 next-o)
           (cond ((null? (cdr l2)) (values (add1 i) (cdr l1) list2 (cons 0 (reverse r))))
                 (else (values i l1 (cdr l2) (cdr o)))))
         (cond
           ((byte=? (car l1) (car l2))
            (define v (add1 (car o)))
            (define next-m (cond ((or (null? m) (< (cdar m) v)) (cons (cons i v) null)) ((> (cdar m) v) m) (else (cons (cons i v) m))))
            (define next-r (if (null? (cdr l2)) null (cons v r)))
            (loop next-i next-o next-r next-m next-l1 next-l2))
           (else
            (define next-r (if (null? (cdr l2)) null (cons 0 r)))
            (loop next-i next-o next-r m next-l1 next-l2))))))))

(define longest-common-subsequence-length
  (lambda (bytes1 bytes2 #:bytes->list [bytes->list bytes->list] #:byte=? [byte=? =])
    (define list2 (bytes->list bytes2))
    (define len2 (length list2))
    (let loop ((o (make-list len2 0)) (r null) (rm 0) (m 0) (l1 (bytes->list bytes1)) (l2 list2))
      (cond
        ((null? l1)
         m)
        (else
         (define state (null? (cdr l2)))
         (define-values (next-l1 next-l2 next-o)
           (cond (state (values (cdr l1) list2 (cons 0 (reverse r))))
                 (else (values l1 (cdr l2) (cdr o)))))
         (cond
           ((byte=? (car l1) (car l2))
            (define crm (max rm (car o)))
            (define v (add1 crm))
            (define next-rm (if state 0 crm))
            (define next-m (max v m))
            (define next-r (if state null (cons (max v (cadr o)) r)))
            (loop next-o next-r next-rm next-m next-l1 next-l2))
           (else
            (define next-rm (if state 0 (max rm (car o))))
            (define next-r (if state null (cons (cadr o) r)))
            (loop next-o next-r next-rm m next-l1 next-l2))))))))

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