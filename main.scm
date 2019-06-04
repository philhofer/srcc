(module
 main
 ()
(import
 scheme
 (chicken base)
 (chicken file) ; create-temporary-directory
 (chicken process)
 (chicken process-context)
 (chicken pretty-print)
 (chicken condition)
 (optimism)
 s6-rc)

;; HACK:
;; we've linked in this code, but this
;; module doesn't load it; instead,
;; it is loaded inside of (eval ...),
;; but we don't want to search for
;; the import library at runtime
(declare (uses interface))
(include "interface.import.scm")

(define (file->list file)
  (define iter
    (lambda ()
      (let ([v (read)])
	(if (eof-object? v)
	    '()
	    (cons v (iter))))))
  (with-input-from-file file iter))

;; wrap a file in a (begin ...)
;; and import the necessary identifiers
(define (file->script file)
  `(begin
     (import interface)
     (flatten1
      ,@(file->list file))))

(define (eval-file file)
  (eval
   (file->script file)
   (interaction-environment)))

(define (usage)
  (display #<#EOF
usage: ss6c [-s] [-d destdir] [-c compiledir] files...

    -s            just emit s-expressions to stdout
    -d destdir    create an s6-rc service directory in destdir
    -c compiledir after running, run 's6-rc-compile destdir compiledir'

EOF
)
  (exit -1))

(define (assq-tail sym lst thunk)
  (let ([v (assq sym lst)])
    (if (and v (pair? v))
	(cdr v)
	(thunk))))

(define (parse-cmdline)
  (with-exception-handler
   (lambda (err)
     (usage))
   (lambda ()
     (parse-command-line
      (command-line-arguments)
      `((-s)       ; stop at s-expression expansion stage
	(-d . dir) ; destdir
	(-c . dir) ; compile dir
	((-h --help) . ,(lambda args (usage))))))))

(let* ([cmdline
	(parse-cmdline)]
       [_
	(when (eqv? (length cmdline) 1) ;; just ((--))
	  (usage))]
       [cmd-args
	(assq-tail '-- cmdline usage)]
       [destdir
	(assq-tail '-d cmdline create-temporary-directory)]
       [script-fn
	(if (assq '-s cmdline)
	    pp
	    (cut write-s6-rc-dir destdir <>))]
       [input-specs
	(foldl
	 (lambda (lst f)
	   (append lst (eval-file f)))
	 '()
	 cmd-args)])
  (for-each script-fn input-specs)
  (and-let* ([s6-rc-destdir (assq '-c cmdline)])
    (process-execute "s6-rc-compile" (list
				      (cdr s6-rc-destdir)
				      destdir))))
)
