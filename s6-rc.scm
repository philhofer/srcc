(module
 s6-rc
 (;export
  write-s6-rc-dir)

 (import
  scheme
  srfi-13        ; string-join
  (chicken base)
  (chicken file) ; create-directory
  (chicken io)   ; write-line
  list-utils)    ; for andmap

 (define (confq datum alist str . rest)
   (cdr (or (assq datum alist)
	    (apply error str rest))))
 
 ;; is this a symbol, string, or list of symbols and strings?
 (define (stringy? x)
   (let ([ok? (lambda (v) (or (string? v) (symbol? v)))])
     (or (ok? x)
	 (and (list? x)
	      (andmap ok? x)))))
 
 ;; perform a quick-and-dirty check
 ;; that the given service descriptor
 ;; is sound, or throw an error
 ;; (returns an unspecified value)
 (define (validate-desc d)

   ;; /env, if it is specified, should
   ;; also be an alist
   (define (validate-envdir d)
     (and-let* ([env (assq 'env d)])
       (or (alist? (cdr env))
	   (error "/env should be an alist"))))

   ;; bundles need /contents
   (define (validate-bundle d)
     (let ([deps (confq 'contents d "bundle needs /contents" d)])
       (or (string? deps)
	   (andmap (lambda (x) (or (string? x) (symbol? x))) deps)
	   (error "bundle needs dependencies as a list of strings or symbols"))))

   ;; oneshots needs /up and /down
   (define (validate-oneshot d)
     (validate-envdir d)
     (or (stringy? (confq 'up d "oneshot needs /up"))
	 (error "/up should be a symbol, string, or list of strings"))
     (or (stringy? (confq 'down d "oneshot needs /down"))
	 (error "/down should be a symbol, string, or list of strings")))
 
   ;; longruns needs /run
   (define (validate-longrun d)
     (validate-envdir d)
     (or (stringy? (confq 'run d "longrun needs /run"))
	 (error "/run should be a symbol, string, or list of strings")))

   (let ([typ (confq 'type d "service needs /type" d)])
     (case typ
       [(bundle)  (validate-bundle d)]
       [(oneshot) (validate-oneshot d)]
       [(longrun) (validate-longrun d)]
       [else      (error "invalid service type" typ)])))

 (define (->string datum)
   (cond
    [(string? datum) datum]
    [(symbol? datum) (symbol->string datum)]
    [(number? datum) (number->string datum)]
    [else            (error "can't easily coerce" datum "to string")]))

 (define (mkdirp! dir)
   (or (directory-exists? dir)
       (create-directory dir #t)))
 
 ;; write-s6-rc-dir writes a directory ('name')
 ;; in which the given service descriptor is
 ;; flattened from an alist into files that
 ;; are named according to the car of the alist
 ;; pair and with contents corresponding to the
 ;; cdr of the alist pair
 ;;
 ;; in other words,
 ;;
 ;;  (write-s6-rc-dir "./svc" '(dhcpcd ((type . longrun)
 ;;                                     (run . "dhcpcd -B -i eth0")))
 ;;
 ;; does what you think it does
 ;;
 (define (write-s6-rc-dir root desc)
   (validate-desc (cdr desc))
   (letrec ([write-file
	     (lambda (name contents)
	       (call-with-output-file name
		 (lambda (oport)
		   (write-line
		    (if (list? contents)
			(string-join (map ->string contents) "\n")
			(->string contents))
		   oport))))]
	    [emit-datum
	     (lambda (path elem)
	       (if (and (alist? elem)
			(not (null? elem))) ; do not create a directory for (foo)
		   (alist->dir path elem)
		   (write-file path elem)))]
	    [alist->dir
	     (lambda (path datum)
	       (let ([p (->string path)])
		 (mkdirp! p)
		 (for-each (lambda (elem)
			     (emit-datum
			      (string-append p "/" (->string (car elem)))
			      (cdr elem)))
			   datum)))])
     (alist->dir (string-append root "/" (car desc)) (cdr desc))))

)
