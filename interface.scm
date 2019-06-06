(module
 interface
 (;export
  bundle
  flatten1
  loopback
  wireless
  ether
  bridge
  l3-static
  net-longrun
  l3-auto
  l2-auto
  hotplug
  coldplug)
 (import
  scheme
  srfi-1
  list-utils
  (chicken format)
  (chicken base)
  (chicken module))

 ;; interfaces are represented as alists
 ;; that have at least the following fields:
 ;;
 ;;  name: the name of the interface
 ;;  type: the type of the interface
 ;;  l1, l2, l3: if present, functions like (fn name options)
 ;;              that produce s6-rc service definitions for
 ;;              each layer of the network stack
 ;;

 ;; join a series of lines together for an execline
 ;; script such that all but the last line are wrapped
 ;; with 'if { line }'
 (define (execline-join lines)
   (cons "#!/bin/execlineb -P"
	 (if (string? lines)
	     (list lines)
	     (let loop ([out '()]
			[rest lines])
	       (cond
		[(null? rest) out]
		[(null? (cdr rest)) (reverse (cons (car rest) out))]
		[else (loop (cons (format "if { ~a }" (car rest)) out)
			    (cdr rest))])))))

 ;; (ifopt sym alist expr)
 ;; produces either (cdr (assq sym alist))
 ;; lazily computes the value of 'expr'
 ;; when there is no associated alist entry
 ;; (analagous to alist-ref with a lazily-evaulated default)
 (define-syntax ifopt
   (syntax-rules ()
     ((_ sym opts otherwise)
      (let ([val (assq sym opts)])
	(if val (cdr val) otherwise)))))

 (define (ifopt/null sym opts)
   (ifopt sym opts '()))
 
 (define (l2-oneshot up-fn down-fn)
   (lambda (name options)
     `((type . oneshot)
       (up . ,(execline-join (up-fn name options)))
       (down . ,(execline-join (down-fn name options)))
       (dependencies . ,(flatten
			 (format "~a.l1" name)
			 (ifopt/null 'l2-depends options))))))

 ;; generate the tail of a config given
 ;; default values for the l{1,2,3} services
 (define (config-from name options l1 l2 l3)
   `((l1 . ,((ifopt 'l1-config options l1) name options))
     (l2 . ,((ifopt 'l2-config options l2) name options))
     (l3 . ,((ifopt 'l3-config options l3) name options))))

 ;; accepted options:
 ;;   (route . (<prefix> . <spec>)...)
 ;; e.g.
 ;;   (route . (default . "via 192.168.0.1 metric 50")
 ;;            ("192.168.3.0/24" . "via 192.168.3.1"))
 (define (l3-with-options up down name options)
   ;; TODO: support more options than just 'route
   (let ([routes   (ifopt 'route options #f)]
	 [uptail   (up name options)]
	 [downtail (down name options)])
     (values
      (flatten ; up
       (if routes
	   (map
	    (lambda (rt) (format "ip route add ~a dev ~a ~a" (car rt) name (cdr rt)))
	    routes)
	   '())
       uptail)
      (flatten ; down
       (if routes
	   (map
	    (lambda (rt) (format "ip route del ~a dev ~a" (car rt) name))
	    routes)
	   '())
       downtail))))
 
 (define (l3-oneshot up-fn down-fn)
   (lambda (name options)
     (let-values ([(upln downln)
		   (l3-with-options up-fn down-fn name options)])
       `((type . oneshot)
	 (up . ,(execline-join upln))
	 (down . ,(execline-join downln))
	 (dependencies . ,(cons
			   (format "~a.l2" name)
			   (ifopt 'l3-depends options '())))))))
 
 ;; l3-static returns an l3 function that
 ;; applies a list of static addresses to an interface
 (define (l3-static . addrs)
   (l3-oneshot
    (lambda (name options)
      (map (cut format "ip addr replace ~a dev ~a valid_lft forever" <> name) addrs))
    (lambda (name options)
      (map (cut format "ip addr del ~a dev ~a" <> name) addrs))))

 ;; l2-auto just calls 'ip link set dev ... {up,down}'
 (define l2-auto
   (l2-oneshot
    (lambda (name options)
      (format "ip link set dev ~a up" name))
    (lambda (name options)
      (format "ip link set dev ~a down" name))))

 ;; coldplugged devices get a placeholder service
 ;; that fails to come up if the device doesn't coldplug
 (define (coldplug name options)
   `((type . oneshot)
     (up . ,(format "test -d /sys/class/net/~a" name))
     (down . "/bin/true")))

 (define (hotplug name options)
   ;; FIXME: implement hot-plug detection
   `((type . oneshot)
     (up . (format "test -d /sys/class/net/~a" name))
     (down . "/bin/true")))

 (define (loopback . options)
   (net->services
    (let ([name (ifopt 'name options "lo")])
      `((name . ,name)
	(type . loopback)
	,@(config-from
	   name
	   options
	   coldplug
	   l2-auto
	   (l3-static "127.0.0.1/8 scope host" "::1/128 scope host"))))))

 (define (net-longrun level up-fn down-fn)
   (lambda (name options)
     `((type . longrun)
       (run . ,(execline-join (up-fn name options)))
       (finish . ,(execline-join (down-fn name options)))
       (dependencies . ,(case level
			  [(l1) (ifopt 'l1-depends options '())]
			  [(l2) (cons (format "~a.l1" name)
				      (ifopt 'l2-depends options '()))]
			  [(l3) (cons (format "~a.l2" name)
				      (ifopt 'l3-depends options '()))])))))

 (define empty-script (lambda (name options) '()))
 
 (define l2-default-wpa
   ;; TODO: think about not hard-coding the path
   ;; to the config file here; could get it from options...
   (net-longrun
    'l2
    (lambda (name options)
      (list
       "fdmove -c 2 1"
       (format "wpa_supplicant -c /etc/wpa_supplicant/wpa_supplicant.conf -i ~a" name)))
    empty-script))

 ;; 'automatic' level 3 configuration
 ;; (use dhcpcd for dhcpv4+slaac)
 (define l3-auto
   (net-longrun
    'l3
    (lambda (name options)
      (list
       "fdmove -c 2 1"
       (format "dhcpcd -B ~a" name)))
   (lambda (name options)
      (format "ip addr flush dev ~a scope global" name))))

 ;; wireless creates a managed wireless interface
 ;; (presently, using wpa_supplicant and dhcpcd)
 (define (wireless name . options)
   (net->services
    `((name . ,name)
      (type . wireless)
      ,@(config-from
	 name
	 options
	 coldplug
	 l2-default-wpa
	 l3-auto))))

 ;; ether creates a managed wired interface
 (define (ether name . options)
   (net->services
    `((name . ,name)
      (type . hwether)
      ,@(config-from
	 name
	 options
	 coldplug
	 l2-auto
	 l3-auto))))

 (define (iface-name iface)
   (ifopt 'name iface (error "interface has no name?")))

 ;; emit all the services for an interface
 (define (net->services iface)
   (let ([name  (iface-name iface)]
	 [inner (net->level-services iface)])
     (cons
      `(,name . ((type . bundle)
		 (contents . ,(if (null? inner) '() (car (car inner))))))
      inner)))

 ;; for an interface, emit the iface.l1, iface.l2, and iface.l3 services
 (define (net->level-services iface)
   (let* ([name       (iface-name iface)]
	  [stages    '(l1 l2 l3)]
	  [stage-name (lambda (stage)
			(format "~a.~a" name (symbol->string stage)))])
     (foldl (lambda (lst stage)
	      (let ([v (assq stage iface)])
		(if (or (not v) (not (cdr v)))
		    lst
		    (cons (cons (stage-name stage) (cdr v)) lst))))
	    '()
	    stages)))
 
 (define (l1-oneshot up-fn down-fn)
   (lambda (name options)
     `((type . oneshot)
       (up . ,(execline-join (up-fn name options)))
       (down . ,(execline-join (down-fn name options)))
       (dependencies . ,(ifopt 'l1-depends options '())))))

 ;; l1 service for software-defined interface types
 ;; (bridge, bond, vlan, vxlan, vether ...)
 (define (l1-soft type)
   (l1-oneshot
    (lambda (name options)
      (format "ip link add name ~a type ~a" name type))
    (lambda (name options)
      (format "ip link del ~a" name))))
 
 ;; auto-bridge creates a bridge interface
 ;;
 ;; for example:
 ;;  (bridge "br0" '("eth0" "wlan0") l3-auto)
 ;; bridges "eth0" and "wlan0" and assigns
 ;; IP addresses using dhcp
 ;;
 ;; the bridge interface will automatically
 ;; depend on the bridged interfaces
 ;;
 ;; FIXME: bridges can have interfaces added
 ;; and removed dynamically; for hotplug depencendies,
 ;; generate an intermediate service that does the
 ;; appropriate 'ip link set dev ... master ...'
 ;;
 (define (bridge name interfaces l3-config . options)
   (net->services
    `((name . ,name)
      (type . bridge)
      ,@(config-from
	 name
	 (cons options
	       ;; automatically inserts '<iface>.l2' as a depencency
	       ;; TODO: this could perhaps be <iface>.l1, but the behavior
	       ;; under those circumstances is less clear
	       (cons 'l2-depends (map (cut format "~a.l2" <>) interfaces)))
	 (l1-soft "bridge")
	 (l2-oneshot
	  (lambda (name options)
	    (append
	     (map (lambda (sub)
		    (format "ip link set dev ~a master ~a" sub name))
		  interfaces)
	     (format "ip link set dev ~a up" name)))
	  (lambda (name options)
	    (map (lambda (sub)
		   (format "ip link set dev ~a nomaster" sub))
		 interfaces)
	    (format "ip link set dev ~a down" name)))
	 l3-config))))
 
 (define (bundle name . contents)
   `((,name (type . bundle)
	    (contents . ,contents))))

 (define (flatten1 . args)
   (let loop ([lst args])
     (if (null? lst)
	 '()
	 (let ([head (car lst)]
	       [rest (loop (cdr lst))])
	   (if (list? head)
	       (append head rest)
	       (cons head rest))))))
)
