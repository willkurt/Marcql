;; Marcql Query example set 1
;; Introduction to marcql 'select' and 'actions'
(load "marcql.lisp")
(in-package :marcql)

;;feel free to change this to you own local file
(defparameter *marc-file* "~/Dropbox/code/marc/will-books-5.out")

(print "starting 1st example")
;; Marcql syntax is extremely simple,
;; so simple in fact that you may wonder how 
;; much you can really do with it.

;; here is a very simple marcql query.
(marcql *marc-file* 
       select
           "245")

;; when you run this nothing will happen
;; well, nothing that you can see:
(print "ending 1st example")

;; the reason nothing happend is that there was
;; no action associated with the select
;; (well technically there is a *default-action*
;;  which if you're interested in you can 
;;  change if you want)

;; okay let's try something else...
(defparameter *title-count* 0)
;; we'll count titles!

;; all we need to do is create an 'action'
;; which is just a function 
;; that takes one argument 
(defun count-title (x)
  (incf *title-count*))
;; notice that we're not using the arugment?
;; no matter what happens actions are always 
;; passed the actual content (unprocessed)
;; of the marc field you're selecting.
;; so any action needs to accept that one
;; argument.

;; If you don't know Common Lisp this
;; should be no great hindrence as
;; many helpful action are fairly
;; easy to write.

(print "starting 2nd example")
;; so let's try that last query again with
;; an action this time.
(marcql *marc-file*
	select
	   "245" => count-title)
(print "done with 2nd query")
;; after this runs you still won't see anything
;; we have to actually access the counter
(print *title-count*)
;; which will show us the count
;; note that if we don't reset this counter
;; and run the query again it will double
(print "ending 2nd example")

(print "time demo")
;; one nice feature of Common Lisp
;; is that you can very easily time 
;; a function. Just wrap you query 
;; like this:
(time (marcql *marc-file*
	select
	   "245" => count-title))
;; on a macbook 2 GHz core2duo
;; marcql runs about 15,000 records/sec



