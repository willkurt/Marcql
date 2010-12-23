;;Copyright (c) 2010 William Kurt.
;;All rights reserved.

;;Redistribution and use in source and binary forms are permitted
;;provided that the above copyright notice and this paragraph are
;;duplicated in all such forms and that any documentation,
;;advertising materials, and other materials related to such
;;distribution and use acknowledge that the software was developed
;;by William Kurt.  The name of the
;;person may not be used to endorse or promote products derived
;;from this software without specific prior written permission.
;;THIS SOFTWARE IS PROVIDED ''AS IS'' AND WITHOUT ANY EXPRESS OR
;;IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
;;WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE

(defpackage :marcql
  (:use :common-lisp))
(in-package :marcql)


;; MARC is an abysmal file format for bibliographic data used in libraries.
;; The purpose of MARCQL is to get data out of MARC records in as pleasant
;; a way as possible, and as fast as possible.
;;
;; If you're goal is to process MARC records this is not the tool for you.
;; However, if you want to play with bibliographic data this aims to
;; be the least painful way prossible.
;;
;; This tool is essentially 2 parts: 
;;
;; 1. 'marcql-run' a very minimal, but also
;; very fast MARC processing function. If your goal is querying records 
;; this library is an order of magnitude faster than many of the other
;; tools available.  
;;
;; 2. 'marcql' is a small DSL designed to make quering much easier
;; while not abandoning expressiveness and extensibility.
;; the basic syntax looks like this
;; 
;; (marcql <file-name>
;;        select
;;           <marc_field_number> | <marc_field_number> => <action>
;;           ...
;;        where
;;           <marc_field_number> =>  <action>)
(defparameter *subfield-delimiter* (string #\Us))
(defparameter *field-delimiter* (string #\Rs)) ;;haven't confirmed

(defun not-much (x)
  x)

(defparameter *default-action* #'not-much)

(defmacro marcql (file-name &rest rest)
  (let ((select-list (parse-select rest))
	(where-list (parse-where (where-part rest))))
    `(marcql-run ,file-name (quote ,select-list)  (quote ,where-list))))

(defun where-part (ls)
  (let ((first (car ls)))
    (cond ((null first) '())
	  ((eql 'where first) ls)
	  (t (where-part (cdr ls))))))

(defun parse-select (ls)
  (parse-term 'select 'where ls))

(defun parse-where (ls)
  (parse-term 'where 'end ls))

(defun parse-term (start-term end-term ls)
  "parse the select portion of a marcql query returning the where clause or '()"
  (let ((first (car ls))  
	(next (cadr ls))
	(third (caddr ls)))
    (cond ((or (null first) (eql first end-term))
	       '())
	   ((eql first start-term) 
	    (parse-select (cdr ls)))
	   ((eql next '=>) ;in this case there is an action
	    (cons (cons first third)
		  (parse-select (cdddr ls))))
	   (T ;this case we just have a selector
	    (cons (cons first *default-action*)
 		  (parse-select (cdr ls)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The trick to getting a lot of speed out of this library
;; was to only process the absolute minimum necessary
;; to run a query.  The leader is processed to get the
;; offset and the directory of fields is quickly parsed
;; but other than that only necessary fields are processed.
;; 'actions' in marcql take care of any additional 
;; processing. Want to convert the char-set? Write
;; an action for it. Want to print to screen? You
;; guessed it, write an action.
;;
;; The major benefit of this philosophy is that you
;; never pay for more than you need.
;;

(defparameter *leader-length* 24)
(defparameter *leader-buff* (make-string *leader-length*))

(defparameter *directory-length* 12)
(defparameter *directory-buff* (make-string *directory-length*))

(defparameter *current-record-position* 0)

(defun marcql-run (file-name selects cnds)
  (progn
    (with-open-file (fs file-name)
		  (loop for next-char = (read-char (progn
						     (file-position fs (1+ *current-record-position*))
						     fs) nil)
			while next-char do (process-next-record 
			    fs
			    selects
			    :conditions cnds))))
  (setf *current-record-position* 0))


;; process-next-record and leaves file pointer in place for the next call
;; select fields are of the form ( FIELDNUMBER . ACTION)
;; action must be a named function
;;
;; for now the conditions will world just like actions
;; i.e. (fieldnumber . condition)
;; all fields must be present in the record for it to pass
(defun process-next-record (file select-fields &key (conditions '()))
  (let* ((base (file-position file *current-record-position*))
	(offset (process-leader *leader-buff* file))
	(s-fields-only (mapcar #'car select-fields))
	(fields (append (make-lead-dir-list offset)
			(process-directory *directory-buff* file (- (+ base offset) 1))))
	;the sort here is used to make sure that the fields are in the same
	;order the user wrote them in the original select statement
	;this is a pretty useful guarantee for doing complex select actions
	(select-to-fetch (sort (remove-if-not (lambda (x) 
					  (member (car x) s-fields-only
						  :test #'equalp)) fields)
			       (lambda (a b) (< (position (car a) s-fields-only :test #'equalp)
						(position (car b) s-fields-only :test #'equalp))))))
			       				
    (progn
      (when (check-conditions conditions fields file (+ base offset))
	(mapcar (lambda (field)
		  (apply-field-func 
		   (cadr field) 
		   (+ base offset (caddr field)) 
		   file
		   (cdr (assoc (car field) select-fields :test #'equalp))))
		select-to-fetch))
      (setf *current-record-position* (end-of-record base offset fields)))))

(defun make-lead-dir-list (offset)
    (list (list "leader" *leader-length* (- offset))
	 (list  "directory" (- offset *leader-length*) (+ *leader-length* (- offset)))))

(defun check-conditions (conditions fields file total-offset)
  (let* ((c-fields-only (mapcar #'car conditions))
	 (fields-to-fetch (remove-if-not (lambda (x)
					   (member (car x) c-fields-only
						   :test #'equalp)) fields)))
    (cond
     ((null conditions) T)
     ((not (= (length c-fields-only)
	       (length (remove-duplicates
			fields-to-fetch)))) '())
     (T (every #'(lambda (x) (not (null x)))
	       (mapcar (lambda (c)
 			 (let* ((field-id (car c))
				(test (cdr c))
				(all-matching (remove-if-not 
					       (lambda (x) (equalp field-id (car x)))
					       fields-to-fetch)))
			   (some #'(lambda (x) (not (null x)))
				 (mapcar (lambda (fld)
					 (let* ((len-loc-pair (cdr fld))
					       (field-content (get-field (car len-loc-pair)
									 (+ total-offset (cadr len-loc-pair))
									 file)))
					   (funcall test field-content))) all-matching))))
					 

		       conditions))))))

(defun get-field (len loc file)
  (let ((buff (make-string len)))
    (progn
      (file-position file (+ 0 loc))
      (read-sequence buff file)
      buff)))

(defun apply-field-func (len loc file func)
  (let ((buff (get-field len loc file))
	(func (if (not func)
		  #'(lambda (x) x)
		func)))
      (funcall func buff)))

(defun sort-in-order-of (ls-a ls-b)
  (sort ls-b (lambda (x y) (< (position x ls-a :test #'equalp)
			      (position y ls-a :test #'equalp)))))
;right now this only return the base address of the data
(defun process-leader (buff file)
  (progn
    (read-sequence buff file)
    (parse-integer (subseq buff 12 17))))

(defun process-directory-entry (buff file)
  (progn
    (read-sequence buff file)
    (let ((tag (subseq buff 0 3))
	  (flen (parse-integer (subseq buff 3 7)))
	  (start (parse-integer (subseq buff 7 12))))
      (list tag flen start))))

(defun process-directory (buff file end)
  (let ((real-end (- end 1)))
    (loop while (< (file-position file) real-end)
	  collecting (process-directory-entry buff file))))

(defun end-of-record (last-record-start off-set directory-list)
  (let* ((last-field (car (last directory-list)))
	 (len-last-field (cadr last-field))
	 (pos-last-field (caddr last-field)))
    (+ last-record-start off-set len-last-field pos-last-field 1)))

