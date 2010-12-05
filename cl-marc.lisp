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
;; However, if you want to play with bibliographic data, and this aims to
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
;; (marql <file-name>
;;        select
;;           <marc_field_number> | <marc_field_number> => <action>
;;           ...
;;        where
;;           <marc_field_number> =>  <action>)

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
	    (cons (cons first '())
		  (parse-select (cdr ls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The trick to getting a lot of speed out of this library
;; was to only process the absolute minimum necessary
;; to run a query.  The leader is processed to get the
;; offset and the directory of fields is quickly parse
;; but other than that only necessary fields are processed
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
  (with-open-file (fs file-name)
		  (loop for i from 0 to 100000
			do (process-next-record 
			    fs
			    selects
			    :conditions cnds))))

;;process record and leaves file pointer in place for the next cal
;; select fields are of the form ( FIELDNUMBER . ACTION)
;; action must be a named function
;;
;;for now the conditions will world just like actions
;;i.e. (fieldnumber . condition)
;;all fields must be present in the record for it to pass
(defun process-next-record (file select-fields &key (conditions '()))
  (let* ((base (file-position file *current-record-position*))
	(offset (process-leader *leader-buff* file))
	(s-fields-only (mapcar #'car select-fields))
	(fields (process-directory *directory-buff* file (- (+ base offset) 1)))
	(select-to-fetch (remove-if-not (lambda (x) 
					  (member (car x) s-fields-only
						  :test #'equalp)) fields)))
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
					;note there are cases where there can be more than
					;one of a field, later we'll have to support that
				(len-loc-pair (cdr (assoc field-id fields :test #'equalp)))
				(field-content (get-field (car len-loc-pair)
						   (+ total-offset (cadr len-loc-pair))
						   file)))
			   (funcall test field-content)))
		       conditions))))))

(defun get-field (len loc file)
  (let ((buff (make-string len)))
    (progn
      (file-position file (+ 2 loc))
      (read-sequence buff file)
      buff)))

(defun apply-field-func (len loc file func)
  (let ((buff (get-field len loc file))
	(func (if (not func)
		  #'(lambda (x) x)
		func)))
      (funcall func buff)))

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
