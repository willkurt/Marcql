;;my attempt at making a very fast  marc reader in common lisp
;; the idea here is not to make a tool for editing marc records
;; marc record are the enemy
;; the idea here is to make a very fast means to query large
;; collections of mark records
;; remember use: file-position
(defparameter *test-file-loc* "~/Dropbox/code/marc/will-books-5.out")
;(defparameter *test-file* (open *test-file-loc*))

(defun test (filename)
  (with-open-file (in filename)
		  (loop while (read-char in nil) count t)))

(defun test2 (filename)
  (with-open-file (in filename)
		  (let ((scratch (make-string 4096)))
		    (loop for read = (read-sequence scratch in)
			  while (plusp read) sum read))))

(defun test-run ()
  (marcql-run *test-file-loc* '(("245" . testfun)) '(("245" . test-test))))

(defun test-test (x)
  (search "dog" x)) 

(defun testfun (x)  x)


(defun count-me (x)
  (incf *counter*))

(defun print-first-30 (x)
  (let ((end (if (< (length x) 30)
		(length x)
	      30)))
  (format T "~a... ~%" (subseq x 0 end))))

(defun contains-dog (x)
  (search "dog" x))

;;the key here is that we only need specific parts of the leader 


#|
tideal synax

(marcql my-file 
 select "008" => subfield_a
	distinct title
	author as low_auth => (lambda (x) (lowercase x))
note not like this ->	distinct (600 . a) => (lambda (x) (subject-format x))
 where pub_year > 1800
	(090 . z) contains "dog"
	title equals "Shakespeare") 
|#
(defparameter *sample-q* '(select "008" => subfield_a
				  "245"
				  "650" => test-fun
			    where
			           "245" => test-test))


(defmacro tst (a i)
  (let ((a1 (* a 3)))
  `(+ ,a1 ,i)))

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
		  
	      



;;right now I'm just worried about the obvious feature of searching for a field
;;next step is to add conditionals and actions



(defparameter *leader-length* 24)
(defparameter *leader-buff* (make-string *leader-length*))

(defparameter *directory-length* 12)
(defparameter *directory-buff* (make-string *directory-length*))

(defparameter *current-record-position* 0)

(defparameter *results* '()) ;later I'll make this a bit more sophisticated

(defparameter *counter* 0)


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
;;and we'll assume both an AND as well as that
;;all fields must be present in the record for it to pass
;;this will be amended later
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


