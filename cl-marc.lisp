;;my attempt at making a very fast  marc reader in common lisp
;; the idea here is not to make a tool for editing marc records
;; marc record are the enemy
;; the idea here is to make a very fast means to query large
;; collections of mark records
;; remember use: file-position
(defparameter *test-file-loc* "~/Dropbox/code/marc/will-books-5.out")
(defparameter *test-file* (open *test-file-loc*))

(defun test (filename)
  (with-open-file (in filename)
		  (loop while (read-char in nil) count t)))

(defun test2 (filename)
  (with-open-file (in filename)
		  (let ((scratch (make-string 4096)))
		    (loop for read = (read-sequence scratch in)
			  while (plusp read) sum read))))















;;the key here is that we only need specific parts of the leader 


#|
ideal synax

(marcql my-file screen
 select "008" => subfield_a
	distinct title
	author as low_auth => (lambda (x) (lowercase x))
note not like this ->	distinct (600 . a) => (lambda (x) (subject-format x))
 where pub_year > 1800
	(090 . z) contains "dog"
	title equals "Shakespeare") 
	
	    
	 



|#

;;right now I'm just worried about the obvious feature of searching for a field
;;next step is to add conditionals and actions



(defparameter *leader-length* 24)
(defparameter *leader-buff* (make-string *leader-length*))

(defparameter *directory-length* 12)
(defparameter *directory-buff* (make-string *directory-length*))

(defparameter *current-record-position* 0)

(defparameter *results* '()) ;later I'll make this a bit more sophisticated


(defun test-run ()
  (loop for i from 0 to 100000
	do (process-next-record *test-file* '("245" "008" "600"))))



;;for testing
(defun reset-sys ()
  (progn
    (setf *current-record-position* 0)
    (file-position *test-file* 0)
    (setf *results* '())))


;;process record and leaves file pointer in place for the next cal
(defun process-next-record (file select-fields)
  (let* ((base (file-position file *current-record-position*))
	(offset (process-leader *leader-buff* file))
	(fields (process-directory *directory-buff* file (- (+ base offset) 1)))
	(fields-to-fetch (remove-if-not (lambda (x) 
					  (member (car x) select-fields
						  :test #'equalp)) fields)))
    (progn
      (mapcar (lambda (field)
		(store-field-results 
		 (cadr field) 
		 (+ base offset (caddr field)) 
		 file))
	      fields-to-fetch)
      (setf *current-record-position* (end-of-record base offset fields)))))


;;this is very simple just to check that the mechanics of it all work
(defun store-field-results (len loc file)
  (let ((buff (make-string len)))
    (progn
     (file-position file loc)
      (read-sequence buff file)
      (push buff *results*))))




;right now this only return the base address of the data
(defun process-leader (buff file)
  (progn
    (read-sequence buff file)
    (parse-integer (subseq buff 12 17))))

;;
;this is needlessly slow, eat a big chunk
;all at once and then process that
(defun process-directory-entry (buff file)
  (progn
    (read-sequence buff file)
    (let ((tag (subseq buff 0 3))
	  (flen (parse-integer (subseq buff 3 7)))
	  (start (parse-integer (subseq buff 7 12))))
      (list tag flen start))))

(defun process-directory-entry2 (start seq)
  (let* ((beginning (* start *directory-length*))
	 (end (+ *directory-length* beginning))
	 (my-seq (subseq seq beginning end))
	 (tag (subseq my-seq 0 3))
	 (flen (parse-integer (subseq my-seq 3 7)))
	 (start (parse-integer (subseq my-seq 7 12))))
    (list tag flen start)))
      
(defun process-directory (buff file end)
  (let ((real-end (- end 1)))
    (princ real-end)
    (loop while (< (file-position file) real-end)
	  collecting (process-directory-entry buff file))))

(defun process-directory2 (file end)
  (let* ((buff-size (- (1- end) *leader-length*))
	(dir-buff (make-string buff-size)))
    (progn
      (read-sequence dir-buff file)
      (loop for i from 0 below (/ buff-size 12)
	    collecting (process-directory-entry2 i dir-buff)))))




(defun end-of-record (last-record-start off-set directory-list)
  (let* ((last-field (car (last directory-list)))
	 (len-last-field (cadr last-field))
	 (pos-last-field (caddr last-field)))
    (+ last-record-start off-set len-last-field pos-last-field 1)))


;;this is temp just to get a feel for working with the file info
(defun calc-nrs (lead-buff directory-buff file)
  (let* ((last-start (file-position file))
	(off-set (process-leader lead-buff file))
	(directory-list (process-directory directory-buff file off-set)))
    (+ 1 (end-of-record last-start off-set directory-list))))

