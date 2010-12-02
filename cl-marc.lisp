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