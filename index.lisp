;;(list "foo" "bar")
;;(list :foo "foo" :bar "bar")

;; Basic Hello world function
(defun hello-world ()
  (format t "Hello world !"))

;; Access symbol from plist
(defun access-foo-plist ()
  (getf (list :foo "bar") :foo))

;; Global variable declaration 'db'
(defvar *db* nil)

(defun dump-db ()
  (dolist (item *db*)
    (format t "~{~a:~10t~a~%~}~%" item)))

;; Add cd function
(defun add-record (cd)
  (push cd *db*))

;; Create a CD record
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; *query-io* is global magic var that gives the standard input stream
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  ;; ensure that lisp doesn't wait for an input before printing prompt
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "TITLE: ")
   (prompt-read "ARTIST: ")
   (or (parse-integer (prompt-read "RATING: ") :junk-allowed t) 0)
   (y-or-n-p "RIPPED: ")))

;; Loop while prompting user to add some cd in the database
(defun add-loop ()
  (loop (add-record(prompt-for-cd))
     (if (not (y-or-n-p "Another one ?"))(return)))
  (dump-db))

;; Save database to file
(defun save-db (filename)
  (with-open-file (out filename
		  :direction :output
		  :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; Open database from file
;;(defun open-db (path))

