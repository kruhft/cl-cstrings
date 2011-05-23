;;;
;;; #"-reader - read in extended C style strings using #"..."# read-macro
;;;
;;; This code allows you to enter extended C style strings with escape
;;; sequences, such as:
;;;
;;;	#"Hello,\tWorld\n"#
;;;
;;; Supported are \a, \b, \f, \r, \n, \v, \nnn (up to 3 digits) and
;;; \xn... (unlimited length until non-hex char) escapes.  
;;; 
;;; Adds \d (decimal), \B (binary) and \o (octal) formatters of
;;; unspecified length (use non-base char or "" to terminate sequence
;;; if needed, ie.  #"\B11100abc" or #"\xabc""def"#).
;;;
;;; Allowed are C style string concatenation using ".*".  In between the ""
;;; characters you can have:
;;;	- any non-visible char (whitespace, newlines, etc)
;;;	- line comments starting with a ;
;;;	- block comments enclosed in #| |#
;;;
;;; The read macro number paramter is used to set the initial buffer size
;;; and expansion parameter (default 16 characters).  For example:
;;;
;;;	* #1024"output: " (a-function-that-returns-a-really-long-string) #"
;;;
;;; This will initially allocate 1024 chars for the internal string
;;; buffer, and then when this is filled, will allocate another 1024
;;; chars, etc, etc.  This saves on reallocations for long strings but
;;; otherwise does not affect operation.
;;;
;;; Bugs: 
;;;	- doesn't like you to use macro characters in between ".*"  sequences
;;;	- tested on SBCL 1.0.47, doesn't work with clisp yet
;;;
;;; Author: Burton Samograd <kruhft@gmail.com>
;;; Date: May 23, 2011
;;; License: 3 Clause BSD (See README)

(defun |#"-reader| (stream subchar arg)
  (declare (ignore subchar))
  (let* ((sb-size (if arg arg 16))
	 (sb (make-array sb-size
			:element-type 'standard-char
			:adjustable t
			:fill-pointer 0))
	(chars nil))
    (catch 'end-of-string
      (do ((c (read-char stream) (read-char stream)))
	  (nil) ; loop until return
	(if (char= c #\")
	    ;; we saw a ", so deal with reading whitespace, comments and expressions
	    (do ((c2 (peek-char nil stream) (peek-char nil stream)))
		(nil) ; loop until return
	      (case c2
		((#\  #\Tab #\Newline #\Return #\Bel #\Backspace #\Page #\Vt) ; invisible char
		 (read-char))
		(#\; (read-char stream) ; read ; line comment
		     (do ((c3 (read-char stream) (read-char stream)))
			 ((char= c3 #\Newline))))
		(#\# (read-char stream)
		     (if (char= (peek-char nil stream) #\|)
			 ; read #| |# comment 
			 (progn
			   (read-char stream)
			   (do ((c3 (read-char stream) (read-char stream)))
			       (nil)
			     (if (char= c3 #\|)
				 (let ((c4 (peek-char nil stream)))
				   (when (char= c4 #\#)
				       (read-char stream)
				       (return))))))
			 (throw 'end-of-string nil))) ; found end maker "#
		(#\" (read-char stream) ; found next "
		     (let ((c2 (peek-char nil stream)))
		       (when (char= c2 #\#) ; check for terminating #
			 (read-char stream)
			 (throw 'end-of-string nil)))
		     (setq c (read-char stream))
		     (return))
		(otherwise
		 (error "#\#-reader: non-whitespace or comment in middle of \"\" span")))))
	(if (char= c #\\) ; if we see a \ escaped char
	    (progn
	      (let ((c2 (read-char stream)))
		(labels ((read-upto-n-char-radix-stream (stream radix &optional (max-digits -1))
			   ;; read in up to max-digit string of radix, or unlimited digits
			   ;; up to non-base character if max-digits is not specified
			   (let ((str (let (l) 
					(do ((c (read-char stream) (read-char stream)))
					    ((handler-case (parse-integer
							    (coerce `(,c) 'string)
							    :radix radix)
					       (parse-error ()
						 (unread-char c)
						 t)
					       (:no-error (a b)
						 (declare (ignore a b))
						 nil)))
					  (push c l)
					  (if (= (length l) max-digits)
					      (return)))
					(coerce (nreverse l) 'string))))
			     ; parse resulting string and return value
			     (handler-case (parse-integer str :radix radix)
			       (parse-error ()
				 (error (format nil "#\"-reader: no digit characters after escape sequence for base ~A" radix)))))))
		  (setq c2 (case c2 ; convert escaped char or escape sequence
			     (#\a #\Bel)
			     (#\b #\Backspace)
			     (#\f #\Page)
			     (#\n #\Newline)
			     (#\r #\Return)
			     (#\t #\Tab)
			     (#\v #\Vt)
			     (#\B (code-char (read-upto-n-char-radix-stream stream 2)))
			     (#\o (code-char (read-upto-n-char-radix-stream stream 8)))
			     (#\d (code-char (read-upto-n-char-radix-stream stream 10)))
			     (#\x (code-char (read-upto-n-char-radix-stream stream 16)))
			     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) ; octal
			      (unread-char c2)
			      (code-char (read-upto-n-char-radix-stream stream 8 3)))
			     (otherwise c2)))
		  (push c2 chars) ; save char
		  (vector-push-extend #\~ sb sb-size) ; add ~C to format specifier string
		  (vector-push-extend #\C sb sb-size))))
	    (vector-push-extend c sb sb-size)))) ; normal char, just add to string
      (if chars
	  (apply #'format nil sb (nreverse chars))
	  sb)))

(set-dispatch-macro-character #\# #\" #'|#"-reader|)
