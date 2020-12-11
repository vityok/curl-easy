(in-package :curl-easy)

;;; A CURLcode is the universal error code.  curl/curl.h says
;;; no return code will ever be removed, and new ones will be
;;; added to the end.
(defctype curl-code :int)

;; We will use this type later in a more creative way.  For
;;; now, just consider it a marker that this isn't just any
;;; pointer.
(defctype easy-handle :pointer)

;;; Initialize libcurl with FLAGS.
(defcfun "curl_global_init" curl-code
  (flags :long))

(defcfun "curl_easy_init" :pointer)

(defparameter *easy-handle* (curl-easy-init))

(defcfun "curl_easy_cleanup" :void
  (easy-handle :pointer))

;; Invoke this function after curl_easy_init and all the
;; curl_easy_setopt calls are made, and will perform the transfer as
;; described in the options. It must be called with the same
;; easy_handle as input as the curl_easy_init call returned.
;;
;; curl_easy_perform performs the entire request in a blocking manner
;; and returns when done, or if it failed. For non-blocking behavior,
;; see curl_multi_perform.
;;
;; You can do any amount of calls to curl_easy_perform while using the
;; same easy_handle. If you intend to transfer more than one file, you
;; are even encouraged to do so. libcurl will then attempt to re-use
;; the same connection for the following transfers, thus making the
;; operations faster, less CPU intense and using less network
;; resources. Just note that you will have to use curl_easy_setopt
;; between the invokes to set options for the following
;; curl_easy_perform.
;;
;; You must never call this function simultaneously from two places
;; using the same easy_handle. Let the function return first before
;; invoking it another time. If you want parallel transfers, you must
;; use several curl easy_handles.
;;
;; While the easy_handle is added to a multi handle, it cannot be used
;; by curl_easy_perform.
;;
;; RETURN VALUE
;;
;; CURLE_OK (0) means everything was ok, non-zero means an error
;; occurred as <curl/curl.h> defines - see libcurl-errors. If the
;; CURLOPT_ERRORBUFFER was set with curl_easy_setopt there will be a
;; readable error message in the error buffer when non-zero is
;; returned.

(defcfun "curl_easy_perform" curl-code
  (handle easy-handle))

(defmacro curl-easy-setopt (easy-handle enumerated-name
                            value-type new-value)
  "Call `curl_easy_setopt' on EASY-HANDLE, using ENUMERATED-NAME
as the OPTION.  VALUE-TYPE is the CFFI foreign type of the third
argument, and NEW-VALUE is the Lisp data to be translated to the
third argument.  VALUE-TYPE is not evaluated."
  `(foreign-funcall "curl_easy_setopt" easy-handle ,easy-handle
                    curl-option ,enumerated-name
                    ,value-type ,new-value curl-code))

(defun curry-curl-option-setter (function-name option-keyword)
  "Wrap the function named by FUNCTION-NAME with a version that
curries the second argument as OPTION-KEYWORD.

This function is intended for use in DEFINE-CURL-OPTION-SETTER."
  (setf (symbol-function function-name)
        (let ((c-function (symbol-function function-name)))
          (lambda (easy-handle new-value)
            (funcall c-function easy-handle option-keyword
                     (if (stringp new-value)
                         (add-curl-handle-cstring
                          easy-handle
                          (foreign-string-alloc new-value))
                         new-value))))))



(defmacro define-curl-option-setter (name option-type
                                     option-value foreign-type)
  "Define (with DEFCFUN) a function NAME that calls
curl_easy_setopt.  OPTION-TYPE and OPTION-VALUE are the CFFI
foreign type and value to be passed as the second argument to
easy_setopt, and FOREIGN-TYPE is the CFFI foreign type to be used
for the resultant function's third argument.

This macro is intended for use in DEFINE-CURL-OPTIONS."
  `(progn
     (defcfun ("curl_easy_setopt" ,name) curl-code
       (easy-handle easy-handle)
       (option ,option-type)
       (new-value ,foreign-type))
     (curry-curl-option-setter ',name ',option-value)))

(defmacro define-curl-options (type-name type-offsets &rest enum-args)
  "As with CFFI:DEFCENUM, except each of ENUM-ARGS is as follows:

      (NAME TYPE NUMBER)

Where the arguments are as they are with the CINIT macro defined
in curl.h, except NAME is a keyword.

TYPE-OFFSETS is a plist of TYPEs to their integer offsets, as
defined by the CURLOPTTYPE_LONG et al constants in curl.h.

Also, define functions for each option named
set-`TYPE-NAME'-`OPTION-NAME', where OPTION-NAME is the NAME from
the above destructuring."

  (flet ((enumerated-value (type offset)
           (+ (getf type-offsets type) offset))
         ;; map PROCEDURE, destructuring each of ENUM-ARGS
         (map-enum-args (procedure)
           (mapcar (lambda (arg) (apply procedure arg)) enum-args))
         ;; build a name like SET-CURL-OPTION-NOSIGNAL
         (make-setter-name (option-name)
           (intern (concatenate
                    'string "SET-" (symbol-name type-name)
                    "-" (symbol-name option-name)))))
    `(progn
       (defcenum ,type-name
           ,@(map-enum-args
              (lambda (name type number)
                (list name (enumerated-value type number)))))
       ,@(map-enum-args
          (lambda (name type number)
            (declare (ignore number))
            `(define-curl-option-setter ,(make-setter-name name)
                 ,type-name ,name ,(ecase type
                                     (long :long)
                                     (objectpoint :pointer)
                                     (functionpoint :pointer)
                                     (off-t :long)))))
       ',type-name)))

(defvar *easy-handle-cstrings* (make-hash-table)
    "Hashtable of easy handles to lists of C strings that may be
  safely freed after the handle is freed.")

(defun add-curl-handle-cstring (handle cstring)
  "Add CSTRING to be freed when HANDLE is, answering CSTRING."
  (car (push cstring (gethash handle *easy-handle-cstrings*))))

(defvar *easy-handle-errorbuffers* (make-hash-table)
  "Hashtable of easy handles to C strings serving as error
writeback buffers.")

;;; An extra byte is very little to pay for peace of mind.
(defparameter *curl-error-size* 257
  "Minimum char[] size used by cURL to report errors.")

(defun make-easy-handle ()
  "Answer a new CURL easy interface handle, to which the lifetime
  of C strings may be tied.  See `add-curl-handle-cstring'."
  (let ((easy-handle (curl-easy-init)))
    (setf (gethash easy-handle *easy-handle-cstrings*) '())
    (setf (gethash easy-handle *easy-handle-errorbuffers*)
          (foreign-alloc :char :count *curl-error-size*
                         :initial-element 0))
    easy-handle))

(defun free-easy-handle (handle)
  "Free CURL easy interface HANDLE and any C strings created to
  be its options."
  (curl-easy-cleanup handle)
  (foreign-free (gethash handle *easy-handle-errorbuffers*))
  (remhash handle *easy-handle-errorbuffers*)
  (mapc #'foreign-string-free
        (gethash handle *easy-handle-cstrings*))
  (remhash handle *easy-handle-cstrings*))

(defun get-easy-handle-error (handle)
  "Answer a string containing HANDLE's current error message."
  (foreign-string-to-lisp
   (gethash handle *easy-handle-errorbuffers*)))

;;; Alias in case size_t changes.
(defctype size :unsigned-int)

;;; To be set as the CURLOPT_WRITEFUNCTION of every easy handle.
(defcallback easy-write size ((ptr :pointer) (size size)
                              (nmemb size) (stream :pointer))
	     (let ((data-size (* size nmemb)))
	       (handler-case
		   ;; We use the dynamically-bound *easy-write-procedure* to
		   ;; call a closure with useful lexical context.
		   (progn (funcall (symbol-value '*easy-write-procedure*)
				   (foreign-string-to-lisp ptr :count data-size))
			  data-size)	;indicates success
		 ;; The WRITEFUNCTION should return something other than the
		 ;; #bytes available to signal an error.
		 (error () (if (zerop data-size) 1 0)))))

(define-curl-options curl-option
    (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
  (:noprogress long 43)
  (:nosignal long 99)
  (:errorbuffer objectpoint 10)
  (:url objectpoint 2)
  (:writefunction functionpoint 11))

;; exported functions
(defun reset-handle ()
  (curl-easy-cleanup *easy-handle*)
  (setf *easy-handle* (make-easy-handle))
  (set-curl-option-nosignal *easy-handle* 1)
  (set-curl-option-writefunction *easy-handle* (callback easy-write)))

(defun easy-query (&optional (url "http://www.cliki.net/CFFI"))
  "Query just one URL."
  (reset-handle)
  (set-curl-option-url *easy-handle* url)

  (with-output-to-string (contents)
    (let ((*easy-write-procedure*
            (lambda (string)
              (write-string string contents))))
      (declare (special *easy-write-procedure*))
      (curl-easy-perform *easy-handle*))))

(defun reuse-query (&optional (url "http://www.cliki.net/CFFI"))
  "When querying multiple resources from the same server, reuse easy handle."

  (set-curl-option-url *easy-handle* url)

  (with-output-to-string (contents)
    (let ((*easy-write-procedure*
            (lambda (string)
              (write-string string contents))))
      (declare (special *easy-write-procedure*))
      (curl-easy-perform *easy-handle*))))
;; EOF
