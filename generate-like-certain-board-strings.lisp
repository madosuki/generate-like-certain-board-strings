(defpackage :generate-like-certain-board-strings
  (:use :cl :ironclad :cl-ppcre :flexi-streams :cl-base64)
  (:export
   #:generate-trip
   #:generate-id
   #:get-unix-time
   #:get-current-datetime
   #:dat-to-html
   #:dat-to-keyword-list
   #:separate-trip-from-input
   #:replace-newline-to-br-tag
   #:replace-hyphen-to-slash
   #:replace-other-line-to-lf
   #:shape-text
   #:replace-not-available-char-when-cp932
   #:escape-sql-query
   #:apply-dice))

(in-package :generate-like-certain-board-strings)

(defun dice (dice-num max-num)
  (labels ((dice-function (d-n m-num &optional (result nil) (count 0))
             (if (= count d-n)
                 result
                 (let ((dice-result (1+ (ironclad:strong-random m-num))))
                   (if (listp result)
                       (dice-function d-n m-num (append result (list dice-result)) (1+ count))
                       (dice-function d-n m-num (list dice-result) (1+ count)))))))
    (dice-function dice-num max-num)))

(defun generate-dice-string (d-n m-num dice-result-list)
  (let ((dice-result-string ""))
    (if (> d-n 1)
        (progn
          (dolist (x dice-result-list)
            (setq dice-result-string
                  (concatenate 'string dice-result-string
                               (if (string= dice-result-string "") "" "+")
                               (write-to-string x))))
          (setq dice-result-string
                (concatenate 'string
                             (write-to-string (reduce #'+ dice-result-list))
                             "(" dice-result-string ")")))
        (setq dice-result-string (write-to-string (car dice-result-list))))
    (concatenate 'string "[" (write-to-string d-n) "D" (write-to-string m-num) ":" dice-result-string "]")))

(defun search-num-on-dice-strings (s)
  (let ((left-num-list (list nil))
        (right-num-list (list nil))
        (s-list (coerce s 'list))
        (is-end-of-left nil))
    (dolist (x s-list)
      (if (or (equal #\D x) (equal #\d x))
          (setq is-end-of-left t)
          (if is-end-of-left
              (push x right-num-list)
              (push x left-num-list))))
    (setq left-num-list (cdr (reverse left-num-list)))
    (setq right-num-list (cdr (reverse right-num-list)))
    (values
     (parse-integer (coerce left-num-list 'string) :junk-allowed t)
     (parse-integer (coerce right-num-list 'string) :junk-allowed t))))

(defun apply-dice (text &optional (is-in-name nil))
  (multiple-value-bind (start-position end-position group)
      (ppcre:scan "!(\\d{1,2})[dD](\\d{1,3})" text)
    (unless start-position
      (return-from apply-dice text))
    (let* ((tmp-string (subseq text (aref group 0) end-position)))
      (multiple-value-bind (left right)
          (search-num-on-dice-strings tmp-string)
        (unless (and (null left) (null right))
          (if (and (< left 11) (< right 101))
              (let ((tmp (generate-dice-string left right (dice left right)))
                    (begin-tag (if is-in-name "" "<b>"))
                    (end-tag (if is-in-name "" "</b>")))
                (apply-dice
                 (concatenate 'string (subseq text 0 start-position) begin-tag tmp end-tag (subseq text end-position))
                 is-in-name))
              (apply-dice
               (concatenate 'string (subseq text 0 start-position)
                            "[is over]"
                            (subseq text end-position))
               is-in-name)))))))

(defmacro char-to-reference-string (c)
  `(format nil "&#~A;" (char-code ,c)))

(defun escape-sql-query (text)
  (flet ((convert-table (c)
           (if (or (equal #\" c) (equal #\' c) (equal #\( c) (equal #\) c) (equal #\= c) (equal #\: c) (equal #\; c) (equal #\\ c))
               (char-to-reference-string c)
               (string c))))
    (let ((result ""))
      (dolist (x (coerce text 'list))
        (setq result (concatenate 'string result (convert-table x))))
      result)))

(defun convert-char-in-reference-to-html-special-chars-table (c)
  (case c
    (#\& "&amp;")
    (#\" "&quot;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\BLACK_HEART_SUIT "&hearts;")
    (t (string c))))

(defun replace-not-available-char-when-cp932 (text)
  (flet ((replace-char-to-character-references (c)
           (let ((tmp (convert-char-in-reference-to-html-special-chars-table c)))
             (if (equal c tmp)
                 (format nil "&#~A;" (char-code c))
                 tmp))))
    (let ((result ""))
      (dolist (x (coerce text 'list))
        (handler-case (sb-ext:string-to-octets (string x) :external-format :sjis)
          (error (e)
            (declare (ignore e))
            (setq result (concatenate 'string result (replace-char-to-character-references x))))
          (:no-error (c)
            (declare (ignore c))
            (setq result (concatenate 'string result (replace-char-to-character-references x))))))
      result)))

(defun shape-text (text)
  (let ((tmp (cl-ppcre:split #\linefeed text))
        (result ""))
    (if (> (length tmp) 1)
        (dolist (x tmp)
          (setq result (format nil "~A ~A <br>" result x)))
        (setq result (format nil " ~A <br>" (car tmp))))
    (print result)
    result))

(defun replace-other-line-to-lf (text)
  (let ((is-cr (cl-ppcre:scan #\return text))
        (is-lf (cl-ppcre:scan #\linefeed text)))
    (cond ((and (not (null is-cr)) (not (null is-lf)))
           (cl-ppcre:regex-replace-all #\linefeed
                                       (cl-ppcre:regex-replace-all #\return text "")
                                       (string #\linefeed)))
          ((and (not (null is-cr)) (null is-lf))
           (cl-ppcre:regex-replace-all #\return text (string #\linefeed)))
          (t
           text))))

(defun replace-br-tag-to-newline (text)
  (cl-ppcre:regex-replace-all "<br>" text (concatenate 'string "<br>" (string #\linefeed))))

(defun replace-hyphen-to-slash (text)
  (cl-ppcre:regex-replace-all "-" text "/"))

(defun remove-space-in-head-and-end-of-line (text)
  (let ((tmp (cl-ppcre:split "<br>" text))
        (result ""))
    (if (> (length tmp) 1)
        (dolist (x tmp)
          (let* ((size (length x))
                 (separated (subseq x 1 (- size 1))))
            (setq result (format nil "~A~A<br>~%" result separated))))
        (let ((x (car tmp)))
          (let ((check (subseq x 1 (- (length x) 1))))
            (if (null check)
                (return-from remove-space-in-head-and-end-of-line "")
                (return-from remove-space-in-head-and-end-of-line check)))))
    result))

(defmacro check-2chan-name-spcecial-char (target)
  `(cond ((equal ,target #\BLACK_DIAMOND)
          #\WHITE_DIAMOND)
         ((equal ,target #\BLACK_CIRCLE)
          #\WHITE_CIRCLE)
         ((equal ,target #\BLACK_STAR)
          #\WHITE_STAR)
         (t
          ,target)))

(defun separate-trip-from-input (name)
  (let ((left (list nil))
        (right (list nil))
        (is-trip nil))
    (dolist (x (coerce name 'list))
      (when (and (equal x #\#) (eql is-trip nil))
        (setq is-trip t))
      (if is-trip
          (push x right)
          (push (check-2chan-name-spcecial-char x) left)))
    (if is-trip
        (list (coerce (cdr (reverse left)) 'string) (coerce (cdr (reverse right)) 'string))
        (list (coerce (cdr (reverse left)) 'string)))))

(defun separate-trip-from-dat (str)
  (let ((pos (cl-ppcre:scan "</b>" str)))
    (if (null pos)
        (list str)
        (let ((name (if (= pos 0)
                        ""
                        (subseq str 0 pos)))
              (end-b-pos (- (length str) 3)))
          (list name (subseq str (+ pos 4) end-b-pos))))))

(defmacro format-dat-to-html (no l)
  (let ((date-and-id (gensym))
        (name (gensym))
        (email (gensym))
        (text (gensym)))
    `(let ((,date-and-id (cl-ppcre:split " " (caddr ,l)))
           (,name (car ,l))
           (,email (cadr ,l))
           (,text (car (cdddr ,l))))
       (format nil "<dl>~%<dt>~A 名前：~A[~A] 投稿日時：~A ~A</dt>~%<dd>~A</dd>~%"
               (write-to-string ,no)
               ,name
               ,email
               (concatenate 'string (car ,date-and-id) " " (cadr ,date-and-id))
               (caddr ,date-and-id)
               ,text))))

(defun dat-to-line-list (dat)
  (with-open-file (input dat
                         :direction :input
                         :if-does-not-exist nil
                         :external-format :sjis)
    (when input
      (loop for line = (read-line input nil)
            while line
            collect line))))

(defun dat-to-html (dat)
  (let ((count 1)
        (result ""))
    (dolist (x (dat-to-line-list dat))
      (let ((tmp (cl-ppcre:split "<>" x)))
        (when (= count 1)
          (setq result (format nil "<h1>~A</h1>~%" (cadr (cdddr tmp)))))
        (setq result (concatenate 'string result (format-dat-to-html count tmp))))
      (incf count))
    result))

(defun dat-to-keyword-list (dat)
  (let ((count 1)
        (result (list nil))
        (dat-list (dat-to-line-list dat)))
    (dolist (x dat-list)
      (let* ((tmp (cl-ppcre:split "<>" x))
             (tmp-list (list nil))
             (tmp-count 0))
        (dolist (y tmp)
          (cond ((and (= count 1) (= tmp-count 4))
                 (setq tmp-list (append (list :title y) tmp-list)))
                ((= tmp-count 0)
                 (let ((name-and-trip (separate-trip-from-dat y)))
                   (if (> (length name-and-trip) 1)
                       (setq tmp-list (append (list :trip (cadr name-and-trip)) tmp-list))
                       (setq tmp-list (append (list :trip "") tmp-list)))
                   (setq tmp-list (append (list :name (car name-and-trip)) tmp-list))))
                ((= tmp-count 1)
                 (setq tmp-list (append (list :email y) tmp-list)))
                ((= tmp-count 2)
                 (let ((date-and-id (cl-ppcre:split " " y)))
                   (setq tmp-list (append (list :date
                                                (concatenate 'string (car date-and-id)
                                                             " "
                                                             (cadr date-and-id))) tmp-list))
                   (setq tmp-list (append (list :id (caddr date-and-id)) tmp-list))))
                ((= tmp-count 3)
                 (setq tmp-list (append (list :text (remove-space-in-head-and-end-of-line y)) tmp-list))))
          (incf tmp-count))
        (push tmp-list result)
        (incf count)))
    (cdr (reverse result))))

(defun get-day-of-the-week (day)
  (cond ((= day 0)
         "Mon")
        ((= day 1)
         "Tue")
        ((= day 2)
         "Wed")
        ((= day 3)
         "Thu")
        ((= day 4)
         "Fri")
        ((= day 5)
         "Sat")
        ((= day 6)
         "Sun")))

;; get unixtime
(defun get-unix-time (universal-time)
  (- universal-time (encode-universal-time 0 0 0 1 1 1970 0)))

;; get formated current datetime from universal-time.
(defun get-current-datetime (universal-time &optional (is-diff nil) (diff 0))
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (if is-diff (decode-universal-time universal-time diff) (decode-universal-time universal-time))
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month date hour minute second)))

(defun sha1 (str &optional (char-code "ASCII"))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1
                             (flexi-streams:string-to-octets str :external-format (intern char-code)))))

(defun sha1-hmac (str key &optional (key-char-code "ASCII") (char-code "ASCII"))
  (let ((hmac (make-hmac (flexi-streams:string-to-octets key :external-format (intern key-char-code)) :sha1))
        (bytes (flexi-streams:string-to-octets str :external-format (intern char-code))))
    (update-hmac hmac bytes)
    (byte-array-to-hex-string (hmac-digest hmac))))


(defun generate-trip (key &optional (char-code "ASCII"))
  (subseq (string-to-base64-string (sha1 (escape-sql-query key) char-code)) 0 12))

;; this solt is sample. don't use production.
(defvar *solt* "wqk0SZoDaZioQbuYzCM3mRBDFbj8FD9sx3ZX34wwhnMjtdAIM2tqonirJ7o8NuDpPkFIFbAacZYTsBRHzjmagGpZZb6aAZVvk5AcWJXWGRdTZlpo7vuXF3zvg1xp9yp0")

(defmacro generate-target-string (ip date solt)
  `(concatenate 'string ,ip ,date ,solt))

(defun generate-id (&key ipaddr date (key-char-code "ASCII") (char-code "ASCII"))
  (let* ((separated-date (cl-ppcre:split " " date))
         (hmac (sha1-hmac (concatenate 'string ipaddr
                                       (if (null separated-date)
                                           date
                                           (car separated-date)))
                          *solt*
                          key-char-code
                          char-code)))
    (subseq (string-to-base64-string hmac) 0 8)))
