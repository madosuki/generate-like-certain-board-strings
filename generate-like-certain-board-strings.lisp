(defpackage :generate-like-certain-board-strings
  (:use :cl :ironclad :cl-ppcre :flexi-streams :cl-base64 :crypt)
  (:export
   #:generate-trip
   #:generate-id
   #:get-unix-time
   #:get-current-datetime
   #:dat-to-html
   #:dat-to-keyword-list
   #:separate-trip-from-input
   #:replace-hyphen-to-slash
   #:replace-other-line-to-lf
   #:shape-text
   #:replace-not-available-char-when-cp932
   #:convert-html-special-chars
   #:apply-dice
   #:sha256
   #:apply-color))
(in-package :generate-like-certain-board-strings)

(defmacro set-extracted-text ((extracted-list pos-list s regex-string begin-tag-start begin-tag-end end-tag &optional (forward 0) (string-prefix "")) &body body)
  (let ((extracted-text (gensym))
        (tag-with-string (gensym))
        (start (gensym))
        (end (gensym))
        (pos-cons (gensym)))
    `(let ((,extracted-list nil)
           (,pos-list nil))
       (ppcre:do-matches (,start ,end ,regex-string ,s)
         (let* ((,extracted-text (subseq ,s (+ ,forward ,start) ,end))
                (,tag-with-string (concatenate 'string
                                               ,begin-tag-start
                                               ,extracted-text
                                               ,begin-tag-end
                                               ,string-prefix
                                               ,extracted-text
                                               ,end-tag))
                (,pos-cons (cons ,start ,end)))
           (push ,tag-with-string ,extracted-list)
           (push ,pos-cons ,pos-list)))
       ,@body)))

(defun replace-http-or-https-url-to-a-tag-with-string (s)
  (let ((previous-end 0)
        (str-list nil)
        (count 0))
    (set-extracted-text (url-list url-pos s "https*://[a-zA-Z0-9%\+./-]+" "<a href=\"" "\">" "</a>")
                        (unless (car url-list)
                          (return-from replace-http-or-https-url-to-a-tag-with-string s))
                        ;; (nreverse url-list)
                           ;; (nreverse url-pos)
                        (setq url-list (nreverse url-list))
                        (setq url-pos (nreverse url-pos))
                        (dolist (x url-pos)
                          (let ((start (car x))
                                (end (cdr x)))
                            (push (subseq s previous-end start) str-list)
                            (push (nth count url-list) str-list)
                            (setq previous-end end)
                            (incf count)))
                        (when (/= previous-end (length s))
                          (push (subseq s previous-end (length s)) str-list))
                        (format nil "~{~A~}" (nreverse str-list)))))

(defun create-reply-link (s)
  (let ((result "")
        (previous-end 0)
        (count 0))
    (set-extracted-text (linked-list matched-pos-list s "&gt;&gt;\\d{1,5}" "<a href=\"#" "\">" "</a>" 8 "&gt;&gt;")
                        (unless (car linked-list)
                          (return-from create-reply-link s))
                        ;; (nreverse linked-list)
                        ;; (nreverse matched-pos-list)
                        (setq linked-list (nreverse linked-list))
                        (setq matched-pos-list (nreverse matched-pos-list))
                        (dolist (x matched-pos-list)
                          (let ((start (car x))
                                (end (cdr x)))
                            (setq result (concatenate 'string result (subseq s previous-end start) (nth count linked-list)))
                            (incf count)
                            (setq previous-end end)))
                        (when (and (/= previous-end 0) (/= previous-end (length s)))
                          (setq result (concatenate 'string result (subseq s previous-end (length s)))))
                        result)))

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
  (let ((left-num-list nil)
        (right-num-list nil)
        (s-list (coerce s 'list))
        (is-end-of-left nil))
    (dolist (x s-list)
      (if (or (equal #\D x) (equal #\d x))
          (setq is-end-of-left t)
          (if is-end-of-left
              (push x right-num-list)
              (push x left-num-list))))
    ;; (nreverse left-num-list)
    ;; (nreverse right-num-list)
    (setq left-num-list (nreverse left-num-list))
    (setq right-num-list (nreverse right-num-list))
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

(defun convert-char-in-reference-to-html-special-chars-table (c)
  (case c
    (#\& "&amp;")
    (#\" "&quot;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\BLACK_HEART_SUIT "&hearts;")
    (t (string c))))


(defun convert-html-special-chars (s)
  (let ((tmp (coerce s 'list))
        (result ""))
    (dolist (c tmp)
      (setq result (format nil "~A~A" result (convert-char-in-reference-to-html-special-chars-table c))))
    result))

(defun shape-text (text)
  (let ((tmp (cl-ppcre:split #\linefeed (apply-color (apply-dice (convert-html-special-chars text)))))
        (result ""))
    (if (> (length tmp) 1)
        (dolist (x tmp)
          (setq result (format nil "~A ~A <br>" result x)))
        (setq result (format nil " ~A " (car tmp))))
    result))

(defun non-cp932-char-table (c)
  (case c
    (#\BLACK_HEART_SUIT "&hearts;")
    (t c)))

(defun replace-not-available-char-when-cp932 (text)
  (let ((result ""))
    (dotimes (x (length text))
      (let ((current-char (aref text x)))
        (handler-case (sb-ext:string-to-octets (string current-char) :external-format :CP932)
          (error (e)
            (declare (ignore e))
            (let ((tmp (non-cp932-char-table current-char)))
              (setq result (concatenate 'string result
                                        (if (stringp tmp)
                                            tmp
                                            (format nil "&#~A;" (char-code tmp)))))))
          (:no-error (c)
            (declare (ignore c))
            (setq result (concatenate 'string result (string current-char)))))))
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
  (let* ((tmp (cl-ppcre:split "<br>" text))
         (tmp-size (length tmp))
         (count 0)
         (result ""))
    (if (> tmp-size 1)
        (dolist (x tmp)
          (let ((size (length x)))
            (incf count)
            (setq result (format nil "~A~A<br>~%" result (subseq x 1 (- size 1))))))
        (let ((x (car tmp)))
          (let* ((size (length x))
                 (check (if (< size 2)
                            nil
                            (subseq x 1 (- size 1)))))
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
  (let ((left nil)
        (right nil)
        (is-trip nil))
    (dolist (x (coerce name 'list))
      (when (and (equal x #\#) (eql is-trip nil))
        (setq is-trip t))
      (if is-trip
          (push x right)
          (push (check-2chan-name-spcecial-char x) left)))
    (if is-trip
        (list (coerce (nreverse left) 'string) (coerce (nreverse right) 'string))
        (list (coerce (nreverse left) 'string)))))

(defun separate-trip-from-dat (str)
  (let ((pos (cl-ppcre:scan "</b>" str)))
    (if (null pos)
        (list str)
        (let ((name (if (= pos 0)
                        ""
                        (subseq str 0 pos)))
              (end-b-pos (- (length str) 3)))
          (list name (subseq str (+ pos 4) end-b-pos))))))

(defun create-number-id-attribute (n)
  (concatenate 'string "id=\"" (write-to-string n) "\""))

(defmacro format-dat-to-html (no l)
  (let ((date-and-id (gensym))
        (name (gensym))
        (email (gensym))
        (text (gensym)))
    `(let ((,date-and-id (cl-ppcre:split " " (caddr ,l)))
           (,name (car ,l))
           (,email (cadr ,l))
           (,text (create-reply-link (replace-http-or-https-url-to-a-tag-with-string (car (cdddr ,l))))))
       (format nil "<dl ~A>~%<dt>~A 名前：~A[~A] 投稿日時：~A ~A</dt>~%<dd>~A</dd>~%"
               (create-number-id-attribute ,no)
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

(defun dat-to-keyword-list (dat)
  (let ((count 1)
        (result nil)
        (dat-list (dat-to-line-list dat)))
    (dolist (x dat-list)
      (let* ((tmp (cl-ppcre:split "<>" x))
             (tmp-list nil)
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
                 (setq tmp-list
                       (append (list :text
                                     (create-reply-link
                                      (replace-http-or-https-url-to-a-tag-with-string
                                       (remove-space-in-head-and-end-of-line y)))) tmp-list))))
          (incf tmp-count))
        (push tmp-list result)
        (incf count)))
    (nreverse result)))

(defun dat-to-html (dat)
  (let ((dat-list-with-keyword (dat-to-keyword-list dat))
        (result "")
        (count 0))
    (dolist (x dat-list-with-keyword)
      (let ((title (member :title x))
            (name (member :name x))
            (trip (member :trip x))
            (email (member :email x))
            (date (member :date x))
            (id (member :id x))
            (text (member :text x)))
        (incf count)
        (if title
            (setq title (cadr title))
            (setq title ""))
        (if name
            (setq name (cadr name))
            (setq name ""))
        (if trip
            (setq trip (cadr trip))
            (setq trip ""))
        (if email
            (setq email (cadr email))
            (setq email ""))
        (if date
            (setq date (cadr date))
            (setq date ""))
        (if id
            (setq id (cadr id))
            (setq id ""))
        (if text
            (setq text (cadr text))
            (setq text ""))
        (setq result
              (if title
                  (format nil
                          "<h1>~A</h1>~%<dl>~%<dt id=\"~A\">~A 名前：<font color=\"#008800\"><b>~A</b>~A</font>[~A] 投稿日：~A ~A</dt>~%<dd id=\"thread\">~A</dd>~%"
                          title count count name trip email date id text)
                  (format nil
                          "~A<dt id=\"~A\">~A 名前：<font color=\"#008800\"><b>~A</b>~A</font>[~A] 投稿日：~A ~A</dt>~%<dd id=\"thread\">~A</dd>~%"
                          result count count name trip email date id text)))))
    (concatenate 'string result "</dl>")))


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

(defun make-hash-string (&key target-string hash-type char-code)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence hash-type
                             (flexi-streams:string-to-octets target-string :external-format (intern char-code)))))

(defun create-hmac (target-string target-key hmac-type &optional (target-key-char-code "ASCII") (value-char-code "ASCII"))
  (let ((hmac (ironclad:make-hmac
               (flexi-streams:string-to-octets target-key :external-format (intern target-key-char-code))
               hmac-type))
        (bytes (flexi-streams:string-to-octets target-string :external-format (intern value-char-code))))
      (ironclad:update-hmac hmac bytes)
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))

(defun sha1 (str &optional (char-code "ASCII"))
  (make-hash-string :target-string str :hash-type :sha1 :char-code char-code))

(defun sha1-hmac (str key &optional (key-char-code "ASCII") (char-code "ASCII"))
  (create-hmac str key :sha1 key-char-code char-code)
  ;; (let ((hmac (make-hmac (flexi-streams:string-to-octets key :external-format (intern key-char-code)) :sha1))
  ;;       (bytes (flexi-streams:string-to-octets str :external-format (intern char-code))))
  ;;   (update-hmac hmac bytes)
  ;;   (byte-array-to-hex-string (hmac-digest hmac)))
  )

(defun sha256 (str &optional (char-code "ASCII"))
  (make-hash-string :target-string str :hash-type :sha256 :char-code char-code))

(defun sha256-hmac (str key &optional (key-char-code "ASCII") (char-code "ASCII"))
  (create-hmac str key :sha256 key-char-code char-code))


(defun generate-trip (key &optional (char-code "ASCII"))
  (cond ((< (length key) 12)
         (labels ((replace1 (s) (ppcre:regex-replace "[^\.-z]" s "."))
                  (replace2 (s)
                    (let ((convert-dictionary '((#\: . #\A)
                                                (#\; . #\B)
                                                (#\< . #\C)
                                                (#\= . #\D)
                                                (#\> . #\E)
                                                (#\? . #\F)
                                                (#\@ . #\G)
                                                (#\[ . #\a)
                                                (#\\ . #\b)
                                                (#\] . #\c)
                                                (#\^ . #\d)
                                                (#\_ . #\e)
                                                (#\` . #\f))))
                      (map 'string (lambda (x)
                                     (let ((tmp (find x convert-dictionary
                                                      :test (lambda (a b)
                                                              (equal a (car b))))))
                                       (if tmp
                                           (cdr tmp)
                                           x)))
                           s))))
           (let ((bytes (sb-ext:string-to-octets key :external-format :sjis)))
             (if (null bytes)
                 ""
                 (let* ((salt (let* ((bytes
                                       (sb-ext:string-to-octets
                                        (concatenate 'string key "H.")
                                        :external-format :sjis))
                                     (result (make-array 2
                                                         :element-type '(unsigned-byte 8)
                                                         :initial-element 0))
                                     (second-element (aref bytes 1))
                                     (third-element (aref bytes 2)))
                                (if (> second-element 127) ;; check whether over ascii code
                                    (setf (aref result 0) 46) ;; 46 is char code of #\.
                                    (setf (aref result 0) second-element))
                                (if (> third-element 127)
                                    (setf (aref result 1) 46)
                                    (setf (aref result 1) third-element))
                                (sb-ext:octets-to-string result :external-format :ASCII)))
                        (replaced (replace2 (replace1 salt)))
                        (trip (crypt bytes salt))
                        (trip-size (length trip)))
                   (subseq trip (- trip-size 10)))))))
        (t
         (subseq (string-to-base64-string (sha1 key char-code)) 0 12))))


(defmacro generate-target-string (ip date solt)
  `(concatenate 'string ,ip ,date ,solt))

(defun generate-id (&key ipaddr date (key-char-code "ASCII") (char-code "ASCII") solt)
  (let* ((separated-date (cl-ppcre:split " " date))
         (hmac (sha256-hmac (concatenate 'string ipaddr
                                       (if (null separated-date)
                                           date
                                           (car separated-date)))
                          solt
                          key-char-code
                          char-code)))
    (subseq (string-to-base64-string hmac) 0 8)))


(defun apply-color (target)
  (let ((result ""))
    (multiple-value-bind (start end begin-pos-array end-pos-array)
        (cl-ppcre:scan "!color:rgb&lt;(#[a-zA-Z0-9]+)&gt;:&lt;begin&gt;([\\w\\W\\s\\t\\n]+?)&lt;end&gt;"
                       target)
      (unless (or start end
                  begin-pos-array end-pos-array)
        (return-from apply-color target))
      (if (and start end
               begin-pos-array end-pos-array
               (aref begin-pos-array 0) (aref end-pos-array 0)
               (aref begin-pos-array 1) (aref end-pos-array 1))
          (setq result (format nil "~A~A<font color=\"~A\">~A</font>~A"
                               result
                               (subseq target 0 start)
                               (subseq target (aref begin-pos-array 0) (aref end-pos-array 0))
                               (subseq target (aref begin-pos-array 1) (aref end-pos-array 1))
                               (subseq target end (length target))
                               ))
          (setq result (format nil "~A~A" result target))))
    (apply-color result)))
