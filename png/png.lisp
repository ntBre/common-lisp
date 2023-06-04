(defun write-bytes (stream &rest bytes)
  (dolist (b bytes)
    (write-byte b stream)))

(defun write-png-signature (stream)
  "write the PNG signature to STREAM"
  (write-bytes stream 137 80 78 71 13 10 26 10))

(defun write-ihdr (stream)
  "first four bytes are the chunk length, followed by the header itself"
  (write-bytes stream 0 0 0 13 73 72 68 82))

(defun write-idat (stream)
  ;; zero length for now
  (destructuring-bind (a b c d)
      (to-four-bytes (* 255 255))
    (write-bytes stream a b c d 73 68 65 84)))

(defun write-iend (stream)
  (write-bytes stream 0 0 0 0 73 69 78 68))

(defun write-width (stream)
  (apply #'write-bytes stream (to-four-bytes 400)))

(defun write-height (stream)
  (apply #'write-bytes stream (to-four-bytes 308)))

(defun to-four-bytes (num)
  (list
   (logand (ash num -24) #xff)
   (logand (ash num -16) #xff)
   (logand (ash num -8) #xff)
   (logand num #xff)))

(with-open-file (f "try.png"
		   :direction :output
		   :if-exists :supersede
		   :element-type '(unsigned-byte 8))
  (write-png-signature f) ; signature
  (write-ihdr f) ; ihdr start
  (write-width f) ; width
  (write-height f) ; height
  (write-byte 8 f) ; bit depth
  (write-byte 6 f) ; color type
  (write-byte 0 f) ; compression method
  (write-byte 0 f) ; filter method
  (write-byte 0 f) ; interlace method
  (write-idat f)
  (write-iend f)
)
