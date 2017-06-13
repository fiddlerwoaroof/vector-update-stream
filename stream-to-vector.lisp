(in-package :vector-update-stream)

(defclass vector-update-stream (fundamental-binary-output-stream)
  ((vector :accessor vector-stream-vector :initarg :vector))
  (:documentation "A binary output stream that writes its data to an associated vector."))

(deftype octet ()
  '(unsigned-byte 8))

(defun make-update-stream (array)
  (unless (and (array-has-fill-pointer-p array)
	       (adjustable-array-p array))
    (error "GET-STREAM-FOR-ARRAY requires an adjustable array with a fill pointer"))
  (make-instance 'vector-update-stream
		 :vector array))

(defun check-if-open (stream)
  "Checks if STREAM is open and signals an error otherwise."
  (unless (open-stream-p stream)
    (error 'flexi-streams:in-memory-stream-closed-error
	   :stream stream)))

(defmethod stream-write-byte ((stream vector-update-stream) byte)
  "Writes a byte \(octet) by extending the underlying vector."
  (check-if-open stream)
  (with-accessors ((vector vector-stream-vector)) stream
    (let ((orig-fill-pointer (fill-pointer vector)))
      (handler-bind ((error (lambda (c)
			      (declare (ignore c))
			      (setf (fill-pointer vector) orig-fill-pointer))))
	(vector-push-extend byte vector)))))

(defmethod stream-write-sequence ((stream vector-update-stream) sequence start end &key)
  "Just calls VECTOR-PUSH-EXTEND repeatedly."
  (declare (fixnum start end))
  (with-accessors ((vector vector-stream-vector)) stream
    (let ((orig-fill-pointer (fill-pointer vector)))
      (handler-bind ((error (lambda (c)
			      (declare (ignore c))
			      (setf (fill-pointer vector) orig-fill-pointer))))
	(loop for index of-type fixnum from start below end
	   do (vector-push-extend (elt sequence index) vector))))
    sequence))

(defmethod stream-file-position ((stream vector-update-stream))
  "Simply returns the fill pointer of the underlying vector."
  (with-accessors ((vector vector-stream-vector)) stream
    (fill-pointer vector)))

(defmethod (setf stream-file-position) (position-spec (stream vector-update-stream))
  "Sets the fill pointer underlying vector if POSITION-SPEC is
acceptable.  Adjusts the vector if necessary."
  (with-accessors ((vector vector-stream-vector))
      stream
    (let* ((total-size (array-total-size vector))
           (new-fill-pointer
            (case position-spec
              (:start 0)
              (:end
               (warn "File position designator :END doesn't really make sense for an output stream.")
               total-size)
              (otherwise
               (unless (integerp position-spec)
                 (error 'flexi-streams:in-memory-stream-position-spec-error
                        :format-control "Unknown file position designator: ~S."
                        :format-arguments (list position-spec)
                        :stream stream
                        :position-spec position-spec))
               (unless (<= 0 position-spec array-total-size-limit)
                 (error 'flexi-streams:in-memory-stream-position-spec-error
                        :format-control "File position designator ~S is out of bounds."
                        :format-arguments (list position-spec)
                        :stream stream
                        :position-spec position-spec))
               position-spec))))
      (declare (fixnum total-size new-fill-pointer))
      (when (> new-fill-pointer total-size)
        (adjust-array vector new-fill-pointer))
      (setf (fill-pointer vector) new-fill-pointer)
      position-spec)))

(defmethod get-output-stream-sequence ((stream vector-update-stream) &key)
  "Returns a vector containing, in order, all the octets that have
been output to the IN-MEMORY stream STREAM. This operation clears any
octets on STREAM, so the vector contains only those octets which have
been output since the last call to GET-OUTPUT-STREAM-SEQUENCE or since
the creation of the stream, whichever occurred most recently.  If
AS-LIST is true the return value is coerced to a list."
  (vector-stream-vector stream))

(defmacro deftest (name (expected actual-init) (&rest bindings) &body actions)
  (alexandria:with-gensyms (assertion-template)
    `(let* ((expected ,expected)
	    (actual ,actual-init)
	    ,@bindings
	    (,assertion-template (formatter "~&~a Test: ~:[fail~%~4texpected ~s~%~4tactual ~s~;succeed~]~%")))
       (declare (ignorable ,assertion-template))
       (flet ((check (assertion)
		    (funcall ,assertion-template t ',name (funcall assertion expected actual) expected actual)))
	 ,@actions)
       (values))))

(defun test ()
  (deftest write-sequence
      (#(1 2 3) (make-array 0 :adjustable t :fill-pointer 0))
      ((vs (make-update-stream actual)))
    (write-sequence expected vs)
    (check 'serapeum:vector=))

  (deftest write-sequence-undo-on-error
      (#(1 2 3) (make-array 0 :adjustable t :fill-pointer 0 :element-type 'octet))
      ((vs (make-update-stream actual)))
    (write-sequence expected vs)
    (handler-case (write-sequence #(1 2 3 #\a) vs)
      (type-error (c) c (values)))
    (check 'serapeum:vector=))

  (deftest write-byte
      (#(1) (make-array 0 :adjustable t :fill-pointer 0))
      ((vs (make-update-stream actual)))
    (write-byte 1 vs)
    (check 'serapeum:vector=))

  (deftest write-byte-undo-on-error
      (#() (make-array 0 :adjustable t :fill-pointer 0 :element-type 'octet))
      ((vs (make-update-stream actual)))
    (handler-case (stream-write-byte vs #\a)
      (type-error (c) c (values)))
    (check 'serapeum:vector=)))
