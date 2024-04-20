(in-package :flow-cl.propagate-test)

;;; Inspired by logic programming and dependent typing
;;; Inspired by the Screamer library
;;; Inspired by the "layers" concept: https://www.youtube.com/watch?v=EbzQg7R2pYU

(defclass lfunction (c2mop:funcallable-standard-object)
  ((%func :accessor func :initarg :func)
   (%layers :accessor layers :initarg :layers)
   (%layer-deps :accessor layer-deps :initarg :layer-deps))
  (:metaclass c2mop:funcallable-standard-class)
  (:default-initargs :func (lambda () (values))
                     :layers nil
                     :layer-deps nil))

(defvar *layer-outputs* nil
  "A dynamic variable tracking layer outputs. Used in `lfuncall' so layers can depend on each other")

(defmethod lfuncall ((f lfunction) args &optional layer-args)
  (s:nest
   (let ((base-output (apply (func f) args))
         (layer-deps (layer-deps f))))
   (let ((*layer-outputs* (acons nil base-output nil))
         (layers (sort (copy-list (layers f))
                       (lambda (a b)
                         (member a (assoc b layer-deps)))))))

   (iter (for (name func) in layers)
     (for layer-inputs = (assoc name layer-args))
     (when layer-inputs
       (setf *layer-outputs*
             (acons name (apply func layer-inputs)
                    *layer-outputs*))))
   (finally)
   (return)

   (values base-output *layer-outputs*)))

(defmethod initialize-instance :after ((f lfunction) &key &allow-other-keys)
  (c2mop:set-funcallable-instance-function f (lambda (&rest args) (lfuncall f args))))

(defun test ()
  (print (funcall (make-instance 'lfunction)))
  (print (funcall (make-instance 'lfunction :func (lambda () "hi" "bye")))))
