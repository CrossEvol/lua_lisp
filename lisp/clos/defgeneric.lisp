  (defgeneric speak (a)
  (:documentation "Make the animal speak."))

(defclass animal ()
  ((name :initarg :name :accessor animal-name)))

(defclass dog (animal) ())

(defmethod speak ((a animal))
  (format t "~a makes a generic animal sound.~%" (animal-name a)))

(defmethod speak ((d dog))
  (format t "~a barks!~%" (animal-name d)))

(let ((generic-animal (make-instance 'animal :name "Generic Animal"))
      (buddy (make-instance 'dog :name "Buddy")))
  (speak generic-animal)  
  (speak buddy)) 