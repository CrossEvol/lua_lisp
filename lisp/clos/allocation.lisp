(defclass person ()
  ((name :initarg :name :accessor name)
   (species
      :initform 'homo-sapiens
      :accessor species
      :allocation :class)))

;; Note that the slot "lisper" was removed in existing instances.
(inspect p1)
;; The object is a STANDARD-OBJECT of type PERSON.
;; 0. NAME: "me"
;; 1. SPECIES: HOMO-SAPIENS
;; > q

(defvar p2 (make-instance 'person))

(species p1)
(species p2)
;; HOMO-SAPIENS

(setf (species p2) 'homo-numericus)
;; HOMO-NUMERICUS

(species p1)
;; HOMO-NUMERICUS

(species (make-instance 'person))
;; HOMO-NUMERICUS