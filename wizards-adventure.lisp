;; Chapter 5 of _Land of Lisp_.

;; p. 70
(defparameter *nodes* '((living-room (you are in the living-room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                     there is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding torch in the corner.))))

;; p. 71
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

;; p. 73
;; this uses quasiquoting.  ` starts it and within a quasiquote
;; the comma means "unquote the next thing and replace the comma
;; with its eval. 
;;
;; E.g., (let ((x 1)) `(+ 3 ,x)) => (+ 3 1).
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; apply the append function to the result of
;; calling describe-path on the car of the list of (location direction edge-type) tuples.
;; #'foo -> (function foo).  It refers to the function given by the quoted symbol.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter  *objects* '(whisky bucket frog chain))

(defparameter *object-locations* '((whisky living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))
;; p. 78
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
                     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
                         `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(look)
