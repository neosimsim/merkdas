; Copyright © 2018, Alexander Ben Nasrallah <me@abn.sh>
; Use of this source code is governed by a BSD 3-clause
; style license that can be found in the LICENSE file.

(defparameter *nodes* '((living-room (you are in the living-room.
										  a wizzard is soring loudly on the couch.))
						(garden (you are in a beautiful garden.
									 there is a well in front of you))
						(attic (you are in the attic
									there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
									 (attic upstairs ledder))
						(garden (living-room east door))
						(attic (living-room downstairs ladder))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
								  (bucket living-room)
								  (chain garden)
								  (frog garden)))

(defparameter *location* 'living-room)

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun object-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
  					 (eq (cadr (assoc obj obj-locs)) loc)))
  	(remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
  						 `(you see a ,obj on the floor.)))
  	(apply #'append (mapcar #'describe-obj (object-at loc objs obj-locs)))))

(defun look ()
  (append (describe-location *location* *nodes*)
  		  (describe-paths *location* *edges*)
  		  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
  					(cdr (assoc *location* *edges*))
  					:key #'cadr)))
  	(if next
  	  (progn (setf *location* (car next))
  	  		 (look))
  	  '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object (object-at *location* *objects* *object-locations*))
  		 (push (list object 'body) *object-locations*)
  		 `(you are now carrying the ,object))
  		(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (object-at 'body *objects* *object-locations*)))

(print (look))
(print (walk 'west))
(print (pickup 'frog))
(print (look))
(print (pickup 'nothing))
(print (inventory))
