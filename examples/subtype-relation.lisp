
(in-package :rte)

(let ((sm (rte-to-dfa '(:0-* (:0-1 (:or string 
					     (satisfies oddp)))
				 (satisfies evenp)))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph11.png" :view t :title "string-oddp-satisfies-even"))

(deftype odd ()
  '(and integer (satisfies oddp)))

(deftype even ()
  '(and integer (satisfies evenp)))


(let ((sm (rte-to-dfa '(:0-* (:0-1 (:or string 
					     odd))
				 even))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph12.dot")
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph12.png" :view t :title "string-oddp-even"))


(deftype odd ()
    '(and integer (or (eql 1) (satisfies oddp))))

(deftype even ()
  '(and integer (or (eql 0) (satisfies evenp))))


(let ((sm (rte-to-dfa '(:0-* (:0-1 (:or string 
					     odd))
				 even))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph13.png" :view t :title "string-oddp-or-eql"))


(deftype odd ()
    '(and integer (not (satisfies evenp)) (or (eql 1) (satisfies oddp))))

(deftype even ()
    '(and integer (not (satisfies oddp)) (or (eql 0) (satisfies evenp))))
(let ((sm (rte-to-dfa '(:0-* (:0-1 (:or string 
					     odd))
				 even))))
  (ndfa:ndfa-to-dot sm #p"/tmp/jnewton/graph14.png" :view t :title "string-oddp-not-satisfies"))

