(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman sheriff)))
(defun Verb ()        (one-of '(hit took saw liked shot)))

(defun one-of (set)
  (list (random-elt set)))

(defun random-elt (choices)
  (nth (rint (llength choices) ) choices))

(defun Adj* ()
  (if (== (rint 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(TRUE nil))
      (append (PP) (PP*))
      nil))

(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))

; redefinition
(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))

(sentence) ; buggy ?