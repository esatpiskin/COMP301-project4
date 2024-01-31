(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 
  
  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    ; #####################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### add a new value type for your vectors
    ; #####################################################
    (vec-val
     (vec vec?))

    ; #####################################################
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  ;; HINT if you need extractors, add them here
  (define expval->vec
    (lambda (v)
      (cases expval v
        (vec-val (vec) vec)
        (else (expval-extractor-error 'vector v)))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ; #####################################################
  ; ###### ENTER YOUR CODE HERE
  ; ###### you might want to add a new datatype for vectors here similar 
  ; ###### to mutable pairs.
  ; #####################################################
    (define-datatype vec vec?
      (vector
       (data reference?)
       (capacity number?)
       (size reference?)
       ))

  ; #####################################################

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
  
  ; Natural x Value -> Vector
  ; initialize a new vector with size and set all values to value
  (define vector-new
    (lambda (size value)
      (cond ((< size 1) (eopl:error 'vector-new "vector must have a positive size"))
            (else (vector (vector-new-helper (newref value) value size) size size)))))
  ; helper function to initialize all values within the vector
  (define vector-new-helper
    (lambda (init value n)
      (cond ((= n 0) init)
            (else
             (begin
               (newref value)
               (vector-new-helper init value (- n 1)))))))

  ; Vector x Natural -> Value
  ; get value from the index of vec1
  (define vector-deref
    (lambda (vec1 index)
      (cases vec vec1
        (vector (data size capacity)
                (cond ((or (>= index size) (< index 0))
                       (eopl:error 'vector-deref "illegal access ~s in vector ~s" index vec1))
                      (else
                       (deref (+ data index))))))))

  ; Vector x Natural x Value -> !
  ; set value on index of vec1 to value
   (define vector-setref!
    (lambda (vec1 index value)
      (cases vec vec1
        (vector (data size capacity)
                (cond ((>= index size)
                       (eopl:error 'vector-setref "illegal mutation ~s in vector ~s" index vec1))
                      (else
                       (setref! (+ data index) value)))))))
  ; Vector -> Natural
  ; get the vector length
  (define vector-length
    (lambda (vec1)
      (cases vec vec1
        (vector (data size capacity)
                size))))

  ; Vector x Vector -> !
  ; copy all values from source vector to a destination vector that is as large or bigger
  (define vector-copy!
    (lambda (vec-src vec-dst)
      (cases vec vec-src
        (vector (data size capacity)
                  (cases vec vec-dst
                    (vector (data2 size2 capacity2)
                            (cond ((> data data2)
                                   (eopl:error 'vector-copy "destination vector ~s is too small to copy source vector ~s" vec-dst vec-src))
                                  (else (vector-copy-helper vec-src vec-dst (- size 1))))))))))

  (define vector-copy-helper
    (lambda (vec-src vec-dst n)
      (begin
        (vector-setref! vec-dst n (vector-deref vec-src n))
        (cond
          ((= n 0) vec-dst)
          (else (vector-copy-helper vec-src vec-dst (- n 1)))))))

  ; Natural -> Stack
  ; initialize a new stack
  (define stack-new
    (lambda (max-size)
      (begin
        (let ((size-ref (newref 0)))
         (vector (vector-new-helper (newref 0) 0 max-size) max-size size-ref)))))

  ; Ref x Value x Natural -> Ref
  ; helper function to initialize all values within the vector
  (define stack-new-helper
    (lambda (init value n)
      (cond ((= n 0) init)
            (else
             (begin
               (newref value)
               (vector-new-helper init value (- n 1)))))))

  ; Stack x ExpVal -> !
  ; pushes new value to the stack and increases the size of the stack
  ; if stack is full throws "stackoverflow" error
  (define stack-push!
    (lambda (stack1 value)
      (cases vec stack1
        (vector (data capacity size)
                (if (= (deref size) capacity)
                    (eopl:error "Stack overflow")
                    (begin
                      (vector-setref! stack1 (deref size) value)
                      (setref! size (+ 1 (deref size)))))))))
  ; Stack -> Natural
  ; gets the size of the stack
  (define stack-size
    (lambda (stack1)
      (cases vec stack1
        (vector (data capacity size)
                (deref size)))))

  ; Stack -> Value
  ; gets the top of the stack
  (define stack-peek
    (lambda (stack1)
      (cases vec stack1
        (vector (data capacity size)
                (vector-deref stack1 (- (deref size) 1))))))

  ; Stack -> Value
  ; pops the element from stack and shrinks the size
  ; if there is no element in the stack, returns -1
  (define stack-pop!
    (lambda (stack1)
      (cases vec stack1
        (vector (data capacity size)
                (if (= (deref size) 0)
                    (num-val -1)
                    (begin
                      (setref! size (- (deref size) 1))
                      (vector-deref stack1 (deref size))))))))
  ; Stack -> Bool
  ; check if the stack is empty
  (define stack-empty?
    (lambda (stack1)
      (cases vec stack1
        (vector (data capacity size)
                (= 0 (deref size))))))
      
      
        
)
