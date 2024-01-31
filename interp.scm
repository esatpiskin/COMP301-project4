(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
        ; ExpVal x ExpVal -> VecVal
        (newvector-exp (length-exp value-exp)
                       (let ((length-val (value-of length-exp env))
                             (value-val (value-of value-exp env)))
                         (vec-val (vector-new (expval->num length-val) value-val))
                       ))
        ; VecVal x ExpVal x ExpVal -> Unspecified
        (update-vector-exp (vec-exp index-exp value-exp)
                           (let ((vec-val (value-of vec-exp env))
                                 (index-val (value-of index-exp env))
                                 (value-val (value-of value-exp env)))
                             (vector-setref! (expval->vec vec-val) (expval->num index-val) value-val)))
        ; VecVal x ExpVal -> ExpVal
        (read-vector-exp (vec-exp index-exp)
                         (let ((vec-val (value-of vec-exp env))
                               (index-val (value-of index-exp env)))
                           (vector-deref (expval->vec vec-val) (expval->num index-val))))
        ; VecVal -> ExpVal
        (length-vector-exp (vec-exp)
                           (let ((vec-val (value-of vec-exp env)))
                             (num-val (vector-length (expval->vec vec-val)))))
        ; VecVal x ExpVal x ExpVal -> Unspecified
        (swap-vector-exp (vec-exp index-exp1 index-exp2)
                         (let ((vec-val (value-of vec-exp env))
                               (index-val1 (value-of index-exp1 env))
                               (index-val2 (value-of index-exp2 env)))
                           (let ((temp-val (vector-deref (expval->vec vec-val) (expval->num index-val1))))
                             (begin
                               (vector-setref! (expval->vec vec-val) (expval->num index-val1) (vector-deref (expval->vec vec-val) (expval->num index-val2)))
                               (vector-setref! (expval->vec vec-val) (expval->num index-val2) temp-val)))))
        ; VecVal -> VecVal
        (copy-vector-exp (vec-exp)
                         (let ((vec-val1 (value-of vec-exp env)))
                           (let ((vec-val2 (vec-val (vector-new (vector-length (expval->vec vec-val1)) 0))))
                             (begin
                               (vector-copy! (expval->vec vec-val1) (expval->vec vec-val2))
                               vec-val2))))

        (vec-mult-exp (vec-exp1 vec-exp2)
                      (let ((vec-val1 (value-of vec-exp1 env))
                            (vec-val2 (value-of vec-exp2 env)))
                        (if
                         (= (vector-length (expval->vec vec-val1)) (vector-length (expval->vec vec-val2)))
                         (let ((vec-val3 (vec-val (vector-new (vector-length (expval->vec vec-val1)) 0))))
                             (vec-val (vec-mult-helper (expval->vec vec-val1)
                                                       (expval->vec vec-val2)
                                                       (expval->vec vec-val3)
                                                       (- (vector-length (expval->vec vec-val3)) 1))))
                             
                         (eopl:error 'vec-mult "vectors ~s and ~s of different length for vector mul" vec-val1 vec-val2))))

        ; create new stack with the given size
	(newstack-exp (s-size)
                      (let ((stack-size (value-of s-size env)))
                        (vec-val (stack-new (expval->num stack-size)))))

        ; adds element to the stack
        (stack-push-exp (stk-exp1 stk-exp2)
                           (let ((stack-val (value-of stk-exp1 env))
                                 (value-val (value-of stk-exp2 env)))
                             (stack-push! (expval->vec stack-val) value-val)))

        ; pops from the stack
        (stack-pop-exp (stk-exp1)
                       (let ((stack (expval->vec (value-of stk-exp1 env))))
                         (stack-pop! stack)))

        ; gets the size of the stack
        (stack-size-exp (stk-exp1)
                        (let ((stack (expval->vec (value-of stk-exp1 env))))
                          (num-val (stack-size stack))))

        ; get the top element of the stack
        (stack-peek-exp (stk-exp1)
                        (let ((stack (expval->vec (value-of stk-exp1 env))))
                          (stack-peek stack)))

        ; checks if the stack is empty
        (empty-stack?-exp (stk-exp1)
                          (let ((stack (expval->vec (value-of stk-exp1 env))))
                                (bool-val (stack-empty? stack))))

        ; prints the elements of the stack
       (print-stack-exp (stk-exp1)
                         (let ((stack (expval->vec (value-of stk-exp1 env))))
                           (stack-print-helper stack 0)))
        ; #####################################################
        )))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
      (define vec-mult-helper
        (lambda (vec1 vec2 vec3 n)
          (begin
            (let ((val1 (expval->num (vector-deref vec1 n)))
                  (val2 (expval->num (vector-deref vec2 n))))
              (vector-setref! vec3 n (num-val (* val1 val2))))
            (if (= n 0)
                vec3
                (vec-mult-helper vec1 vec2 vec3 (- n 1))))))

  ; helper function to print elements of the stack
  (define stack-print-helper
    (lambda (stack1 iter)
      (cases vec stack1
        (vector (data capacity size)
                (cond
                  ((= (deref size) 0)
                   (display "[ ]"))
                  ((= iter 0)
                       (begin
                         (display "[ ")
                         (display (expval->num (vector-deref stack1 (- (deref size) (+ iter 1)))))
                         (display " ")
                         (stack-print-helper stack1 (+ 1 iter))))
                  ((= iter (deref size))
                   (begin
                     (display "]")
                     (newline)))
                  (else
                   (begin
                     (display (expval->num (vector-deref stack1 (- (deref size) (+ iter 1)))))
                     (display " ")
                     (stack-print-helper stack1 (+ 1 iter)))))))))
                     
    

  
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
