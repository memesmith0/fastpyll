;;this code is gpl v2 found at gnu.org
;;this code is written by john morris beck
;;https://github.com/memesmith0



(use-modules (ice-9 textual-ports)
             (ice-9 eval-string)
)


(define fastpyll_indent (lambda (x) (if (= x 0) "" (string-append "    " (fastpyll_indent (- x 1))))))

(define fastpyll_curry (lambda (. x) (if (= (length x) 0) "" (if (> (length x) 1) (fastpyll_curry (car x) " " (fastpyll_curry (cdr x))) (car x)))))

(define fastpyll_helper (lambda (d x) (if (pair? x) (string-append (fastpyll_indent d) (car x) "\n\n" (fastpyll_helper d (cdr x))) "")))

(define fastpyll_codeblock (lambda (d x . y) (string-append x ":\n\n" (fastpyll_helper d y))))



(define fastpyll_unary_operation (lambda (x y) (fastpyll_group x y)))



(define fastpyll_string (lambda (x) (string-append "\"" x "\"")))
(define fastpyll_unpack (lambda (x) (string-append "*" x)))

(define fastpyll_fstring (lambda (x) (string-append "f" (fastpyll_string x))))

(define fastpyll_comment (lambda (x) (fastpyll_curry "#" x "\n\n")))

(define fastpyll_key (lambda (x y) (fastpyll_curry x ": " y)))


(define fastpyll_check_for_structure  (lambda (x)
			      (if (and (symbol? (car x))
				       (or

					

					(eq? (car x) 'fastpyll_define)
				       (eq? (car x) 'fastpyll_print)
				       (eq? (car x) 'fastpyll_while)
				       (eq? (car x) 'fastpyll_if)
				       (eq? (car x) 'fastpyll_else_if)
				       (eq? (car x) 'fastpyll_for)
				       
				       (eq? (car x) 'fastpyll_else)
				       (eq? (car x) 'fastpyll_try)
				       (eq? (car x) 'fastpyll_except)
				       (eq? (car x) 'fastpyll_finally)

				       )
				       
				       )
				  #t
				  #f
				  )
			      )
  )




	(define fastpyll_import (lambda (x) (string-append "import " x)))
	(define fastpyll_assign (lambda (x y) (string-append x " = " y)))
	(define fastpyll_none (lambda () "None"))
	(define fastpyll_pgroup (lambda (left right . x) (apply string-append (append (list left) (fastpyll_arguments x) (list right )))))
	(define fastpyll_group (lambda (. x) (apply fastpyll_pgroup (append (list "( " " )") x))))
	(define fastpyll_arguments (lambda x (if (= (length x) 1) (car x) (if (> (length x) 1) (string-append (car x) " , " (apply fastpyll_arguments (cdr x)))""))))
	(define fastpyll_array (lambda (. x) (apply fastpyll_pgroup (list "[ " " ]" (apply fastpyll_arguments x)))))
	(define fastpyll_call (lambda (x . y) (string-append x "( " (apply fastpyll_arguments y) " )")))
	(define fastpyll_in (lambda (x y) (string-append x " in " y)))

(define fastpyll_range (lambda ( . x) (apply fastpyll_call (append (list "range") x))))
(define fastpyll_access_helper (lambda (x) (string-append "[ " (if (> (length x) 0) (car x) "") " ]" (if (> (length x) 1) (fastpyll_access_helper (cdr x)) ""))))
(define fastpyll_access (lambda (b . x) (string-append b (fastpyll_access_helper x))))
(define fastpyll_dot_helper (lambda (x) (string-append "." (if (> (length x) 0) (car x) "")  (if (> (length x) 1) (fastpyll_dot_helper (cdr x)) ""))))
(define fastpyll_dot (lambda (b . x) (string-append b (fastpyll_dot_helper x))))


(define fastpyll_dictionary_helper (lambda ( . x)
			    (if (= 0 (length x))
				""
		     (if (and (= 0 (modulo (length x) 2)) (> (length x) 1))
			 (string-append (car x) ": " (car (cdr x))
					(if (> (length (cdr (cdr x))) 1)
					(string-append " , " (apply fastpyll_dictionary_helper (cdr (cdr x)))) ""))
			 (error 'dictionary "wrong number of arguments to dictionary")))))

(define fastpyll_dictionary (lambda ( . x) (string-append "{ " (apply fastpyll_dictionary_helper x) " }")))
(define fastpyll_dot_dictionary_helper (lambda ( . x)
			    (if (= 0 (length x))
				""
		     (if (and (= 0 (modulo (length x) 2)) (> (length x) 1))
			 (string-append (fastpyll_fstring (car x)) ": " (car (cdr x))
					(if (> (length (cdr (cdr x))) 1)
					(string-append " , " (apply fastpyll_dot_dictionary_helper (cdr (cdr x)))) ""))
			 (error 'dot_dictionary "wrong number of arguments to dot_dictionary")))))

(define fastpyll_dot_dictionary (lambda ( . x) (string-append "{ " (apply fastpyll_dot_dictionary_helper x) " }")))

(define fastpyll_pappend (lambda (x y) (fastpyll_call (fastpyll_d x "append") y)))
(define fastpyll_print (lambda ( . a ) (apply fastpyll_call (append (list "print") (list (fastpyll_arguments a))))))
(define fastpyll_true (lambda () "True"))
(define fastpyll_false (lambda () "False"))
(define fastpyll_none (lambda () "None"))

	(define fastpyll_binary_operation (lambda (operation x y) (fastpyll_group x " " operation " "  y)))
(define fastpyll_equal (lambda (x y) (fastpyll_binary_operation "==" x y)))
(define fastpyll_comma (lambda (x y) (string-append x " , " y)))
	(define fastpyll_subtract (lambda (x y) (fastpyll_binary_operation "-" x y)))
	(define fastpyll_integer_divide (lambda (x y) (fastpyll_binary_operation "//" x y)))
	(define fastpyll_float_divide (lambda (x y) (fastpyll_binary_operation "/" x y)))
	(define fastpyll_multiply (lambda (x y) (fastpyll_binary_operation "*" x y)))

	(define fastpyll_add (lambda (x y) (fastpyll_binary_operation "+" x y)))
(define fastpyll_and (lambda (x y) (fastpyll_binary_operation "and" x y)))
(define fastpyll_delete (lambda (x) (string-append "del " x )))
(define fastpyll_or (lambda (x y) (fastpyll_binary_operation "or" x y)))
(define fastpyll_less_than (lambda (x y) (fastpyll_binary_operation "<" x y)))
(define fastpyll_greater_than (lambda (x y) (fastpyll_binary_operation ">" x y)))
(define fastpyll_subtract (lambda (x y) (fastpyll_binary_operation "-" x y)))
	(define fastpyll_not_equal (lambda (x y) (fastpyll_binary_operation "!=" x y)))
	(define fastpyll_not (lambda (x) (string-append "( " "not " x " )")))
	(define fastpyll_tilda (lambda (x) (string-append "~" "( " x " )")))
	(define fastpyll_set_intersection (lambda (x y) (fastpyll_binary_operation "&" x y)))
	(define fastpyll_set_union (lambda (x y) (fastpyll_binary_operation "|" x y)))
	(define fastpyll_global (lambda ( . y) (string-append "global " (apply fastpyll_arguments y))))
(define fastpyll_return (lambda ( . a ) (apply string-append "return " (fastpyll_arguments a))))



(define fastpyll_for (lambda (d x . y) (string-append "for " x ":\n\n" (fastpyll_helper d y))))

(define fastpyll_define (lambda (d x a . y)
				 (string-append "def " x "( " (apply fastpyll_arguments a) " ):\n\n" (fastpyll_helper d y))))




(define fastpyll_while (lambda (d x . y) (string-append "while " x ":\n\n" (fastpyll_helper d y))))
(define fastpyll_if (lambda (d x . y) (string-append "if " x ":\n\n" (fastpyll_helper d y))))
(define fastpyll_else_if (lambda (d x . y) (string-append "elif " x ":\n\n" (fastpyll_helper d y))))
(define fastpyll_else (lambda (d . y) (string-append "else:\n\n" (fastpyll_helper d y))))
(define fastpyll_try (lambda (d . y) (string-append "try:\n\n" (fastpyll_helper d y))))
(define fastpyll_except (lambda (d x . y) (string-append "except " x ":\n\n" (fastpyll_helper d y))))
(define fastpyll_finally (lambda (d . y) (string-append "finally:\n\n" (fastpyll_helper d y))))


(define fastpyll_stringify
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_stringify #t counter (car x))) (let ((foo (fastpyll_stringify #f counter (cdr x))))
						     (if (list? foo)
							 foo
							 (list foo))))

      )


     ((and (not b) (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_stringify #t counter (car x))) (let ((foo (fastpyll_stringify #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))

      (append (list (car x))
		  (let ((foo (fastpyll_stringify #f counter (cdr x))))
		    (if (list? foo)
			foo
			(list foo))))
      
      )



     ((and b (list? (car x)) (not (> (length x) 1))) 

      (list (fastpyll_stringify #t counter (car x)))

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append (list (cond ((string? (car x)) (fastpyll_fstring (car x)))
			  ((symbol? (car x)) (symbol->string (car x)))
			  ((number? (car x)) (number->string (car x)))

			))



	      (let ((foo (fastpyll_stringify #f counter (cdr x))))
			       (if (list? foo)
				   foo
				   (list foo))))
      
      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_stringify #t counter (car x)))

      )


     ((and  b (not (list? (car x))) (not (> (length x) 1))) x)


     ((and (not b) (not (list? (car x))) (not (> (length x) 1)))
      (cond ((string? (car x)) (fastpyll_fstring (car x)))
	    ((symbol? (car x)) (symbol->string (car x)))
	    ((number? (car x)) (number->string (car x)))))
     )
    )
  )







(define fastpyll_add_indentation
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_add_indentation #t counter (car x))) (let ((foo (fastpyll_add_indentation #f counter (cdr x))))
						     (if (list? foo)
							 foo
							 (list foo))))

      )


     ((and (not b) (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_add_indentation #t counter (car x))) (let ((foo (fastpyll_add_indentation #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))

      (append (list (car x))
	      (if (fastpyll_check_for_structure x)
		  (append (list counter) (let ((foo (fastpyll_add_indentation #f (+ counter 1) (cdr x))))
					   (if (list? foo)
					       foo
					       (list foo)))
			  )
		  (let ((foo (fastpyll_add_indentation #f counter (cdr x))))
		    (if (list? foo)
			foo
			(list foo)))))
      
      )



     ((and b (list? (car x)) (not (> (length x) 1))) 

      (list (fastpyll_add_indentation #t counter (car x)))

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append (list (car x)) (let ((foo (fastpyll_add_indentation #f counter (cdr x))))
			       (if (list? foo)
				   foo
				   (list foo))))
      
      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_add_indentation #t counter (car x)))

      )


     ((and  b (not (list? (car x))) (not (> (length x) 1))) x)


     ((and (not b) (not (list? (car x))) (not (> (length x) 1))) (car x))
     )
    )
  )


(define fastpyll_is_fastpyll_primative
  (lambda (symbol)

    (cond
     ((eq? symbol 'indent) 'fastpyll_indent)
     ((eq? symbol 'string) 'fastpyll_string)
     ((eq? symbol 'fstring) 'fastpyll_fstring)
     ((eq? symbol 'comment) 'fastpyll_comment)
     ((eq? symbol 'import) 'fastpyll_import)
     ((eq? symbol 'assign) 'fastpyll_assign)
     ((eq? symbol 'none) 'fastpyll_none)
     ((eq? symbol 'group) 'fastpyll_group)
     ((eq? symbol 'comma) 'fastpyll_comma)
     ((eq? symbol 'arguments) 'fastpyll_arguments)
     ((eq? symbol 'array) 'fastpyll_array)
     ((eq? symbol 'in) 'fastpyll_in)
     ((eq? symbol 'dot_dictionary) 'fastpyll_dot_dictionary)
     ((eq? symbol 'range) 'fastpyll_range)
     ((eq? symbol 'dot) 'fastpyll_dot)
     ((eq? symbol 'access) 'fastpyll_access)
     ((eq? symbol 'fstring) 'fastpyll_fstring)
     ((eq? symbol 'dictionary) 'fastpyll_dictionary)
     ((eq? symbol 'append) 'fastpyll_append)
     ((eq? symbol 'true) 'fastpyll_true)
     ((eq? symbol 'false) 'fastpyll_false)
     ((eq? symbol 'delete) 'fastpyll_delete)
     ((eq? symbol 'none) 'fastpyll_none)
     ((eq? symbol 'equal) 'fastpyll_equal)
     ((eq? symbol 'add) 'fastpyll_add)
     ((eq? symbol 'integer_divide) 'fastpyll_integer_divide)
     ((eq? symbol 'float_divide) 'fastpyll_float_divide)
     ((eq? symbol 'multiply) 'fastpyll_multiply)
     ((eq? symbol 'and) 'fastpyll_and)
     ((eq? symbol 'or) 'fastpyll_or)
     ((eq? symbol 'not_equal) 'fastpyll_not_equal)
     ((eq? symbol 'not) 'fastpyll_not)
     ((eq? symbol 'tilda) 'fastpyll_tilda)
     ((eq? symbol 'less_than) 'fastpyll_less_than)
     ((eq? symbol 'greater_than) 'fastpyll_greater_than)
     ((eq? symbol 'set_intersection) 'fastpyll_set_intersection)
     ((eq? symbol 'vertical_bar) 'fastpyll_set_union)
     ((eq? symbol 'global) 'fastpyll_global)
     ((eq? symbol 'return) 'fastpyll_return)
     ((eq? symbol 'subtract) 'fastpyll_subtract)
     ((eq? symbol 'for) 'fastpyll_for)
     ((eq? symbol 'define) 'fastpyll_define)
     ((eq? symbol 'while) 'fastpyll_while)
     ((eq? symbol 'if) 'fastpyll_if)
     ((eq? symbol 'else_if) 'fastpyll_else_if)
     ((eq? symbol 'else) 'fastpyll_else)
     ((eq? symbol 'try) 'fastpyll_try)
     ((eq? symbol 'except) 'fastpyll_except)
     ((eq? symbol 'finally) 'fastpyll_finally)
     (#t 'foop)
     )





     )
    )
  

(define fastpyll_precall
  (lambda (symbol_a . y)
    (string-append symbol_a "( " (apply fastpyll_arguments y) " )")))



(define fastpyll_add_prefix
  (lambda (symbol_a)
    (if (eq? symbol_a 'list)
	(list symbol_a)
	(let* (( fastpyll_symbol (fastpyll_is_fastpyll_primative symbol_a)))
	  (if (not (eq? fastpyll_symbol 'foop))
	      (list fastpyll_symbol)

	      (list 'fastpyll_precall (symbol->string symbol_a))

	      )



	    
    


	)

    )))









(define fastpyll_change_names
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_change_names #t counter (car x))) (let ((foo (fastpyll_change_names #f counter (cdr x))))
						     (if (list? foo)
							 foo
							 (list foo))))
      )


     ((and (not b) (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_change_names #t counter (car x))) (let ((foo (fastpyll_change_names #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))


      (append 

	       (fastpyll_add_prefix (car x))


	      (append (let ((foo (fastpyll_change_names #f counter (cdr x))))
					   (if (list? foo)
					       foo
					       (list foo)))
		      )
	      )
      )



     ((and b (list? (car x)) (not (> (length x) 1))) 

      (list (fastpyll_change_names #t counter (car x)))

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append (list (car x)) (let ((foo (fastpyll_change_names #f counter (cdr x))))
			       (if (list? foo)
				   foo
				   (list foo))))
      
      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_change_names #t counter (car x)))

      )

     ((and  b (not (list? (car x))) (not (> (length x) 1))) (fastpyll_add_prefix (car x)))



     ((and (not b) (not (list? (car x))) (not (> (length x) 1))) (car x))
     )
    )
  )






















(define fastpyll_string_precall
  (lambda (symbol_a . y)
    (string-append symbol_a "( " (apply fastpyll_arguments y) " )")))



(define fastpyll_add_prefix_to_string
  (lambda (symbol_a)
    (if (string? symbol_a)
	(list 'fastpyll_string_precall symbol_a)
					symbol_a

)


    )
  )





(define fastpyll_string_apply
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))

      (let ((bar (fastpyll_string_apply #t counter (car x))))
	(if (string? bar)
	    (fastpyll_string_apply #t counter (append

					       (fastpyll_add_prefix_to_string bar)

					       (cdr x)
					       ))
					       

	    (append 
		 (let ((foo (fastpyll_string_apply #f counter (cdr x))))
		(if (list? foo)
		    foo
		    (list foo))))
					     ))
      )


     ((and (not b) (list? (car x)) (> (length x) 1))


      (append (list (fastpyll_string_apply #t counter (car x))) (let ((foo (fastpyll_string_apply #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))

      
      (apply

       (eval (car x) (interaction-environment))


	      (append (let ((foo (fastpyll_string_apply #f counter (cdr x))))
					   (if (list? foo)
					       foo
					       (list foo))))
	      )
      )




     ((and b (list? (car x)) (not (> (length x) 1)))

      (let ((bar (fastpyll_string_apply #t counter (car x))))
	(if (string? bar)
	    (fastpyll_string_apply #t counter (append

					       (fastpyll_add_prefix_to_string bar)
))
					       



	    (list (fastpyll_string_apply #t counter (car x)))
	    )
	)

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append 

       (list

		     (car x)
)




	      (let ((foo (fastpyll_string_apply #f counter (cdr x)))) (if (list? foo) foo (list foo))))


      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_string_apply #t counter (car x)))

      )

     ((and  b (not (list? (car x))) (not (> (length x) 1)))

      (apply (eval (car x) (interaction-environment)) (list))
      )
      

								   
								   



     ((and (not b) (not (list? (car x))) (not (> (length x) 1)))


      (car x)
      
	   
      )

    )
    )
  )
  
  



  


(define fastpyll_fastpyll_helper
  (lambda (x)
  (begin
    (string-append

(fastpyll_string_apply #t 1 
			    (fastpyll_add_indentation #t 1
     (fastpyll_change_names #t 1


			    (fastpyll_stringify #t 1 x)
			    )
     )
			    )


  

			    
     
     

"\n\n"))))

(define fastpyll 
  (lambda (x)
    (apply string-append (map fastpyll_fastpyll_helper x))))



(define fastpyll_c_indent (lambda (x) (if (= x 0) "" (string-append "    " (fastpyll_c_indent (- x 1))))))

(define fastpyll_c_curry (lambda (. x) (if (= (length x) 0) "" (if (> (length x) 1) (fastpyll_c_curry (car x) " " (fastpyll_c_curry (cdr x))) (car x)))))

(define fastpyll_c_helper (lambda (d x) (if (pair? x) (string-append (fastpyll_c_indent d) (car x) ";\n\n" (fastpyll_c_helper d (cdr x))) (string-append (fastpyll_c_indent d) "}"))))



(define fastpyll_c_struct_helper (lambda (d x) (if (pair? x) (string-append (fastpyll_c_indent d) (car x) ";\n\n" (fastpyll_c_struct_helper d (cdr x))) (string-append (fastpyll_c_indent d) ""))))


(define fastpyll_c_codeblock (lambda (d x . y) (string-append x ":\n\n" (fastpyll_c_helper d y))))



(define fastpyll_c_unary_operation (lambda (x y) (fastpyll_c_group x y)))



(define fastpyll_c_string (lambda (x) (string-append "\"" x "\"")))
(define fastpyll_c_star (lambda (x) (string-append "*" x)))

(define fastpyll_c_fstring (lambda (x) (string-append "f" (fastpyll_c_string x))))

(define fastpyll_c_comment (lambda (x) (fastpyll_c_curry "//" x "\n\n")))

(define fastpyll_c_check_for_structure  (lambda (x)
			      (if (and (symbol? (car x))
				       (or

					

					(eq? (car x) 'fastpyll_c_define)
				       (eq? (car x) 'fastpyll_c_while)
				       (eq? (car x) 'fastpyll_c_if)
				       (eq? (car x) 'fastpyll_c_else_if)
				       (eq? (car x) 'fastpyll_c_for)
				       (eq? (car x) 'fastpyll_c_struct)
				       
				       (eq? (car x) 'fastpyll_c_else)
				       )
				       
				       )
				  #t
				  #f
				  )
			      )
  )




	(define fastpyll_c_include (lambda (x) (string-append "#include " x)))
	(define fastpyll_c_assign (lambda (x y) (string-append x " = " y)))
	(define fastpyll_c_pgroup (lambda (left right . x) (apply string-append (append (list left) (fastpyll_c_arguments x) (list right )))))
	(define fastpyll_c_group (lambda (. x) (apply fastpyll_c_pgroup (append (list "( " " )") x))))
(define fastpyll_c_arguments (lambda (. x) (if (= (length x) 1) (car x) (if (> (length x) 1) (string-append (car x) " , " (apply fastpyll_c_arguments (cdr x)))""))))

(define fastpyll_c_asm_arguments (lambda (. x) (if (= (length x) 1) (car x) (if (> (length x) 1) (string-append (car x) " " (apply fastpyll_c_arguments (cdr x)))""))))
(define fastpyll_c__asm__ (lambda ( . x ) (string-append "__asm__( " (apply fastpyll_c_asm_arguments x)" )")))
	(define fastpyll_c_array (lambda (. x) (apply fastpyll_c_pgroup (list "{ " " }" (apply fastpyll_c_arguments x)))))
	(define fastpyll_c_call (lambda (x . y) (string-append x "( " (apply fastpyll_c_arguments y) " )")))
(define fastpyll_c_access_helper (lambda (x) (string-append "[ " (if (> (length x) 0) (car x) "") " ]" (if (> (length x) 1) (fastpyll_c_access_helper (cdr x)) ""))))
(define fastpyll_c_access (lambda (b . x) (string-append b (fastpyll_c_access_helper x))))
(define fastpyll_c_dot_helper (lambda (x) (string-append "." (if (> (length x) 0) (car x) "")  (if (> (length x) 1) (fastpyll_c_dot_helper (cdr x)) ""))))
(define fastpyll_c_dot (lambda (b . x) (string-append b (fastpyll_c_dot_helper x))))

(define fastpyll_c_arrow_helper (lambda (x) (string-append "->" (if (> (length x) 0) (car x) "")  (if (> (length x) 1) (fastpyll_c_dot_helper (cdr x)) ""))))
(define fastpyll_c_arrow (lambda (b . x) (string-append b (fastpyll_c_dot_helper x))))



(define fastpyll_c_binary_operation (lambda (operation x y) (fastpyll_c_group x " " operation " "  y)))
(define fastpyll_c_equal (lambda (x y) (fastpyll_c_binary_operation "==" x y)))
(define fastpyll_c_subtract (lambda (x y) (fastpyll_c_binary_operation "-" x y)))
(define fastpyll_c_divide (lambda (x y) (fastpyll_c_binary_operation "/" x y)))

(define fastpyll_c_multiply (lambda (x y) (fastpyll_c_binary_operation "*" x y)))
(define fastpyll_c_add (lambda (x y) (fastpyll_c_binary_operation "+" x y)))
(define fastpyll_c_and (lambda (x y) (fastpyll_c_binary_operation "&&" x y)))
(define fastpyll_c_bitwise_and (lambda (x y) (fastpyll_c_binary_operation "&" x y)))
(define fastpyll_c_or (lambda (x y) (fastpyll_c_binary_operation "||" x y)))
(define fastpyll_c_bitwise_or (lambda (x y) (fastpyll_c_binary_operation "|" x y)))
(define fastpyll_c_less_than (lambda (x y) (fastpyll_c_binary_operation "<" x y)))
(define fastpyll_c_greater_than (lambda (x y) (fastpyll_c_binary_operation ">" x y)))
(define fastpyll_c_less_than_or_equal_to (lambda (x y) (fastpyll_c_binary_operation "<=" x y)))
(define fastpyll_c_greater_than_or_equal_to (lambda (x y) (fastpyll_c_binary_operation ">=" x y)))
(define fastpyll_c_bitshift_left (lambda (x y) (fastpyll_c_binary_operation "<<" x y)))
(define fastpyll_c_bitshift_right (lambda (x y) (fastpyll_c_binary_operation ">>" x y)))
(define fastpyll_c_ternary_operation (lambda (x y z) (string-append "( ( " x " ) ? ( " y " ) : ( " z " ) )")))


(define fastpyll_c_subtract (lambda (x y) (fastpyll_c_binary_operation "-" x y)))
(define fastpyll_c_not_equal (lambda (x y) (fastpyll_c_binary_operation "!=" x y)))
(define fastpyll_c_not (lambda (x) (string-append "( " "!" "( " x " )" " )")))
(define fastpyll_c_bitwise_negate (lambda (x) (string-append "( " "~( " x " ) )")))
(define fastpyll_c_return (lambda ( a ) (string-append "return " a)))

(define fastpyll_c_system_library (lambda ( a ) (string-append "<" a ">")))



(define fastpyll_c_for (lambda (d x . y) (string-append "for( " x " ) {\n\n" (fastpyll_c_helper d y))))

(define fastpyll_c_struct (lambda (d x a . y) (string-append "struct " x " {\n\n" (fastpyll_c_struct_helper d y) "} " a " ;")))

(define fastpyll_c_define (lambda (d x a . y)
				 (string-append x "( " (apply fastpyll_c_arguments a) " ) {\n\n" (fastpyll_c_helper d y))))




(define fastpyll_c_while (lambda (d x . y) (string-append "while( " x " ) {\n\n" (fastpyll_c_helper d y))))
(define fastpyll_c_if (lambda (d x . y) (string-append "if( " x " ) {\n\n" (fastpyll_c_helper d y))))
(define fastpyll_c_else_if (lambda (d x . y) (string-append "else if( " x " ) {\n\n" (fastpyll_c_helper d y))))
(define fastpyll_c_else (lambda (d . y) (string-append "else {\n\n" (fastpyll_c_helper d y))))
(define fastpyll_c_switch (lambda (a d . y) (string-append "switch( " a " ) {\n\n" (fastpyll_c_helper d y))))
(define fastpyll_c_case (lambda (a d . y) (string-append "case " a " :\n\n" (fastpyll_c_helper d y))))
(define fastpyll_c_do_while (lambda () "havent implemented do while yet"))
(define fastpyll_c_do_struct (lambda () "havent implemented struct yet"))
(define fastpyll_c_do_literal_character (lambda (x) "'" x "'"))
(define fastpyll_c_do_literal_address (lambda (x) "&" x))
(define fastpyll_c_type_cast (lambda (x y) (string-append "( " y " )" y)))
(define fastpyll_c_label (lambda (x) (string-append x ":")))
(define fastpyll_c_goto (lambda (x) (string-append "goto " x)))
(define fastpyll_c_type (lambda (type x) (string-append type " " x)))
(define fastpyll_c_void (lambda (x) (fastpyll_c_type "void" x)))
(define fastpyll_c_const (lambda (x) (fastpyll_c_type "const" x)))
(define fastpyll_c_extern (lambda (x) (fastpyll_c_type "extern" x)))
(define fastpyll_c_static (lambda (x) (fastpyll_c_type "static" x)))
(define fastpyll_c_int (lambda (x) (fastpyll_c_type "int" x)))
(define fastpyll_c_short (lambda (x) (fastpyll_c_type "short" x)))
(define fastpyll_c_long (lambda (x) (fastpyll_c_type "long" x)))
(define fastpyll_c_unsigned (lambda (x) (fastpyll_c_type "unsigned" x)))
(define fastpyll_c_float (lambda (x) (fastpyll_c_type "float" x)))
(define fastpyll_c_float_literal (lambda (x) (string-append x "f")))
(define fastpyll_c_post_increment (lambda (x) (string-append x "++")))
(define fastpyll_c_pre_increment (lambda (x) (string-append "++" x)))
(define fastpyll_c_post_decrement (lambda (x) (string-append x "--")))
(define fastpyll_c_pre_decrement (lambda (x) (string-append "--" x)))


(define fastpyll_c_define_macro (lambda (x y z) (string-append "#define " x "( " (apply fastpyll_c_arguments y) " ) " z )))

(define fastpyll_c_end_if (lambda () "endif"))
(define fastpyll_c_if_not_defined (lambda (x) (string-append "#ifndef " x)))
(define fasstpyll_c_enum (lambda (x y) (string-append "enum " x " " y)))
(define fastpyll_c_type_define (lambda (x y ) (string-append "typedef " x " " y)))

(define fastpyll_c_type_define_struct (lambda ( . a) (string-append "typedef " (apply fastpyll_c_struct a))))


(define fastpyll_c_stringify
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_c_stringify #t counter (car x))) (let ((foo (fastpyll_c_stringify #f counter (cdr x))))
						     (if (list? foo)
							 foo
							 (list foo))))

      )


     ((and (not b) (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_c_stringify #t counter (car x))) (let ((foo (fastpyll_c_stringify #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))

      (append (list (car x))
		  (let ((foo (fastpyll_c_stringify #f counter (cdr x))))
		    (if (list? foo)
			foo
			(list foo))))
      
      )



     ((and b (list? (car x)) (not (> (length x) 1))) 

      (list (fastpyll_c_stringify #t counter (car x)))

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append (list (cond ((string? (car x)) (fastpyll_c_string (car x)))
			  ((symbol? (car x)) (symbol->string (car x)))
			  ((number? (car x)) (number->string (car x)))

			))



	      (let ((foo (fastpyll_c_stringify #f counter (cdr x))))
			       (if (list? foo)
				   foo
				   (list foo))))
      
      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_c_stringify #t counter (car x)))

      )


     ((and  b (not (list? (car x))) (not (> (length x) 1))) x)


     ((and (not b) (not (list? (car x))) (not (> (length x) 1)))
      (cond ((string? (car x)) (fastpyll_c_string (car x)))
	    ((symbol? (car x)) (symbol->string (car x)))
	    ((number? (car x)) (number->string (car x)))))
     )
    )
  )







(define fastpyll_c_add_indentation
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_c_add_indentation #t counter (car x))) (let ((foo (fastpyll_c_add_indentation #f counter (cdr x))))
						     (if (list? foo)
							 foo
							 (list foo))))

      )


     ((and (not b) (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_c_add_indentation #t counter (car x))) (let ((foo (fastpyll_c_add_indentation #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))

      (append (list (car x))
	      (if (fastpyll_c_check_for_structure x)
		  (append (list counter) (let ((foo (fastpyll_c_add_indentation #f (+ counter 1) (cdr x))))
					   (if (list? foo)
					       foo
					       (list foo)))
			  )
		  (let ((foo (fastpyll_c_add_indentation #f counter (cdr x))))
		    (if (list? foo)
			foo
			(list foo)))))
      
      )



     ((and b (list? (car x)) (not (> (length x) 1))) 

      (list (fastpyll_c_add_indentation #t counter (car x)))

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append (list (car x)) (let ((foo (fastpyll_c_add_indentation #f counter (cdr x))))
			       (if (list? foo)
				   foo
				   (list foo))))
      
      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_c_add_indentation #t counter (car x)))

      )


     ((and  b (not (list? (car x))) (not (> (length x) 1))) x)


     ((and (not b) (not (list? (car x))) (not (> (length x) 1))) (car x))
     )
    )
  )

(define fastpyll_c_is_fastpyll_c_primative
  (lambda (symbol)
    (cond
     ((eq? symbol 'indent) 'fastpyll_c_indent)
     ((eq? symbol 'string) 'fastpyll_c_string)
     ((eq? symbol 'comment) 'fastpyll_c_comment)
     ((eq? symbol 'include) 'fastpyll_c_include)
     ((eq? symbol 'assign) 'fastpyll_c_assign)
     ((eq? symbol 'group) 'fastpyll_c_group)
     ((eq? symbol 'arguments) 'fastpyll_c_arguments)
     ((eq? symbol 'array) 'fastpyll_c_array)
     ((eq? symbol 'dot) 'fastpyll_c_dot)
     ((eq? symbol 'arrow) 'fastpyll_c_arrow)
     ((eq? symbol 'system_library) 'fastpyll_c_system_library)
     ((eq? symbol 'star) 'fastpyll_c_star)
     ((eq? symbol 'access) 'fastpyll_c_access)
     ((eq? symbol 'string) 'fastpyll_c_string)
     ((eq? symbol 'append) 'fastpyll_c_append)
     ((eq? symbol 'equal) 'fastpyll_c_equal)
     ((eq? symbol 'less_than_or_equal_to) 'fastpyll_c_less_than_or_equal_to)
     ((eq? symbol 'greater_than_or_equa_to) 'fastpyll_c_greater_than_or_equal_to)
     ((eq? symbol 'ternary_operation) 'ternary_operation)
     ((eq? symbol 'switch) 'fastpyll_c_switch)
     ((eq? symbol 'case) 'fastpyll_c_case)
     ((eq? symbol 'and) 'fastpyll_bitwise_and)
     ((eq? symbol 'bitshift_left) 'fastpyll_bitwise_bitshift_left)
     ((eq? symbol 'bitshift_right) 'fastpyll_bitwise_bitshift_right)
     ((eq? symbol 'or) 'fastpyll_bitwise_or)
     ((eq? symbol 'add) 'fastpyll_c_add)
     ((eq? symbol 'if_not_defined) 'fastpyll_c_if_not_defined)
     ((eq? symbol 'end_if) 'fastpyll_c_end_if)
     ((eq? symbol 'enum) 'fastpyll_c_enum)
     ((eq? symbol 'type_define) 'fastpyll_c_type_define)
     ((eq? symbol 'type_define_struct) 'fastpyll_c_type_define_struct)
     ((eq? symbol 'divide) 'fastpyll_c_divide)
     ((eq? symbol 'multiply) 'fastpyll_c_multiply)
     ((eq? symbol '__asm__) 'fastpyll_c__asm__)
     ((eq? symbol 'define_macro) 'fastpyll_c_define_macro)
     ((eq? symbol 'and) 'fastpyll_c_and)
     ((eq? symbol 'or) 'fastpyll_c_or)
     ((eq? symbol 'not_equal) 'fastpyll_c_not_equal)
     ((eq? symbol 'not) 'fastpyll_c_not)
     ((eq? symbol 'bitwise_negate) 'fastpyll_c_bitwise_negate)
     ((eq? symbol 'less_than) 'fastpyll_c_less_than)
     ((eq? symbol 'greater_than) 'fastpyll_c_greater_than)
     ((eq? symbol 'vertical_bar) 'fastpyll_c_set_union)
     ((eq? symbol 'global) 'fastpyll_c_global)
     ((eq? symbol 'return) 'fastpyll_c_return)
     ((eq? symbol 'subtract) 'fastpyll_c_subtract)
     ((eq? symbol 'for) 'fastpyll_c_for)
     ((eq? symbol 'define) 'fastpyll_c_define)
     ((eq? symbol 'while) 'fastpyll_c_while)
     ((eq? symbol 'struct) 'fastpyll_c_struct)
     ((eq? symbol 'type_cast) 'fastpyll_c_type_cast)
     ((eq? symbol 'case) 'fastpyll_c_case)
     ((eq? symbol 'do_while) 'fastpyll_c_do_while)
     ((eq? symbol 'if) 'fastpyll_c_if)
     ((eq? symbol 'else_if) 'fastpyll_c_else_if)
     ((eq? symbol 'else) 'fastpyll_c_else)
     ((eq? symbol 'label) 'fastpyll_c_label)
     ((eq? symbol 'goto) 'fastpyll_c_goto)
     ((eq? symbol 'type) 'fastpyll_c_type)
     ((eq? symbol 'void) 'fastpyll_c_void)
     ((eq? symbol 'const) 'fastpyll_c_const)
     ((eq? symbol 'extern) 'fastpyll_c_extern)
     ((eq? symbol 'static) 'fastpyll_c_static)
     ((eq? symbol 'int) 'fastpyll_c_int)
     ((eq? symbol 'short) 'fastpyll_c_short)
     ((eq? symbol 'long) 'fastpyll_c_long)
     ((eq? symbol 'unsigned) 'fastpyll_c_unsigned)
     ((eq? symbol 'float) 'fastpyll_c_float)
     ((eq? symbol 'float_literal) 'fastpyll_c_float_literal)
     ((eq? symbol 'post_increment) 'fastpyll_c_post_increment)
     ((eq? symbol 'post_pre_increment) 'fastpyll_c_pre_increment)
     ((eq? symbol 'post_post_increment) 'fastpyll_c_post_increment)
     ((eq? symbol 'post_pre_decrement) 'fastpyll_c_pre_decrement)
     ((eq? symbol 'post_post_decrement) 'fastpyll_c_post_decrement)
     (#t 'foop)
     )





     )
    )
  

(define fastpyll_c_precall
  (lambda (symbol_a . y)
    (string-append symbol_a "( " (apply fastpyll_c_arguments y) " )")))



(define fastpyll_c_add_prefix
  (lambda (symbol_a)
    (if (eq? symbol_a 'list)
	(list symbol_a)
	(let* (( fastpyll_c_symbol (fastpyll_c_is_fastpyll_c_primative symbol_a)))
	  (if (not (eq? fastpyll_c_symbol 'foop))
	      (list fastpyll_c_symbol)

	      (list 'fastpyll_c_precall (symbol->string symbol_a))

	      )



	    
    


	)

    )))









(define fastpyll_c_change_names
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_c_change_names #t counter (car x))) (let ((foo (fastpyll_c_change_names #f counter (cdr x))))
						     (if (list? foo)
							 foo
							 (list foo))))
      )


     ((and (not b) (list? (car x)) (> (length x) 1))

      (append (list (fastpyll_c_change_names #t counter (car x))) (let ((foo (fastpyll_c_change_names #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))
      (append 

	       (fastpyll_c_add_prefix (car x))


	      (append (let ((foo (fastpyll_c_change_names #f counter (cdr x))))
					   (if (list? foo)
					       foo
					       (list foo)))
		      )
	      )
      )



     ((and b (list? (car x)) (not (> (length x) 1))) 

      (list (fastpyll_c_change_names #t counter (car x)))

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append (list (car x)) (let ((foo (fastpyll_c_change_names #f counter (cdr x))))
			       (if (list? foo)
				   foo
				   (list foo))))
      
      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_c_change_names #t counter (car x)))

      )
     ((and  b (not (list? (car x))) (not (> (length x) 1))) (fastpyll_c_add_prefix (car x)))



     ((and (not b) (not (list? (car x))) (not (> (length x) 1))) (car x))
     )
    )
  )






















(define fastpyll_c_string_precall
  (lambda (symbol_a . y)
    (string-append symbol_a "( " (apply fastpyll_c_arguments y) " )")))



(define fastpyll_c_add_prefix_to_string
  (lambda (symbol_a)
    (if (string? symbol_a)
	(list 'fastpyll_c_string_precall symbol_a)
					symbol_a

)


    )
  )





(define fastpyll_c_string_apply
  (lambda (b counter x)
    (cond
     ((and b (list? (car x)) (> (length x) 1))
      (let ((bar (fastpyll_c_string_apply #t counter (car x))))
	(if (string? bar)
	    (fastpyll_c_string_apply #t counter (append

					       (fastpyll_c_add_prefix_to_string bar)

					       (cdr x)
					       ))
					       

	    (append 
		 (let ((foo (fastpyll_c_string_apply #f counter (cdr x))))
		(if (list? foo)
		    foo
		    (list foo))))
					     ))
      )


     ((and (not b) (list? (car x)) (> (length x) 1))


      (append (list (fastpyll_c_string_apply #t counter (car x))) (let ((foo (fastpyll_c_string_apply #f counter (cdr x))))
							    (if (list? foo)
								foo
								(list foo))))
      

      )



     ((and  b (not (list? (car x))) (> (length x) 1))
      (apply

       (eval (car x) (interaction-environment))


	      (append (let ((foo (fastpyll_c_string_apply #f counter (cdr x))))
					   (if (list? foo)
					       foo
					       (list foo))))
	      )
      )




     ((and b (list? (car x)) (not (> (length x) 1)))
      (let ((bar (fastpyll_c_string_apply #t counter (car x))))
	(if (string? bar)
	    (fastpyll_c_string_apply #t counter (append

					       (fastpyll_c_add_prefix_to_string bar)
))
					       



	    (list (fastpyll_c_string_apply #t counter (car x)))
	    )
	)

      )

     

     ((and (not b) (not (list? (car x))) (> (length x) 1))

      (append 

       (list

		     (car x)
)




	      (let ((foo (fastpyll_c_string_apply #f counter (cdr x)))) (if (list? foo) foo (list foo))))


      
      )

     

     ((and (not b) (list? (car x)) (not (> (length x) 1)))

      (list (fastpyll_c_string_apply #t counter (car x)))

      )
     ((and  b (not (list? (car x))) (not (> (length x) 1)))

      (apply (eval (car x) (interaction-environment)) (list))
      )
      

								   
								   



     ((and (not b) (not (list? (car x))) (not (> (length x) 1)))


      (car x)
      
	   
      )

    )
    )
  )
(define fastpyll_c_fastpyll_c_helper
  (lambda (x)
    (string-append

(fastpyll_c_string_apply #t 1 
			    (fastpyll_c_add_indentation #t 1
     (fastpyll_c_change_names #t 1


			    (fastpyll_c_stringify #t 1 x)
			    )
     )
			    )


  

			    
     
     

  "\n\n")))

(define fastpyll_c
  (lambda (x)
    (apply string-append (map fastpyll_c_fastpyll_c_helper x))))

;examples
					;(display (fastpyll '((print "hello") (print "goodbye"))))
					;(display (fastpyll_c '((print "hello") (print "goodbye"))))
;;to load this library (load "/path/to/fastpyll.scm"
;;this language requires guile 3.0 to run
