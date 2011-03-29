;;Simple utility functions
;;
;;
;;

(defun manhattan (pos1 pos2)
	(cond
		( ( some (null pos1) (null pos2)) nil)
		(T
			(+ 	(abs (- (nth 0 pos1) (nth 0 pos2)))
				(abs (- (nth 1 pos1) (nth 1 pos2)))
			)
		)
	)
)

;; 
;; Finds the given item in the given board.
;; returns the all the positions of the item in a list:
;; (( x1 y1 ) (x2 x2))
( defun find-item ( board item ) 
	( cond
		( ( some ( NULL board ) ( NULL item ) ) nil )
		( T ( let 	(( y 0 )
					 ( x 0 ))
			( loop for i in board do
				( setq x 0 )
				nconc ( loop for j in i
					when (string= j item) collect (LIST x y) 
					do ( incf x )
				)
				do ( incf y )
			))
		)  	
	)
)

;this is specific to vt100, vt200, xterm and xterm-color terminals :( 
(defun Clear-Screen ()
  (let ((String " [2J [H"))
    (setf (aref String 0) #\Escape)
    (setf (aref String 4) #\Escape)
        (princ String)
    (values) ))
    
;;
;;-------------------------------
;; List based functions 
;;-------------------------------
;;

( defun index-of ( L x n )
				( cond ( ( null L ) nil )
							 ( ( equal ( car L ) x ) n )
							 ( t ( index-of ( cdr L ) x  ( + n 1 ) ) ) ) )


;;
;;-------------------------------
;;

;;
;;-------------------------------
;; Unit tests
;;-------------------------------
;;

( defun test-index-of ()
				( princ ( index-of '( 1 2 3 ) 1 0 ) ) ( terpri )
				( princ ( index-of '( 1 2 3 ) 2 0 ) ) ( terpri )
				( princ ( index-of '( 1 2 3 ) 3 0 ) ) ( terpri )
				( princ ( index-of '( 1 2 3 ) 4 0 ) ) ( terpri ) )
				

;;
;;-------------------------------
;;

;;
;;-------------------------------
;; 
;;-------------------------------
;;

;; count the number of occurances
;; of an item in a given board
( defun count-occ ( board item n )
				( cond
					( ( null board ) n )
					( ( position item ( car board ) ) ( count-occ ( cdr board ) item ( + n 1 ) ) )
					( t ( count-occ ( cdr board ) item n ) ) ) )


;;
;;-------------------------------
;;

;;
;;-------------------------------
;; my deep copy function
;;-------------------------------
;;

( defun deep-cpy ( A B )
				( cond
					( ( null A ) B )
					( ( and ( null B ) ( listp ( car A ) ) ) ( setq B ( deep-cpy ( car A ) '() ) ) ( setq B ( append  ( list B ) ( deep-cpy ( cdr A ) '() ) ) ) )
					( ( null B ) ( setq B ( list ( car A ) ) ) ( setq B ( append B ( deep-cpy ( cdr A ) '() ) ) ) ) ) )
					;( t ( setq B ( append B ( deep-cpy ( cdr A) B ) ) ) ) ) )

;;
;;-------------------------------
;;

