;;Simple utility functions
;;
;;
;;


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

