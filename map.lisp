;; This file is responsible for reading in and
;;printing out the game map.
;;

;;
;;-------------------------------
;; Global vars 
;;-------------------------------
;;

( defvar *map* '() )

;;
;;-------------------------------
;;

( defun clear-map () ( setf *map* '() ) )

;;
;;-------------------------------
;; File Input
;;-------------------------------
;;

( defun add-to-map ( line )
				( let 
					(
					 	( tmp '() )
					)
					( loop for i from 0 to ( - ( length line ) 1 ) do
								 ( setf tmp ( append tmp ( list (char line i ) ) ) ) ) 
					( setf *map* ( append *map* ( list  tmp ) ) ) ) )

( defun read-file ( filename )
				 ( let 
					 (
							( in ( open filename :if-does-not-exist nil ) )
					 )
					 ( when in
									( loop for line = ( read-line in nil )
										while line do 
										( add-to-map line ) )
									( close in ) ) ) )

;;
;;-------------------------------
;;

;;
;;-------------------------------
;; File Output
;;-------------------------------
;;

( defun print-line ( m )
				( cond ( ( null m ) nil )
							 ( ( princ ( car m ) ) ( princ " " ) ( print-line ( cdr m ) ) ) ) ) 

( defun print-map ( m )
				( cond ( ( null m ) nil )
							 ( ( listp ( car m ) ) ( print-line ( car m ) ) ( terpri ) ( print-map ( cdr m ) ) ) ) )

;;
;;-------------------------------
;;

;;
;;-------------------------------
;; Unit tests 
;;-------------------------------
;;

( defun run-unit-test ()
				( read-file "egmap.lay" )
				( print-map *map* ) )

( defun num-occ-test ()
				( princ ( num-occ '( ( 1 2 3 ) ( 2 3 1 ) ) 8 0 ) ) )
;;
;;-------------------------------
;;
