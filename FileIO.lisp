;;
;;
;;
;;

( defvar *game-map* '() )

( defun add-to-map ( line )
				( let 
					(
					 	( tmp '() )
					)
					( loop for i from 0 to ( - ( length line ) 1 ) do
								 ( setf tmp ( append tmp ( list (char line i ) ) ) ) ) 
					( setf *game-map* ( append *game-map* ( list  tmp ) ) ) ) )

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

( defun print-line ( m )
				( cond ( ( null m ) nil )
							 ( ( princ ( car m ) ) ( princ " " ) ( print-line ( cdr m ) ) ) ) ) 

( defun print-map ( m )
				( cond ( ( null m ) nil )
							 ( ( listp ( car m ) ) ( print-line ( car m ) ) ( terpri ) ( print-map ( cdr m ) ) ) ) )

( defun run() 
				( read-file "egmap.lay" ) 
				( print-map *game-map* ) )
