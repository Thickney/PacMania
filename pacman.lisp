( load "utils.lisp" )
( load "map.lisp" )

;;
;;-------------------------------
;; Global vars 
;;-------------------------------
;;

( defvar *pacman* #\P )
( defvar *wall*   #\% )
( defvar *space* #\SPACE )
;;
;;-------------------------------
;;

;; 
;; Finds pacman in the given board.
;; returns the positon of pacman in a list
;; ( x y )
( defun find-pacman ( board n pacman ) 
				( cond ( ( null board ) nil )
							 ( t
								 ( let 
									 (
										  ( x ( index-of ( car board ) pacman 0 ) ) 
									 )
									 ( cond 
										 ( ( null x ) ( find-pacman ( cdr board ) ( + n 1 ) pacman ) )
										 ( t ( list x n ) ) ) ) ) ) )

( defun is-it-safe ( board new-pos )
				( cond
					( ( equal ( nth ( car new-pos ) ( nth ( cadr new-pos ) board ) ) *wall* ) nil )
					( t t ) ) )

( defun move-pacman ( board new-pos old-pos pacman )
				( cond
					( ( equal ( find-pacman board 0 pacman ) old-pos )
						( setf ( nth ( car new-pos ) ( nth ( cadr new-pos ) board ) ) pacman )
						( setf ( nth ( car old-pos ) ( nth ( cadr old-pos ) board ) ) " " ) )
					( t nil ) ) )

;;
;;-------------------------------
;; Pacman Moves
;;-------------------------------
;;

( defun pacman-up ( board pacman )
				( let
					(
					  ( pos ( find-pacman board 0 pacman ) )
					)
					( cond
						( ( null pos ) nil )
						( ( null ( is-it-safe board ( list ( car pos ) ( - ( cadr pos ) 1 ) ) ) ) nil ) 
						( t ( move-pacman board ( list ( car pos ) ( - ( cadr pos ) 1 ) ) pos pacman ) board ) ) ) )

( defun pacman-down ( board pacman )
				( let
					(
					  ( pos ( find-pacman board 0 pacman ) )
					)
					( cond
						( ( null pos ) nil )
						( ( null ( is-it-safe board ( list ( car pos ) ( + ( cadr pos ) 1 ) ) ) ) nil ) 
						( t ( move-pacman board ( list ( car pos ) ( + ( cadr pos ) 1 ) ) pos pacman ) board ) ) ) )

( defun pacman-left ( board pacman )
				( let
					(
					  ( pos ( find-pacman board 0 pacman ) )
					)
					( cond
						( ( null pos ) nil )
						( ( null ( is-it-safe board ( list ( - ( car pos ) 1 ) ( cadr pos ) ) ) ) nil ) 
						( t ( move-pacman board ( list ( - (car pos ) 1 ) ( cadr pos ) ) pos pacman ) board ) ) ) )


( defun pacman-right ( board pacman )
				( let
					(
					  ( pos ( find-pacman board 0 pacman ) )
					)
					( cond
						( ( null pos ) nil )
						( ( null ( is-it-safe board ( list ( + ( car pos ) 1 ) ( cadr pos ) ) ) ) nil ) 
						( t ( move-pacman board ( list ( + (car pos ) 1 ) ( cadr pos ) ) pos pacman ) board ) ) ) )
;;
;;-------------------------------
;;

;;
;;-------------------------------
;; Unit tests 
;;-------------------------------
;;

( defun run-some-thing ()
				( clear-map )
				( read-file "egmap.lay" )
				( print-map *map* )

				( pacman-up *map* *pacman* )  
				( print-map *map* ) 
				
				( pacman-down *map* *pacman* )  
				( print-map *map* ) 
				
				( pacman-right *map* *pacman* )  
				( print-map *map* ) 
				
				( pacman-left *map* *pacman* )  
				( print-map *map* ) ) 


( defvar *t-board* '( ( "." "." "." ) ( "." "P" "." ) ) )
( defun test-move-pacman ()
				( princ ( move-pacman *t-board* '( 0 1 ) '( 1 1 ) *pacman* ) ) )

;; in order for these tests to work change *pacman* and *wall* to "P" and "%" respectivly

( defun test-find-pacman ()
				( princ ( find-pacman '( ( " " "P" " " ) ( "D" "E" "F" ) ) 0 *pacman* ) ( terpri ) ) 
				( princ ( find-pacman '( ( "P" " " "C" ) ( "D" "E" "F" ) ) 0 *pacman*) ( terpri ) ) 
				( princ ( find-pacman '( ( "A" " " "C" ) ( "D" "P" "F" ) ) 0 *pacman*) ( terpri ) ) 
				( princ ( find-pacman '( ( "A" " " "C" ) ( "D" " " "P" ) ) 0 *pacman*) ( terpri ) ) 
				( princ ( find-pacman '( ( "A" " " "C" ) ( "P" " " " " ) ) 0 *pacman*) ( terpri ) ) 
				( princ ( find-pacman '( ( "A" " " "C" ) ( " " " " " " ) ( " " " " "P" ) ) 0 *pacman*) ( terpri ) ) 
				( princ ( find-pacman '( ( "P" "P" "P" ) ( "P" "P" "P" ) ) 0 *pacman*) ( terpri ) ) 
				( princ ( find-pacman '( ( "X" "X" "X" ) ( "X" "X" "X" ) ) 0 *pacman*) ( terpri ) ) )

( defun test-is-it-safe ()
				( princ ( is-it-safe '( ( "%" "P" "%" ) ) '( 2 0 ) ) ) ( princ " should be nil" ) ( terpri ) 
				( princ ( is-it-safe '( ( "%" "P" " " ) ) '( 2 0 ) ) ) ( princ " should be T" ) ( terpri ) 
				( princ ( is-it-safe '( ( "%" "P" "%" ) ) '( 0 0 ) ) ) ( princ " should be nil" ) ( terpri ) 
				( princ ( is-it-safe '( ( " " "P" "%" ) ) '( 0 0 ) ) ) ( princ " should be T" )( terpri ) 
				( princ ( is-it-safe '( ( "%" "%" "%" ) ( "%" "P" "%" )  ) '( 1 0 ) ) ) ( princ " should be nil" ) ( terpri ) 
				( princ ( is-it-safe '( ( "%" " " "%" ) ( "%" "P" "%" )  ) '( 1 0 ) ) ) ( princ " should be T" ) ( terpri ) 
				( princ ( is-it-safe '( ( "%" "P" "%" ) ( "%" "%" "%" )  ) '( 1 1 ) ) ) ( princ " should be nil" ) ( terpri ) 
				( princ ( is-it-safe '( ( "%" "P" "%" ) ( "%" " " "%" )  ) '( 1 1 ) ) ) ( princ " should be T" ) ( terpri ) ) 
;;
;;-------------------------------
;;
