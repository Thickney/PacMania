;; The main game engine
;;
;;
;;

( load "pacman.lisp" )
( load "utils.lisp" )
( load "map.lisp" )

( defvar *orig-map* '() )


( defun gen-states ( state )
				( remove-null ( list 
						( list ( pacman-up    ( deep-cpy state '() ) *pacman* ) )
						( list ( pacman-down  ( deep-cpy state '() ) *pacman* ) )
						( list ( pacman-left  ( deep-cpy state '() ) *pacman* ) )
						( list ( pacman-right ( deep-cpy state '() ) *pacman* ) ) ) ) )  

( defun remove-null ( list )
				( cond
					( ( null list ) nil )
					( ( null ( caar list ) ) ( remove-null ( cdr list ) ) )
					( t ( cons ( car list ) ( remove-null ( cdr list ) ) ) ) ) )

( defun game-state ( board timer )
				 ( cond
					 ( ( = timer 90 ) nil )
					 ( t t ) ) )

( defun chose-random ( moves )
				( let
					(
					 	( rand ( random ( length moves ) ) )
					)
				( car ( nth rand moves ) ) ) )

( defun game-engine ( board timer )
				( print-map board )
				( cond
					( ( null ( game-state board timer ) ) ( princ "done" ) ) 
			    ( t ( game-engine ( chose-random ( gen-states board ) ) ( + timer 1 ) ) ) ) )
				;( cond
				;	( ( null ( game-state board timer ) ) nil )
				;	( t ( game-engine board ( + timer 1 ) ) ) ) )

( defun simple-run ()
			 ( clear-map )
			 ( read-file "egmap.lay" )
			 ( setf *orig-map* '() )
			 ( setf *orig-map* ( deep-cpy *map* '() ) )
			 ( game-engine ( deep-cpy *orig-map* '() ) 0 ) )


( defun run () )


