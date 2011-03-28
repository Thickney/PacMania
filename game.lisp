;; The main game engine
;;
;;
;;
( load "utils.lisp" )
( load "map.lisp" )
( load "search.lisp" )

( defvar *orig-map* '() )
( defvar *pp* #\o )
( defvar *dot* #\. )

( defun game-over-pacman-wins ( board )
				( cond
					( ( and ( = ( count-occ board *dot* 0 ) 0 ) ( = ( count-occ board *pp* 0 ) 0 ) ) t )
					( t nil ) ) )

( defun game-over-pacman-loses ( board ) 
				( cond 
					( ( = ( count-occ board *pacman* 0 ) 0 ) t )
					( t nil ) ) )

( defun game-state ( board timer )
				 ( cond
					 ( ( = timer 90 ) nil )
					 ( ( game-over-pacman-wins board ) nil )
					 ( ( game-over-pacman-loses board ) nil )
					 ( t t ) ) )

( defun game-engine ( board timer )
				( print-map board )
				( cond
					( ( null ( game-state board timer ) ) ( princ "done" ) ) 
			    ( t ( game-engine ( chose-random ( gen-states board ) ) ( + timer 1 ) ) ) ) )
				;( cond
				;	( ( null ( game-state board timer ) ) nil )
				;	( t ( game-engine board ( + timer 1 ) ) ) ) )

( defun run-random ()
			 ( clear-map )
			 ( read-file "egmap.lay" )
			 ( setf *orig-map* '() )
			 ( setf *orig-map* ( deep-cpy *map* '() ) )
			 ( game-engine ( deep-cpy *orig-map* '() ) 0 ) )

( defun show-results ( X )
				( cond 
					( ( null X ) nil )
					( t ( print-map ( get-state ( car X ) ) ) (sleep .7)  ( show-results ( cdr X ) ) ) ) )


( defun run-dfs ()
				( clear-map )
				( read-file "egmap.lay" )
				( run-search ( deep-cpy *map* '() ) "dfs" )
				( show-results *results* ) )

( defun run-bfs ()
				( clear-map )
				( read-file "egmap.lay" )
				( run-search ( deep-cpy *map* '() ) "bfs" )
				( show-results *results* ) )

( defun run () )


