( load "pacman" )
( load "utils" )
;;-------------------------
;; This is a simple set of moves to make pacman 
;; move randomly through the game map.
;;-----------------------

;;
;;----------------------
;; Global vars
;;----------------------
;;

( defvar *open*   '() )
( defvar *close*  '() )
( defvar *results* '() )

( defun clear-open-list   () ( setf *open*   '() ) )
( defun clear-close-list  () ( setf *close*  '() ) )
( defun clear-result-list () ( setf *results* '() ) )

;;
;;----------------------
;;

;;
;;----------------------
;; Open-close list functions
;;----------------------
;;

( defun remove-from-open-list () ( setq *open* ( cdr *open* ) ) )

( defun add-to-close-list ( state ) ( setq *close* ( append *close* ( list state ) ) ) )

( defun add-to-open-list ( states s-type )
				( cond
					( ( equal s-type "dfs" ) ( setq *open* ( append states *open* ) ) )
					( ( equal s-type "bfs" ) ( setq *open* ( append *open* states ) ) ) ) )

( defun add-to-results ( state ) ( setq *results* ( append ( list state ) *results* ) ) )

;;
;;----------------------
;;


;;
;;----------------------
;; Search functions 
;;----------------------
;;

( defun goalp ( state ) ( and ( = ( count-occ state #\. 0 ) 0 ) ( = ( count-occ state #\o 0 ) 0 ) ) )

( defun back-trace ( state )
				( add-to-results state )
				( let
					 (
							( parent ( has-move *close* ( get-parent state ) ) )
					 )
					 ( cond
						 ( ( null parent ) nil )
						 ( t ( back-trace parent ) ) ) ) )

( defun run-search ( start-state s-type )
				( clear-open-list   )
				( clear-close-list  )
				( clear-result-list )
				( add-to-open-list ( list ( make-state start-state '() 'start ) ) s-type ) 
				( m-search s-type ) )

( defun m-search ( s-type )
				( cond
					( ( null *open* ) nil )
					( ( goalp ( get-state ( car *open* ) ) ) ( back-trace ( car *open* ) ) )
					( t ( let
								(
								  ( children ( remove-visited ( gen-states ( get-state ( car *open* ) ) ) ) )
								)
								( add-to-close-list ( car *open* ) )
								( remove-from-open-list )
								( add-to-open-list children s-type )
								( m-search s-type )
								) ) ) )


;;
;;----------------------
;;

;;
;;----------------------
;; Simple state functions
;;----------------------
;;

( defun make-state ( state parent move )
				( list state parent move ) )

( defun get-state ( state )  ( nth 0 state ) )
( defun get-parent ( state ) ( nth 1 state ) )
( defun get-move   ( state ) ( nth 2 state ) )

( defun gen-states ( state )
				( remove-null ( list 
						( make-state ( pacman-up    ( deep-cpy state '() ) *pacman* ) ( deep-cpy state '() ) 'pacman-up    )  
						( make-state ( pacman-down  ( deep-cpy state '() ) *pacman* ) ( deep-cpy state '() ) 'pacman-down  )   
						( make-state ( pacman-left  ( deep-cpy state '() ) *pacman* ) ( deep-cpy state '() ) 'pacman-left  )  
						( make-state ( pacman-right ( deep-cpy state '() ) *pacman* ) ( deep-cpy state '() ) 'pacman-right ) ) ) )  

( defun remove-null ( list )
				( cond 
					( ( null list ) nil )
					( ( null ( get-state ( car list ) ) ) ( remove-null ( cdr list ) ) )
					( t ( cons ( car list ) ( remove-null ( cdr list ) ) ) ) ) )


( defun has-move ( list x )
				( cond
					( ( null list ) nil )
					( ( equal ( get-state ( car list ) ) x ) ( car list ) )
					( t ( has-move ( cdr list ) x ) ) ) )

( defun remove-visited ( list )
				( cond
					( ( null list ) nil )
					( ( has-move *close* ( get-state ( car list ) ) ) ( remove-visited ( cdr list ) ) )
					( t ( cons ( car list ) ( remove-visited ( cdr list ) ) ) ) ) )

;;;
;;;-------------------
;;;

( defun chose-random ( moves )
				( let
					(
					 	( rand ( random ( length moves ) ) )
					)
				( get-state ( nth rand moves ) ) ) )




