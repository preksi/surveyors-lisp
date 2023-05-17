;*******************************************************
;*******************************************************
;	  Lisp za reinsertiranje bloka
;	  izborom vec postojeceg bloka na dwg-u
;		 by prexy, 04.02.2001.
;*******************************************************
;*******************************************************

(defun C:RI ()
   
	(setq blok (entsel "\nSelektiraj blok"))
		
		(if blok 
		(progn
			(setq bl (entget (car blok)))
			(if (= (cdr (assoc 0 bl)) "INSERT")
			(progn
				
				(setq blok2 (cdr (assoc 2 bl)))
				(setq blscale (cdr (assoc 41 bl)))
				(setq pt (getpoint "\nInsertna toèka:"))

				(command "INSERT" blok2  pt  blscale  blscale)

			);progn
			(alert "Selektirani objekt nije blok! ")
			);if
		);progn
		(princ "\nPromašaj!")
		);if

(princ)
);RI


;*************************************************************************************
;  	               BX by prexy 24.04.2000, zadnja promjena 14.01.2012.
;       	sve instance nekog bloka unutar dwg-a zamjenjuju se drugim dwg blokom
;*************************************************************************************

(defun C:BX()

	(setq bldef (entsel "Selektiraj blok:"))
	(setq e (entget (car bldef)))
	(setq bln (cdr (assoc 2 e)))
	(setq ss1 (ssget "x" (list (assoc 2 e))))
	(setq putanja "g:/simboli")
  	(if (= (findfile putanja) nil)
	  (setq putanja (getvar "DWGPREFIX"))
	  (setq putanja (strcat putanja "/"))
	);if
	(setq wf (getfiled "Izbor datoteke" putanja "DWG" 8))
  	(if (= wf nil) (quit))
	(COMMAND "INSERT" wf "0,0" 1 "" 0 "")
	(setq ee (entget (entlast)))
	(entdel (entlast))

	(setq i 0)

	(repeat (sslength ss1)
		(setq ent (ssname ss1 i))
		(setq ed (entget ent))
		(setq ed (subst (assoc 2 ee) (assoc 2 ed) ed))
		(entmod ed)
		(setq i (+ i 1))
	)
	(COMMAND "PURGE" "B" BLN "N")
);BX


;*************************************************************************************
;  	                        	RB by prexy, 24.04.2000.
;       	               svi izabrani blokovi rotiraju se za zadani kut
;*************************************************************************************


(defun C:RB (/ e f inspt sca ss cmde)
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (print)
  (setq sca (getreal "Rotacija: "))
  (setq c 0)
  (if ss (setq e (ssname ss c)))
  
  (while e
    (setq f (entget e))
    (setq inspt (cdr (assoc 10 f)))
    (COMMAND "ROTATE" e "" inspt sca)
    (PRINC ".")
    (setq c (1+ c))
    (setq e (ssname ss c))
  )
  (setvar "CMDECHO" 1)
  (princ)
);RB


;*************************************************************************************
;  	                        	SB by prexy, 24.04.2000.
;       	  svi izabrani blokovi skaliraju se za zadano mjerilo (scalefactor)
;*************************************************************************************



(defun C:SB (/ e f inspt sca ss cmde)
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (print)
  (setq sca (getreal "Mjerilo: "))
  (setq c 0)
  (if ss (setq e (ssname ss c)))
  
  (while e
    (setq f (entget e))
    (setq inspt (cdr (assoc 10 f)))
    (COMMAND "SCALE" e "" inspt sca)
    (PRINC ".")
    (setq c (1+ c))
    (setq e (ssname ss c))
  )
  (setvar "CMDECHO" 1)
  (princ)
);SB