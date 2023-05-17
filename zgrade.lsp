;***************** Lispovi koji su vezani uz crtanje zgrada *********************


;********************************************************************************
;
;                 Lisp za crtanje zgrade iz tri toèke
;
;                 by pyton, zadnja promjena 14.01.2012. by prexy
;
;********************************************************************************


(defun c:yy ()
  (setvar "cmdecho" 0)
  (setq	t1 (getpoint "\nPrva toèka :")
	t2 (getpoint "\nDruga toèka :")
  )
  (command "line" t1 t2 "")
  (setq e (list (entlast) t1))
  (setq lay (cdr (assoc 8 (entget (entlast)))))
  (setq tl (cdr (assoc 6 (entget (entlast)))))
  (setq pt (getpoint "\nNaznaèi nasuprotnu toèku :"))
  (command "offset" "t" e pt "")
  (setq	e1 (entlast)
	ee nil
	e3 nil
  )
  (setq	e1 (entget e1)
	t3 (cdr (assoc 10 e1))
	t4 (cdr (assoc 11 e1))
  )
  (command "line" t1 t3 "")
  (setq e3 (cons (entlast) e3))
  (setq ee (cons (entget (entlast)) ee))
  (setq os (getvar "OSMODE"))
  (setvar "OSMODE" 0)
  (command "line" t2 t4 "")
  (setq e3 (cons (entlast) e3))
  (setq ee (cons (entget (entlast)) ee))
  (foreach n ee
    (progn (setq n (subst (cons 8 lay) (assoc 8 n) n))
	   (entmod n)
    )
  )
  (if (/= tl nil)
    (foreach n e3 (command "chprop" n "" "lt" tl ""))
  )
  (setvar "OSMODE" os)
  (princ)
)


;********************************************************************************
;
;                 Lisp za crtanje kosih i ravnih šrafura u mjerilu
;
;                 by pyton, zadnja promjena 14.01.2012. by prexy
;
;********************************************************************************


(defun C:q (/ pt1 pt2 pt3 ent1 snap tip kut opi m)
    
	(setvar "cmdecho" 0)
  	(setq snap (getvar "osmode")) ;èitanje trenutne postave OSNAP-a
	(command "OSNAP" "intersection") ;postavljanje OSNAP-a		  

	(initget 1 "Ravna Kosa") ;izbor tipa šrafure
	(setq tip (getkword "\nTip šrafure? (Ravna ili Kosa) "))

	(if (= tip "Ravna")      ;postavljanje kuta rotacije šrafure
		(progn (setq kut 0))
		(setq kut -45)
	);if

	(setq opi (open "c:/vb/set.opi" "r")) (read-line opi) 
	(setq m (read (read-line opi))) (setq h (* m 0.0012)) ;odreðivanje mjerila šrafure
	(setq pt2 (getpoint "\n** Odaberi prvu toèku duže stranice : "))  

(while (/= pt2 nil)

	(setq pt3 (getpoint "\n** Odaberi drugu toèku duže stranice : "))  
	 
	(command "OSNAP" "none")
	(command "UCS" "3" pt2 pt3 "0,0") ;postavljanje pomoænog UCS-a
	  
	(setq pt1 (getpoint "\n** Odaberi centar šrafure : "))
	
	(command "boundary" pt1 "") ;kreiranje pomoænog objekta					  
	(setq ent1 (cdr (assoc -1 (entget(entlast)))))
	(setq ss1 (ssadd))
	(ssadd ent1 ss1) 
	(command "SCALE" ss1 "" pt1 ".5") ;zatvaranje podruèja šrafure
	 
	(command "HATCH" "u" kut h "" ss1 "") ;crtanje user-defined hatch-a
	(command "ERASE" ss1 "") ;brisanje pomoænog objekta
	(command "UCS" "") ;povratak na World UCS
	(command "REDRAW" "")
	 
	(command "OSNAP" "intersection")
	(setq pt2 (getpoint "\n** Odaberi prvu toèku duže stranice : "))  
);while

	(setvar "osmode" snap) ;vraæanje OSNAP-a na poèetnu postavu
	(princ)
);q


;********************************************************************************
;
;                 Lisp za crtanje šrafura prema novom kartografskom kljuèu
;
;                 by prexy, 04.01.2012.
;
;********************************************************************************

(defun C:qq (/ hpname cc snap tip ss1 lastent pt1 ent1)

	(setvar "cmdecho" 0)
  	;ucitaj trenutne vrijednosti sistemskih varijabli
  	(setq hpname (getvar "HPNAME"))
  	(setq cc (getvar "CECOLOR"))
  	(setq snap (getvar "OSMODE"))(setvar "OSMODE" 0)
  
	;izbor vrste objekta
	(initget 128 "Stambena Gospodarska") 
	(setq tip (getkword "Vrsta zgrade (Stambena ili Gospodarska)<Stambena>: "))
  	(if (= tip nil) (setq tip "Stambena"))
	(if (= tip "Stambena")
	  (setvar "CECOLOR" "RGB:150,150,150")
	  (setvar "CECOLOR" "RGB:215,215,215")
	);if
  
  	(setvar "HPNAME" "SOLID")
	(setq ss1 (ssadd))
  	(setq lastent "")

  	(setq pt1 (getpoint "\n** Odaberi toèku unutar zgrade: "))
  	(while (/= pt1 nil)
		(command "_bhatch" "a" "i" "n" "n" "" pt1 "")
		(setq ent1 (cdr (assoc -1 (entget(entlast)))))
		(if (/= lastent (assoc -1 (entget(entlast))))
 			(progn (ssadd ent1 ss1) (setq lastent (assoc -1 (entget(entlast)))))
		);if
  		(setq pt1 (getpoint "\n** Odaberi toèku unutar zgrade: "))
	);while
  
	;postavi srafure iza svega ostaloga
  	(command "_draworder" ss1 "" "b")
  	
  	;vrati vrijednosti sistemskih varijabli
  	(setvar "HPNAME" hpname)
  	(setvar "CECOLOR" cc)
  	(setvar "OSMODE" snap)
	(princ)
);qq


;********************************************************************************
;
;                 Lisp za crtanje šrafura stepenica
;
;                 by pyton, zadnja promjena 14.01.2012. by prexy
;
;********************************************************************************


(defun C:s (/ pt1 pt2 pt3 ent1 snap opi m h ss1)
    
	(setvar "cmdecho" 0)
	(setq snap (getvar "osmode")) ;èitanje trenutne postave OSNAP-a
	(command "OSNAP" "INTERSECTION")
	(command "MEASUREMENT" 1) ;BITNO! ako je postavljeno na 0, neæe dobro raditi!!!!
	 
	(setq opi (open "c:/vb/set.opi" "r")) (read-line opi) 
	(setq m (read (read-line opi))) (setq h (* m 0.000375)) ;odreðivanje mjerila šrafure
	(setq pt2 (getpoint "\n** Odaberi prvu toèku duže stranice stepenica : "))

	(while (/= pt2 nil)

		(setq pt3 (getpoint "\n** Odaberi drugu toèku duže stranice stepenica : "))  
		(command "OSNAP" "NONE")		       
		(command "UCS" "3" pt2 pt3 "0,0")   
		(setq pt1 (getpoint "\n** Odaberi centar šrafure : "))	    
		(command "boundary" pt1 "")
		(setq ent1 (cdr (assoc -1 (entget(entlast)))))
		(setq ss1 (ssadd))
		(ssadd ent1 ss1)
	 	(command "HATCH" "LINE" h "90" ss1 "")
		(command "ERASE" ss1 "")
		(command "UCS" "") 
		(command "REDRAW" "")
		(command "OSNAP" "INTERSECTION")
		(setq pt2 (getpoint "\n** Odaberi prvu toèku duže stranice stepenica : "))  
	);while
  
	(setvar "OSMODE" snap)
	(princ)
);s