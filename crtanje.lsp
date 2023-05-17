;********************************  C  R  T  A  NJ  E  ********************************



;********************************* O R T O G O N A L**********************************
;*****                                                                           *****
;*****                             lisp za ortogonal                             *****
;*****                          by prexy, 15.07.2001.god.                        *****
;*****               zadnja promjena by prexy, 15.01.2012.god.                   *****
;*************************************************************************************

(defun setort (/ lay stl)

	(setq lay "0_ortogonal")
	(if (tblsearch "layer" lay)
		(command "LAYER" "T" lay "U" lay "ON" lay "S" lay "")
	  	(progn
		  (if (not (tblsearch "ltype" "_GEO_kontr")) (command ".linetype" "L" "_GEo_kontr" "acadiso.lin" ""))
		  (command "LAYER" "m" lay "c" "white" "" "L" "_GEO_kontr" "" "s" "" "") ;stvaranje layera
		);progn
	);if
  	(command "LINETYPE" "s" "BYLAYER" "") ;setiranje linetype-a
	(setq stl "rec")
  	(if (tblsearch "style" stl)
	  	(setvar "textstyle" "rec")
		(command "STYLE" "rec" "simplex" 0 0.8 0 "N" "N" "N") ;postavljanje text stila
	);if
  
);setort

(defun ucsset()
	
	;funkcija za setiranje privremenog UCS-a

	(command "UCS" "o" pt1)
	(command "UCS" "z" pt1 pt2 "")

) ;ucsset

(defun nula()
	
	;fukcija za ispis prve i zadnje apscise

	(ucsset)
	(setq ptt (list 0 (* -1 h) 0))
	(command "TEXT" ptt h "-90" "0.00")
	(setq ptt (list linija (* -1 h) 0))
	(command "TEXT" ptt h "-90" (rtos linija 2 dec))
	(command "UCS" "") ;povratak u World UCS

) ;nula

(defun apscisa()
	
	;funkcija za ispis apscise

	(if (< (abs o) 0.001)  ;ispis apscise bez ordinate
		(progn
		(setq ptt (list a (* -1 h) 0))
		(command "TEXT" ptt h "-90" (strcat "%%u" (rtos a 2 dec)))
		) ;progn

		(progn
		(if (< o 0)
			(progn
			(setq ptt (list a h 0))
			(command "TEXT" "j" "r" ptt h "-90" (rtos a 2 dec))
			) ;progn

			(progn
			(setq ptt (list a (* -1 h) 0))
			(command "TEXT" ptt h "-90" (rtos a 2 dec))
			) ;progn
		) ;if
		) ;progn
	) ;if

) ;apscisa

(defun ordinata()

	;funkcija za crtanje i ispis ordinate
	
	(setvar "CELTSCALE" ( * (/ 1 (GETVAR "ltscale")) (/ m 1000.0)))
	(command "LINE" pt (list a 0 0) "")
	(command "TEXT" (list (+ a (* h 0.75)) (/ o 2) 0) h "-90" (rtos (abs o) 2 dec))

	(setq mj (/ m 1000.0))

	; insertiranje simbola
	(if (> o 0)
		(progn
		(command "INSERT" "oo" (list a 0 0) mj mj 0)
		) ;progn
		(progn
		(command "INSERT" "oo" (list a 0 0) mj mj -90 )
		) ;progn
	) ;if

) ;ordinata

(defun c:OR()

	(setvar "cmdecho" 0) ;onemogucavanje ispisa statusne linije
	(setvar "dimzin" 0)	 ;postavljanje ispisa nule iza decimalnog zareza
	(setq snap (getvar "OSMODE")) ;spremanje ulazne vrijednosti osnapa
	(setort)
  
	;citanje postavki iz set.opi
	(setq opi (open "C:/VB/SET.OPI" "r")) (read-line opi)
	(setq m (read (read-line opi))) ;postavljanje mjerila
	(setq h (read (read-line opi))) ;postavljanje visine teksta
	(setq dec 2) ;postavljanje tocnosti ispisa iza decimalnog zareza

	;definiranje linije snimanja

	(setq pt1 (getpoint "\nPikni 1. toèku linije snimanja:"))
	(setq pt2 (getpoint "\nPikni 2. toèku linije snimanja:"))
	(setq linija (distance pt1 pt2))


	(nula) ;pozivanje funkcije za ispis prve i zadnje apscise

	(setq pt (getpoint "\nPikni detaljnu toèku:"))

	(while (/= pt nil)

		(setvar "OSMODE" 0) ;ukidanje osnapa
		(ucsset) ;pozivanje funkcije za setiranje UCS-a

		;transformacija tocke iz WCS-a u UCS		
		(setq pt (trans pt 0 1))

		(setq a (car pt) o (cadr pt))
		(apscisa) ;pozivanje funkcije za ispis apscise
		(if (> (abs o) 0.001) 
			(ordinata) ;pozivanje funkcije za ispis ordinate
		) ;if

		(command "UCS" "") ;povratak u World UCS
		(setvar "OSMODE" snap) ;vracanje osnapa na pocetnu vrijednost
		(setq pt (getpoint "\nPikni detaljnu toèku:"))

	) ;while

) ;OR


;*********************************** O K O M I C A ***********************************
;*****                                                                           *****
;*****                           lisp za crtanje okomice                         *****
;*****                           by prexy, 22.10.2002.god.                       *****
;*****                                                                           *****
;*************************************************************************************
;
; nova verzija: 29.11.2004. god. - crtanje u 3D i moguænost zadavanja dužine
;

(defun ucsset()
	
	;funkcija za setiranje privremenog UCS-a
	(setvar "ucsicon" 0)
	(command "UCS" "o" pt1)
	(command "UCS" "z" pt1 pt2 "")

) ;ucsset

(defun tocke()

	;funkcija za definiranje tocaka pt1 i pt2
  	
	(setq y1 (nth 1 (assoc ' 10 ent)))  ; èitanje koordinate y
	(setq x1 (nth 2 (assoc ' 10 ent)))  ; èitanje koordinate x
    	(setq z1 (nth 3 (assoc ' 10 ent)))  ; èitanje koordinate z
  	(setq y2 (nth 1 (assoc ' 11 ent)))  ; èitanje koordinate y
	(setq x2 (nth 2 (assoc ' 11 ent)))  ; èitanje koordinate x
    	(setq z2 (nth 3 (assoc ' 11 ent)))  ; èitanje koordinate z
	
	(setq pt1 (list y1 x1 z1))
	(setq pt2 (list y2 x2 z2))
	
	(if (< (distance pt0 pt1) (distance pt0 pt2)) (zamjena))
) ;tocke

(defun zamjena()

	; funkcija koja zamjenjuje tocke pt1 i pt2
	(setq pttemp pt1)
	(setq pt1 pt2)
	(setq pt2 pttemp)

) ;zamjena

;------------------------------------------------------------------------------------------
;---------------------------------- glavna funkcija ---------------------------------------
;------------------------------------------------------------------------------------------

(defun c:ok()

	(setvar "cmdecho" 0) ;onemogucavanje ispisa statusne linije
	(setq snap (getvar "OSMODE")) ;spremanje ulazne vrijednosti osnapa

  	(initget "d")
	(setq pt (getpoint "\nPikni toèku <Dužina>:"))
	(if (= pt "d")
	  (setq d(getreal "\nZadaj dužinu:"))
	);if
	(while (/= pt nil)

		(setvar "OSMODE" 0) ;ukidanje osnapa
	  	(setq izbor(entsel "\nIzaberi liniju:")) ;selektiranje linije
	  	(setq pt0 (car(cdr izbor)))
		(setq ent (entget (car izbor)))

		(tocke) ;pozivanje funkcije za definiranje tocaka na liniji
		(ucsset) ;pozivanje funkcije za setiranje UCS-a
		(if (= d 0)
	  		(setq pt (trans pt 0 1)) ;transformacija koordinata toèke iz WCS u UCS
		);if
		(setq pt2 (trans pt2 0 1))

		(setq y (nth 0 pt2))
		(if (= d 0)
	  		(setq x (nth 1 pt))
		  	(setq x d)
		);if
		(setq z (nth 2 pt2))
		(setq pt3 (list y x z))

	  	(command "line" pt2 pt3 "")
		(command "UCS" "") ;povratak u World UCS
		(setvar "ucsicon" 1)
		(setvar "OSMODE" snap) ;vracanje osnapa na pocetnu vrijednost
		(setq d 0)
	  	(initget "d u")
		(setq pt (getpoint "\nPikni toèku <Dužina/Undo>:"))
		(if (= pt "d")
	  		(setq d(getreal "\nZadaj dužinu:"))
		);if
	  	(if (= pt "u")
		  (progn (entdel (entlast))(c:ok))
		);if
	) ;while

) ;ok




;************************** Crtanje poprecnih profila**************************
;****                                                                      ****
;****     	              by Prexy, 20.05.1998.                        ****
;****                   VERZIJA 1.0 dovrsena 06.11.1999.                   ****
;****                                                                      ****
;******************************************************************************

(defun c:pp ()
	
	(setvar "cmdecho" 0)
	(setvar "dimzin" 0)
	(command "osnap" "non")
	(command "style" "standard" "" 0 1 0 "N" "N" "N")
	(command "layer" "m" "profili" "s" "" "")
	
	(setq opi (open "c:/vb/set.opi" "r")) 
	(read-line opi)
	(setq m (read (read-line opi)))
	(setq h (read (read-line opi)))
	(setq mv (read (read-line opi)))
	(close opi)
	
	(setq d (* h 5))
	
	(setq n (getint "\nBroj profila <1>:"))
	(if (= n nil) (setq n 1))

	(while (/= n 0)

		(setq stac (getstring "\nStacionaža:"))
		(setq vis (getreal "\nVisina poèetne toèke:"))
		(setq pt0 (getpoint "\nPoèetna toèka profila:"))
		(setq p 0)
		(setq ptt (list (nth 0 pt0) (+ (nth 1 pt0) (* 2 d)) 0))
		(command "text" ptt h "270" (rtos vis 2 2))

		(setq dy (getreal "\nUdaljenost:"))

		(while (/= dy nil)

			(setq dx (getreal "\nVisina:"))
			(setq dx (- dx vis))
			(setq pt (list (+ (nth 0 pt0) dy) (+ (nth 1 pt0) (* dx (/ m mv))) 0))
			(if (= p 0) (command "line" pt0 pt "") (command "line" "" pt ""))
			(setq p 1)
			(setq ptt1 (list (nth 0 pt) (nth 1 ptt) 0))
			(command "text" ptt1 h "270" (rtos (+ vis dx) 2 2))
			(setvar "lastpoint" pt)
			(setq dy (getreal "\nUdaljenost:"))
		
		)

		(setq pts (list (/ (+ (nth 0 pt0) (nth 0 pt)) 2) (+ (nth 1 ptt) (* 2 d)) 0))
		(setq ptn (list (nth 0 pts) (+ (nth 1 pts) (* h 3)) 0))
		(command "text" "j" "c" pts (* h 1.5) "0" stac)
		(command "text" "j" "c" ptn (* 2 h) "0" (strcat "Profil " (rtos n 2 0)))

		(setq nn (+ n 1))
		(setq n (getint (strcat "\nBroj profila <" (rtos nn 2 0) ">:")))
		(if (= n nil) (setq n nn))

	)

	(setvar "cmdecho" 1)
)


;***********************************   P  I  K  A   **********************************
;*****                                                                           *****
;*****                             lisp za crtanje pika                          *****
;*****                           by prexy, 13.01.2012.god.                       *****
;*****                                                                           *****
;*************************************************************************************


(defun c:pika()
  (setvar "cmdecho" 0)
  (setq opi (open "c:/vb/set.opi" "r")) (read-line opi) (setq mjerilo (read-line opi))
  (prompt (strcat "Mjerilo je 1:" mjerilo))
  (setq pnt (getpoint "\n**Izaberi mjesto za piku:"))
  ;(setq fr (cdr (assoc 70 (dictsearch (namedobjdict) "ACAD_WIPEOUT_VARS"))))
  ;(if (= fr 1) (command "wipeout" "f" "off"))
  (command "wipeout" "f" "off")
  (setq mj (/ (read mjerilo) 1000.0))
  (while (/= pnt nil)
	(command "_insert" "s_medja_neob.dwg" pnt mj mj 0)
	(setq pnt (getpoint "\n**Izaberi mjesto za piku:"))
  );while
)