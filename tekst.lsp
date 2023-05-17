;************************************ F R O N T **************************************
;*****                                                                           *****
;*****                      lisp za ispisivanje frontova                         *****
;*****                       by prexy, 10.07.2001.god.                           *****
;*****                   zadnja promjena by prexy 14.01.2012.                    *****
;*************************************************************************************

(defun setfr (/ lay stl)

	(setq lay "0_front")
	(if (tblsearch "layer" lay)
		(command "LAYER" "T" lay "U" lay "ON" lay "S" lay "")
		(command "LAYER" "m" lay "c" "white" "" "s" "" "") ;stvaranje layera
	);if
  
	(setq stl "rec")
  	(if (tblsearch "style" stl)
	  	(setvar "textstyle" "rec")
		(command "STYLE" "rec" "simplex" 0 0.8 0 "N" "N" "N") ;postavljanje text stila
	);if
  
);setfr

(defun c:FR(/ snap stil opi h dec pt1 pt2 pt3 kut duzina)

	(setvar "cmdecho" 0) ;onemogucavanje ispisa statusne linije
	(setvar "dimzin" 0)	 ;postavljanje ispisa nule iza decimalnog zareza
  	(setq stil (getvar "textstyle")) ;spremanje ulazne vrijednosti tekst stila
  	(setfr) ;postavljanje layera i tekst stila
	
	(setq opi (open "C:/VB/SET.OPI" "r")) (read-line opi) (read-line opi)
	(setq h (read (read-line opi))) ;postavljanje visine teksta
	(setq dec 2) ;postavljanje tocnosti ispisa iza decimalnog zareza

	(setq pt1 (getpoint "\nPikni 1. toèku:"))

	(while (/= pt1 nil)

		(setq pt2 (getpoint "\nPikni 2. toèku:"))
		(setq snap (getvar "OSMODE")) ;spremanje ulazne vrijednosti osnapa
		(setq kut (angle pt1 pt2)) ;racunanje kuta u odnosu na os x
		(setq duzina (distance pt1 pt2)) ;racunanje duzine izmedju tocaka

		; setiranje privremenog UCS-a s pravilnom orijentacijom

		(if (and (> kut (/ PI 2)) (< kut (* 3 (/ PI 2))))
			(progn
				(command "UCS" "o" pt2)
				(command "UCS" "z" pt2 pt1 "")
			) ;progn
			(progn
				(command "UCS" "o" pt1)
				(command "UCS" "z" pt1 pt2 "")
			) ;progn
		) ;if

		; ispis fronta na pola duzine s visinom h i odmakom h/2
		
		(setvar "OSMODE" 0) ;ukidanje osnapa
		(setq pt3 (list (/ duzina 2) (* h 0.75) 0)) ;tocka upisa teksta
		(command "TEXT" "j" "c" pt3 h "0" (rtos duzina 2 dec))
		(command "UCS" "") ;povratak u World UCS
		(setvar "OSMODE" snap) ;vracanje osnapa na pocetnu vrijednost

		(setq pt1 (getpoint "\nPikni 1. toèku:"))

	) ;while

  	(setvar "textstyle" stil)
	(setvar "cmdecho" 1) ;omogucavanje ispisa statusne linije
	(princ)

);FR


;**************************************************************
;*************** Rotacija teksta v 1.1 ************************
;*************** by prexy, 05.04.2002. ************************
;**************************************************************

; Opis: tekst se rotira tako da bude okomit na zadanu liniju
; Verzija 1.1: nije bitna poèetna rotacija teksta


(defun C:RTT ()
  (command "UNDO" "be" "")
  (setq snap (getvar "OSMODE")) ;spremanje ulazne vrijednosti osnapa
  (setvar "CMDECHO" 0)
  (setq ss (ssget))
  (print)
  (setq pt1 (getpoint "Pikni prvu toèku linije:")
        pt2 (getpoint "\nPikni drugu toèku linije:"))
  ;(setq rot (+ (/ pi 2) (angle pt1 pt2)))
  (setq rot (angle pt1 pt2))
  (setq c 0)
  (if ss (setq e (ssname ss c)))
  (setvar "OSMODE" 0) ;ukidanje osnapa
  (while e
    (setq f (entget e))

    (if (= (cdr (assoc 0 f)) "TEXT")
	(progn
	    (setq inspt (cdr (assoc 10 f)))
	    (setq e
		; zamjena kuta rotacije objekta
		(subst (cons 50 rot) (assoc ' 50 f) f)
	    )
	    (entmod e)
	);progn
    );if

    (setq c (1+ c))
    (setq e (ssname ss c))
  );while

  (command "UNDO" "e" "")
  (setvar "CMDECHO" 1)
  (setvar "OSMODE" snap)
  (princ)
)


;*******************************     V I S I N E      ********************************
;*****                                                                           *****
;*****                 lisp za promjenu teksta visina toèaka                     *****
;*****                                  by nn                                    *****
;*****                                                                           *****
;*************************************************************************************

(defun C:ZS (/ sl br a b bb c r p)
 (setvar "cmdecho" 0)
 (setvar "dimzin" 0)	;postavljanje ispisa nule iza decimalnog zareza
 (princ "\n")
 (initget "Layer Select")
 (if (= (getkword "Select<Layer>: ") "Select")
     (progn (setq sl (ssget))
	    (setq a (sslength sl)
		  c (1- a))
	    (while (>= c 0)
	      (if (/= (cdr (assoc 0 (entget (ssname sl c)))) "TEXT")
		  (setq sl (ssdel (ssname sl c) sl))
	      )
	      (setq c (1- c))
	    )
     )
     (setq sl (ssget "X" (list (cons '8 (getstring "\nLayer: ")))))
 )
 (setq r (getreal "\nRazlika visina: ")
       p (getint "\nDecimala: ")
       br 0
       b (sslength sl))
 (while (< br b)
	(entmod (subst (cons '1
			 (strcat 
			 (rtos (+
			  (atof (cdr (assoc 1 (entget (ssname sl br)))))
			  r) 2 p) " "))
		       (assoc 1 (entget (ssname sl br)))
		       (entget (ssname sl br))
		)
	)
	(setq br (1+ br))
 )
 (setvar "cmdecho" 1)
 (princ)
);ZS


;******************************* F R O N T E X T *************************************
;*****                                                                           *****
;*****                 lisp za ispisivanje tekstualnih frontova                  *****
;*****                       by prexy, 08.08.2004.god.                           *****
;*****                   zadnja promjena by prexy 14.01.2012.                    *****
;*************************************************************************************
  
(defun frontaj(vrsta / opi h PT1 PT P snap DUZINA)      

	(setq opi (open "C:/VB/SET.OPI" "r")) (read-line opi) (read-line opi)
	(setq h (read (read-line opi))) ;postavljanje visine teksta

      (setq PT1(getpoint "\nPikni 1. tocku !"))

	(while (/= PT1 nil)
	      (setq PT(getpoint "\nPikni 2. tocku !"))
	      (setq snap (getvar "OSMODE")) ;spremanje ulazne vrijednosti osnapa
	      (if (> (car PT1) (car PT))
		  (progn (setq P PT1 PT1 PT PT P)
		  )
	      )
	      
	      (setq DUZINA (distance PT PT1))
	      (if (= vrsta nil)
		 (progn (setq P (polar PT1 (angle PT1 PT) (/ DUZINA 2))
			    P1 (polar P (+ (angle PT1 PT) (/ PI 2)) h)))
		 (progn (setq P1 (polar PT1 (angle PT1 PT) (/ DUZINA 2))))
	      );if
	      (command "osnap" "non")
	      (command "TEXT" "M" P1 h (* (/ 180 pi) (angle PT1 PT)) txt "")
	      (if (= vrsta "inst")
		(acet-textmask-make-wipeout (entlast) (* 0.5 h) )
	      );if
	      (setvar "OSMODE" snap) ;vracanje osnapa na pocetnu vrijednost
	      (setq PT1(getpoint "\nPikni 1. tocku !"))
	);while
  
);frontaj

(defun C:ft (/ txt stil)

  	(setvar "cmdecho" 0)
	(setq txt (getstring (strcat "\Tekst<" (getvar "users1") ">:")))
	(if (= txt "") (setq txt (getvar "users1")) (setvar "users1" txt))
      
  	(setq stil (getvar "textstyle")) ;spremanje ulazne vrijednosti tekst stila
  	(setfr) ;postavljanje layera i tekst stila
	(frontaj nil)
	(setvar "textstyle" stil)

);ft     


;*************************       OZNAKA INSTALACIJE       ****************************
;*****                                                                           *****
;*****     lisp za ispisivanje profila cijevi ili oznake instalacije             *****
;*****                       by prexy, 15.01.2012.god.                           *****
;*****                   zadnja promjena                                         *****
;*************************************************************************************


(defun c:oi (/ txt stil stl)

  	(setvar "cmdecho" 0)
	(setq txt (getstring (strcat "\Tekst<" (getvar "users1") ">:")))
	(if (= txt "") (setq txt (getvar "users1")) (setvar "users1" txt))
	(command "wipeout" "f" "off")
	(setq stil (getvar "textstyle")) ;spremanje ulazne vrijednosti tekst stila
	(setq stl "rec")
  	(if (tblsearch "style" stl)
	  	(setvar "textstyle" "rec")
		(command "STYLE" "rec" "simplex" 0 0.8 0 "N" "N" "N") ;postavljanje text stila
	);if
  	(command "ucsicon" "OFF")
	(frontaj "inst")
  	(command "ucsicon" "ON")
	(setvar "textstyle" stil)
  
);oi

;*****************************************************************
; ----------------- MASK USING WIPEOUT FUNCTION ------------------
;   This function draws a wipeout under text TXT using the
;   offset value OSET
;   - funkcija je preuzeta iz express toolsa - moraju biti
;     instalirani da bi lisp radio!!!!
; ----------------------------------------------------------------
;*****************************************************************
 
(defun acet-textmask-make-wipeout (ENT OSET / TXT TXTLAY TBX WIPOUT TXTYP TXTSTR)
 
  (setq TXT    (entget ENT (list "*"))               ; Get the text's edata
        TXTLAY (cdr (assoc 8 TXT))                   ; Get the layer of the text
        TXTYP  (cdr (assoc 0 TXT))                   ; Text or Mtext
        TXTSTR (cdr (assoc 1 TXT))
  )
 
  (if (/= TXTSTR "")
    (progn
 
      (if (= TXTYP "TEXT")
        (acet-ucs-cmd (list "_object" ENT))              ; set UCS to object
        (acet-ucs-to-object ENT)
      )
 
      (setq TBX (acet-geom-textbox TXT OSET))              ; Get the points around the text
 
      (if TBX
        (progn
          (command "_.pline")                        ; Create bounding pline box
           (while TBX
            (command (car TBX))
            (setq TBX (cdr TBX))
          )
          (command "_c")
 
          (command "_.wipeout" "_Polyline" (entlast) "_yes")  ; create wipeout entity          
 
          (setq WIPOUT (entlast))
 
          (command "_.change" WIPOUT "" "_Prop" "_Layer" TXTLAY "") ; and set its layer
 
          (acet-ucs-cmd (list "_previous"))              ; reset the ucs
 
          (entmake TXT)                              ; recreate text
          (setq TXT (entlast))                       ; such that it's on top
 
          (acet-xdata-set (list WIPOUT "ACET-TEXTMASK"
                          (list
                            (list "TextObject" (cdr (assoc 5 (entget TXT))) 1005)
                            (list "Offset" OSET 1040)
                          )
                        )
          )
 
          (acet-xdata-set (list TXT "ACET-TEXTMASK"
                          (list
                            (list "MaskObject" (cdr (assoc 5 (entget WIPOUT))) 1005)
                            (list "MaskType" "WIPEOUT" 1000)
                            (list "Offset" OSET 1040)
                          )
                        )
          )
 
          (command "_.move" TXT "" "0,0" "0,0")
 
          (acet-group-make-anon (list WIPOUT TXT) "In use by TEXTMASK") ; make the text and wipeout a group
 
          (entdel ENT)                               ; delete original text
 
        )
      ); if TBX
    ); progn then
  ); if not ""
)


;********************************************************************************
;********************************************************************************
;                 Lisp za opis raznih elemenata crteža
;                 by pyton
;********************************************************************************
;********************************************************************************



(defun rad1 ()
  (prompt "ODABERI SEGMENT")
  (setq h0 h)
  (setq	e   (entsel)
	e1  (car e)
	e2  (entget e1)
	ime (cdr (assoc 0 e2))
  )
  (if (= ime "ARC")
    (luk)
  )
  (if (= ime "LINE")
    (duz)
  )
  (if (= ime "POINT")
    (toc)
  )
  (setq nas (getstring "\n<DALJE> ?"))
  (if (= nas "")
    (rad1)
    (command "redraw")
  )
)
;------------------------------ END RAD1 -----------------------------
(defun luk ()
  (setq	rr (cdr (assoc 40 e2))
	r  (rtos rr 2 3)
  )
  (setq	f0 (cdr (assoc 50 e2))
	f1 (cdr (assoc 51 e2))
	c  (cdr (assoc 10 e2))
  )
  (dimarc)
					;(setq f (angtos (- f1 f0) s1 s2))
  (setq	tx1 (strcat "R=" r)
	tx2 (strcat "L=" (rtos da 2 3))
  )
  (setq	a1 (polar c f1 rr)
	a2 (polar c f0 rr)
  )					;(poi) 
  (stre)
  (pol)
  (setq	em  m2
	ha  h
	bee be1
	tx  tx2
  )
  (upis)				;(kord)
)
;------------------------------ END LUK ------------------------------
(defun duz ()
  (setq	a1 (cdr (assoc 10 e2))
	a2 (cdr (assoc 11 e2))
  )
  (setq	l   (rtos (distance a1 a2) 2 3)
	tx1 l
  )
  (pol)
)
;------------------------------ END DUZ -----------------------------
(defun toc ()
  (setq	a1 (cdr (assoc 10 e2))
	a2 (polar a1 0 1)
	h0 h
  )
  (setq	x1 (rtos (car a1) 2 3)
	y1 (rtos (cadr a1) 2 3)
  )
  (setq	tx3 (strcat x x1)
	tx4 (strcat y y1)
	ab  a1
	mu  a1
  )
  (upi)
  (pol1)
  (setq	em  m1
	ha  h
	bee 0
	tx  tx3
  )
  (upis)
  (setq	em m2
	tx tx4
  )
  (upis)
)
;------------------------------ END TOC -----------------------------
(defun pol ()
  (setq mu (getpoint "\nmjesto upisa"))
  (pol1)
  (setq	em  m1
	ha  h
	bee be1
	tx  tx1
  )
  (if (= uv "t")
    (setq ha (/ ha 2))
  )
  (upis)
)
;------------------------------ END POL -------------------------------
(defun pol1 ()
  (setq be (angle a1 a2))
  (setq m1 (polar mu (+ be (/ pi 2)) (* h0 0.5)))
  (setq m2 (polar mu (- be (/ pi 2)) (* h0 1.5)))
  (setq be1 (/ (* 180 be) pi))
  (if (> be1 90)
    (setq be1 (- be1 180))
  )
  (if (> be1 90)
    (setq be1 (- be1 180))
  )
)
;------------------------------- END POL1 -----------------------------
(defun stre ()
  (setq b1 (polar a1 (+ f1 pi) (* 10 vs)))
  (setq b2 (polar a2 (+ f0 pi) (* 10 vs)))
  (setq a21 (polar a1 (+ f1 pi) (* 2 vs)))
  (setq a22 (polar a2 (+ f0 pi) (* 2 vs)))
  (command "dim" "leader" a1 b1 "" "" "exit")
  (repeat 2 (entdel (entlast)))
  (command "line" a21 b1 "")
  (command "dim" "leader" a2 b2 "" "" "exit")
  (repeat 2 (entdel (entlast)))
  (command "line" a22 b2 "")
)
;------------------------------- END STRE -----------------------------
(defun kord ()
  (setq	x1 (rtos (car a1) 2 3)
	y1 (rtos (cadr a1) 2 3)
  )
  (setq	x2 (rtos (car a2) 2 3)
	y2 (rtos (cadr a2) 2 3)
	h0 (/ h 2)
  )
  (setq	tx3 (strcat x x1)
	tx4 (strcat y y1)
	ab  a1
  )					;(upi) 
  (if (/= mu nil)
    (progn
      (pol1)
      (setq em m1
	    ha h0
	    tx tx3
      )
      (upis)
      (setq em m2
	    tx tx4
      )
      (upis)
    )
  )
  (setq	tx3 (strcat x x2)
	tx4 (strcat y y2)
	ab  a2
  )					;(upi) 
  (if (/= mu nil)
    (progn
      (pol1)
      (setq em m1
	    ha h0
	    tx tx3
      )
      (upis)
      (setq em m2
	    tx tx4
      )
      (upis)
    )
  )
)
;-------------------------------- END KORD -----------------------------
(defun upi ()				;(command "polygon" "5" ab "i" "2")
  (setq mu (getpoint "\nmjesto upisa KOORDINATE ")) ;(entdel (entlast))
)
;------------------------------- END UPI ------------------------------
(defun upis ()
  (command "text" "c" em ha bee tx)
)
;------------------------------- END UPIS -----------------------------
(defun poi ()
  (command "point" a1)
  (command "point" a2)
)
;------------------------------ END POI -------------------------------
(defun dimarc ()
  (setq df (- f1 f0))
  (if (< df 0)
    (setq df (+ df 6.283185))
  )
  (setq da (* rr df))
)
;-----------------------------END DIMARC-------------------------------

(defun C:OP ()
  (setvar "cmdecho" 0)
  (setvar "blipmode" 1)
  (command "style" "standard" "" 0 1 0 "N" "N" "N")
  (setvar "dimzin" 0)			;postavljanje ispisa nule iza decimalnog zareza
  (setq w1 (open "c:/vb/set.opi" "r"))
  (read-line w1)
  (setq kw (read (read-line w1)))
  (setq	x "y="
	y "x="
  )
					;(if (= kw 1) (setq x "y=" y "x="))
  (setq	h   (read (read-line w1))
	mis (read (read-line w1))
  )
  (close w1)
  (setq	s1 1
	s2 4
  )
  (if (= mis 1)
    (setq s1 0
	  s2 6
    )
  )
  (setq vs h)
  (setvar "dimasz" (* 2 vs))
  (rad1)
)
;------------------------------ END OPIS -------------------------------

