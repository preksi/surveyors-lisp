;********************************  S T A T I S T I K A  ******************************
;*****                                                                           *****
;*****                        lisp za ispis statistike dwg-a                     *****
;*****                           by prexy, 29.10.2002.god.                       *****
;*****                                                                           *****
;*************************************************************************************


(defun space (prostor duzina)

	;funkcija koja ispisuje "prostor - duzina" space-ova
	(setq i (- prostor duzina))

	(while (/= i 0)
		(princ " " f)
		(setq i (- i 1))
	);while

);space

(defun tablica (ime)

	;funkcija koja racuna broj elemenata u tablici
	(setq tab (tblnext ime T))
	(setq broj 0)
	(while tab
		(setq broj (+ broj 1))
		(setq tab (tblnext ime))
	);while
	;(if (= ime "block") (setq broj (- broj 1)))
	(princ (itoa broj))

);tablica

(defun opca_stat()

	(princ (strcat "\n Ime datoteke:       " (getvar "DWGPREFIX") (getvar "DWGNAME")) f)
	(setq objekti (ssget "X"))
	(princ (strcat "\n Broj objekata:      " (itoa (sslength objekti))) f)
	(princ (strcat "\n Broj layera:        " (tablica "layer")) f)	
	(princ (strcat "\n Broj def. blokova:  " (tablica "block")) f)

);opca_stat

(defun lejer()

	(setq n 0)
	(princ "\n Red.br  Ime layera                      Boja    Linija" f)
	(princ "\n---------------------------------------------------------" f)
	(setq lay (tblnext "layer" T))
	(while lay
		(setq n (+ n 1))
		(princ "\n   " f)
		(princ (itoa n) f)
		(space 6 (strlen (itoa n)))(princ (cdr (assoc '2 lay)) f)
		(space 33 (strlen (cdr (assoc '2 lay))))(princ (abs (cdr (assoc '62 lay))) f)
		(space 5 (strlen (itoa (abs (cdr (assoc '62 lay))))))(princ (cdr (assoc '6 lay)) f)
		(setq lay (tblnext "layer"))
	);while

)

(defun c:stt ()
	
	(setvar "cmdecho" 0)
  	(setq imeDat (strcat (getvar "dwgprefix") (getvar "dwgname") "_stat.txt"))
  	(setq dat imeDat)
	(setq f (open dat "w"))
	(opca_stat) ;opca statistika
  	(princ "\n" f)(princ "\n" f)
	(lejer) ;statistika layera
	(close f)

  	(initget "m d")
  	(setq opcija (getkword "\nIspis na Monitor/Datoteku <Monitor>:"))
  	(if (= opcija "d")
	  (command "shell" (strcat "notepad " dat)) ;ispis u datoteku
	  (progn
		;ispis na monitor
		(setq ff (open imeDat "r"))
		(setq linija T)
		(while linija
			(princ "\n")
			(princ (setq linija (read-line ff)))
		);while
		(close ff)
		(command "shell" (strcat "del " imeDat)) ;brisanje datoteke
		(textpage)
		(setvar "cmdecho" 1)
		(princ)
	  )
	)
)


;************************  L  A  Y  E  R      C  H  A  N  G  E  **********************
;*****                                                                           *****
;*****     Lisp za promjenu Layer-a (broja, toèke i visine) u željeni layer      *****
;*****                                   by DARKO                                *****
;*****                                                                           *****
;*************************************************************************************

(defun C:ll()
(progn
      (setq elem1 (ssget)
            brelem (sslength elem1)
            brojac (1- brelem)
		new (getstring "Novi layer:")
		lnew (tblsearch "layer" new)
		tip (cdr (cadr lnew))
		tip2 (substr tip 3)
      );setq
);progn	
(setq a 0)
(while (<= a brojac)
	 (setq name (ssname elem1 a))
	 (setq nname (entget name))
	 (setq code (cdr(assoc 8 nname)))
	 (setq code1 (substr code 1 2))
	 (setq code1 (strcat code1 tip2))
	 (setq codde (assoc 8 nname))
	 (setq code1 (cons 8 code1))
	 (setq nname(subst code1 codde nname))
	 (entmod nname)
	 (setq a (1+ a))
);while
(princ)
);end ll



;*************************************************************************************
;*****************  Lisp za postavljanje objekata na zadanu visinu   *****************
;*****************            ili promjenu visine za zadani pomak    *****************
;*****************                  verzija 1.1                      *****************
;*****************		 by prexy, 22.05.2002.               *****************
;*************************************************************************************


; Funkcije za postavljanje pojedinih vrsta objekata na visinu z

;----------------------------------2D poli-------------------------------------------------
(defun 2dpoli0()
  ; funkcija za postavljanje 2d polilinije na visinu z

  	(if (= tip "Relativni")
	  (progn
  	    (setq h (cdr (assoc ' 38 (entget ent))))  ; èitanje visine objekta
	    (setq visina (+ h pomak))
	  ); progn
	); if
  	
  	(setq ent
		; zamjena visine 2d polilinije
		(subst (cons 38 visina) (assoc ' 38 (entget ent)) (entget ent))
	)
	; promjena visine objekta
	(entmod ent)
)

;--------------------------------polilinija-------------------------------------------------
(defun poli0()
  ; funkcija za postavljanje 3d polilinije na visinu z

	(setq entpoli ent) ; èuvanje varijable polyline entity
	(setq ent (entnext entpoli))
	(setq podvrsta (cdr (assoc 0 (entget ent)))) ; èitanje vrste objekta

	(while (= podvrsta "VERTEX") 
		(tocka0) ; postavljanje èvorova polilinije na visinu
		(setq ent (entnext (cdr (assoc -1 ent)))) ; pomicanje na iduæi èvor
		(setq podvrsta (cdr (assoc 0 (entget ent)))) ; èitanje vrste objekta
	); while

	; update prikaza polilinije
	(entupd entpoli)
)

;----------------------------------linija--------------------------------------------------
(defun linija0()
  ; funkcija za postavljanje linije na visinu z

	(tocka0) ; pomak prve toèke linije svodi se na pomak toèke

  	(if (= tip "Relativni")
	  (progn
  	    (setq h (nth 3 (assoc ' 11 ent)))  ; èitanje visine objekta
	    (setq visina (+ h pomak))
	  ); progn
	); if
  	
  	(setq y (nth 1 (assoc ' 11 ent)))  ; èitanje koordinate y
	(setq x (nth 2 (assoc ' 11 ent)))  ; èitanje koordinate x
		
	(setq ent
 		; zamjena visine druge toèke linije
		(subst (list 11 y x visina) (assoc ' 11 ent) ent)
	) 

	; promjena visine objekta
	(entmod ent)

)

;-----------------------------------tocka0--------------------------------------------------
(defun tocka0()
  ; funkcija za postavljanje toèkastih objekata (toèka, tekst, blok) na visinu z
  
  	(if (= tip "Relativni")
	  (progn
  	    (setq h (nth 3 (assoc ' 10 (entget ent))))  ; èitanje visine objekta
	    (setq visina (+ h pomak))
	  ); progn
	); if
  	
  	(setq y (nth 1 (assoc ' 10 (entget ent))))  ; èitanje koordinate y
	(setq x (nth 2 (assoc ' 10 (entget ent))))  ; èitanje koordinate x
		
	(setq ent
 		; zamjena visine toèkastog objekta
		(subst (list 10 y x visina) (assoc ' 10 (entget ent)) (entget ent))
	) 

	; promjena visine objekta
	(entmod ent)

)

;------------------------------------------------------------------------------------------
;---------------------------------- glavna funkcija ---------------------------------------
;------------------------------------------------------------------------------------------
(defun c:vv ()

  	(setvar "cmdecho" 0)
	(command "osnap" "non")
	(setq i 0)
  
	(initget  "Apsolutni Relativni") ; izbor vrste pomaka
	(setq tip (getkword "\nVrsta pomaka (Apsolutni/Relativni) <Apsolutni>:"))
	(if (= tip nil) (setq tip "Apsolutni"))
  	
  	(if (= tip "Apsolutni") (setq visina (getreal "Nova visina:"))
	                        (setq pomak (getreal "Razlika visine:"))
	); if
  	
	(setq objekti (ssget))   ; selektiranje objekata
	
	(repeat (sslength objekti)

		(setq ent (ssname objekti i)) ; dodjeljivanje objekta iz sset-a varijabli
		(setq vrsta (cdr (assoc 0 (entget ent)))) ; èitanje vrste objekta

		; filtriranje objekata
		(if (= vrsta "LWPOLYLINE") (2dpoli0)) 
		(if (= vrsta "POLYLINE") (poli0)) 
		(if (= vrsta "LINE") (linija0)) 
		(if (= vrsta "POINT") (tocka0)) 
		(if (= vrsta "TEXT") (tocka0)) 
		(if (= vrsta "MTEXT") (tocka0)) 
		(if (= vrsta "INSERT") (tocka0)) 
		(if (= vrsta "ARC") (tocka0)) 
		(if (= vrsta "CIRCLE") (tocka0))

	  	(princ (strcat "\r " (rtos (+ i 1) 2 0))) ; ispis broja objekta
		(setq i (+ i 1))
		
	); repeat

  	(setvar "cmdecho" 1)
	(princ)
)
