(* CCI TP IPRG 2 2009   *)


(* M2CCI IPRG  fichier date.ml *)


(* acces sur les quadruplets *)
let f1 (a,_,_,_) = a;;
let f2 (_,b,_,_) = b;;
let f3 (_,_,c,_) = c;;  
let f4 (_,_,_,d) = d;;   



(* fonctions utilitaires sur les dates *) 
(* bissextile int->bool 
   pre : bissextile a : a>=0
   post : rend truee ssi a est bissextile
*)
let bissextile a =
   (a mod 4 =0) & ((a mod 100 <> 0) or (a mod 400 =0)) ;;


(* nombre_de_jours_du_mois : int -> int -> int
    pre : nombre_de_jours_du_mois a m : a >=0 ; 1<= m et m<=12
    post : rend le nombre de jours de mois m (l'annee a)
*)
let nombre_de_jours_du_mois a m =
      if m = 2 then if (bissextile a) then 29 else 28
 else if m=4 or m=6 or m=9 or m=11 then 30
 else (* m=1 or m=3 or m=5 or m=7 or m=8 or m=10 or m=12 *)  31 ;;

(* valid_date int > int> int> int> bool
   pre : neant
   post : (valid_date a m j h) : rend true ssi (a,m,j,h) est une date (valide)
*)

let valid_date a m j h =
	if (a <0) or (a> 3000) then false
else    if  (h<0) or (h>23)    then false
else    (j>=1) &  (j<= (nombre_de_jours_du_mois a m)  ) ;;


(* ===========================  ACCES SUR LES DATES *)

(* constructeur *)
let creer_date a m j h =
   if valid_date a m j h
    then
      (a,m,j,h)
    else
      failwith "creer_date : valeurs incorrectes" ;;             


 (* accesseurs *)
let an d = f1 d;;
let mois d = f2 d;;
let jour d = f3 d;;
let heure d = f4 d;;

(* Comparateurs *)
let D1 = (creer_date 2015 12 17 0);;
let D2 = (creer_date 2015 12 05 12);;

(* avant : date -> date -> bool
   pre : (avant d1 d2) : neant
   post : true ssi d1 precede strictement d2
*)
let avant d1 d2 = let a1=(an d1) and a2=(an d2) and 
                      m1=(mois d1) and m2=(mois d2) and 
					  j1=(jour d1) and j2=(jour d2) and
					  h1=(heure d1) and h2=(heure d2)
					in
						if a1<a2 then true 
						else if a1>a2 then false
						else (* années identiques *)
							if m1<m2 then true 
							else if m1>m2 then false
							else (* mois identiques *)
								if j1<j2 then true
								else if j1>j2 then false
								else (* jours identiques *)
									 if h1<h2 then true 
									 else false;;
(* Test *)
(avant D1 D2);;
						
(* egal : date -> date -> bool
   pre : (egal d1 d2) : neant
   post : vrai ssi d1=d2
*)
let egal d1 d2 = (d1=d2);;
(* Test *)
(egal D1 D2);;
						
(* apres : date -> date -> bool
   pre : (apres d1 d2) : neant
   post : true ssi d1 succede d2
*)
let apres d1 d2 = (not(avant d1 d2) & not(egal d1 d2));;
(*let apres d1 d2 = let a1=(an d1) and a2=(an d2) and 
                      m1=(mois d1) and m2=(mois d2) and 
					  j1=(jour d1) and j2=(jour d2) and
					  h1=(heure d1) and h2=(heure d2)
					in
						if a1>a2 then true 
						else if a1<a2 then false
						else (* années identiques *)
							if m1>m2 then true 
							else if m1<m2 then false
							else (* mois identiques *)
								if j1>j2 then true
								else if j1<j2 then false
								else (* jours identiques *)
									 if h1>h2 then true 
									 else false;;
*)
(* Test *)
(apres D1 D2);;

(* avant_e : date -> date -> bool
   pre : (avant_e d1 d2) : neant
   post : true ssi d1 precede ou égal à d2
*)
let avant_e d1 d2 = ((avant d1 d2) or (egal d1 d2));;
(*2eme solution : let avant_e d1 d2 = not((apres d2 d1));;*)
(* Test *)
(avant_e D1 D2);;

(* apres_e : date -> date -> bool
   pre : (apres_e d1 d2) : neant
   post : true ssi d1 succede ou égal à  d2
*)
let apres_e d1 d2 = ((apres d1 d2 ) or (egal d1 d2));;
(*2eme solution : let apres_e d1 d2 = not((avant d2 d1));;*)
(* Test *)
(apres_e D1 D2);;

(* Constructeus *)
let date1 = (creer_date 2009 12 31 0);;
let date2 = (creer_date 2004 2 28 0);;

(* lendemain : date -> date
   pre : (lendemain d) : d valide
   post : rend la date lendemain de la date d
*)
let lendemain d = let a=(an d) and m=(mois d) and 
                      j=(jour d) and h=(heure d)
				  in (*On teste si le dernier j du dernier m alors on change l'année*)
				      if ((j=31) & (m=12)) then (creer_date (a+1) 1 1 h)
					  (*on teste si c'est le dernier jour des mois qui contiennent 31 jours*)
					  else if j=31 then (creer_date a (m+1) 1 h)
					  (*On teste s'il s'agit du mois Fevrier avec l'année bissextile*)
					  else if ((m=2)& (bissextile a)) then (creer_date a m (j+1) h)
					  else if ((m=2)& (j=28))then (creer_date a (m+1) 1 h)
					  (* Le prochain if conserne que les mois qui contiennent 30 jours *)
					  else if ((j=30) & ((m=2) or (m=4) or (m=6) or (m=7) or (m=9) or (m=11))) then (creer_date a (m+1) 1 h)
					  (* Si on est dans aucun cas spécial, on incremente tout simplement j *)
					  else (creer_date a m (j+1) h);;
					  
(* Test *)				  
(lendemain date1);;
(lendemain date2);;

(* veille : date -> date
   pre : (veille d) : d valide
   post : rend la date veille de la date d
*)
let veille d = let a=(an d) and m=(mois d) and 
                   j=(jour d) and h=(heure d)
               in 
			       if ((j=1) & (m=1)) then (a-1,12,31,h)
				   else if ((m=3)& (j=1) & (bissextile a)) then (a,m-1,29,h)
				   else if ((m=3)& (j=1))then (a,m-1,28,h)
				   else if ((j=1) & ((m=2) or (m=4) or (m=6) or (m=8) or (m=9) or (m=11))) then (a,m-1,31,h)
				   else if ((j=1) & ((m=5) or (m=7) or (m=10) or (m=12))) then (a,m-1,30,h)
				   else (a,m,j-1,h);;
				   
(* Test *)				  
(veille date1);;
(veille date2);;				   


(* age : date -> date -> int 
   pre : (age d1 d2) : d1 et d2 valides
   post : rend l'age d'un individu
*)
let age d1 d2 = if ((f3 d1)>=(f3 d2)) then ((f1 d1)-(f1 d2))
                else ((f1 d1)-(f1 d2)-1);;

(* Test *)
let dcour = (creer_date 2009 09 21 0);;
let dnaiss = (creer_date 1985 1 1 0);;

(age dcour dnaiss);;