(***********************************************************************)
(* Langages de Programmation: IFT 3000 NRC 51158                       *)
(* TP2 ÉTÉ 2015. Date limite: Mercredi 15 juillet à 17h                *) 
(* Implanter un système permettant de chercher des événements          *)
(* en utilisant les données ouvertes de la ville de Québec             *)
(***********************************************************************)
(*                                                                     *)
(* NOM: ___________________________ PRÉNOM:___________________________ *) 
(* MATRICULE: _____________________ PROGRAMME: _______________________ *)
(*                                                                     *)
(***********************************************************************)
(*                                                                     *)
(* NOM: ___________________________ PRÉNOM:___________________________ *) 
(* MATRICULE: _____________________ PROGRAMME: _______________________ *)
(*                                                                     *)
(***********************************************************************)

#load "unix.cma";; (* Charger le module unix *)
#load "str.cma";;  (* Charger le module Str  *)
#directory "+labltk";; 
#load "labltk.cma";;  (* Charger le module labltk  *)

(* Charger la signature du système d'activités *)
#use "TP2-SIG-E2015.mli";;

(********************************************************************) 
(* Implantation du système en utilisant                             *)
(* la programmation orientée objet                       	    *) 
(********************************************************************)

module Tp2e15 : TP2E15 = struct

  open List
  open Str
  open Tk  

  (* Fonctions manipulant les listes et/ou les chaînes de caractères *)

  (* appartient : 'a -> 'a list -> bool                   *)
  (* Retourner si un élément existe ou non dans une liste *)

  let appartient e l = exists (fun x -> x = e) l

  (* enlever : 'a -> 'a list -> 'a list *)
  (* Enlever un élément dans une liste  *)

  let enlever e l = 
    let (l1, l2) = partition (fun x -> x = e) l
    in l2

  (* remplacer : 'a -> 'a -> 'a list -> 'a list       *)
  (* Remplacer un élément par un autre dans une liste *)

  let remplacer e e' l =
    map (fun x -> (if (x = e) then e' else x)) l 

  (* uniques : string list -> string list                         *)
  (* Retourner une liste ne contenant que des éléments uniques    *) 
  (* Les chaînes vides sont également enlevées de la liste        *)
  (* ainsi que les espaces inutiles avant et/ou après les chaînes *)

  let uniques liste =
    let ltrim = map (fun ch -> String.trim ch) liste in
    let res = ref [] in
    let rec fct l = match l with
     | [] -> !res
     | x::xs -> if (not (mem x !res) && (x <> "")) then res := (!res)@[x]; fct xs
    in fct ltrim

  (* decouper_chaine : string -> string -> string list                          *)
  (* Retourner une liste en découpant une chaîne selon un séparateur (p.ex "|") *)

  let decouper_chaine chaine separateur = split (regexp separateur) chaine

  (* formater_chaine : string list -> string                                  *)
  (* Construire une chaîne selon un certain formatage pour les besoins du TP  *)

  let formater_chaine liste = 
    let res = ref "" in
    let n = (length liste) - 1  in
      for i = 0 to n do
	res := !res ^ ((string_of_int i) ^ " - " ^ (nth liste i) ^ "\n")
      done;
      res := !res ^ ((string_of_int (n+1)) ^ " - Tous \n"); !res

   (* lire_fichier : in_channel -> string -> string list list                     *)
   (* Lire un fichier CSV et retourne une lite de listes de chaînes de caractères *)
   (* en spécifiant le séparateur qu'il faut utiliser pour délimiter les chaînes  *)

   let rec lire_fichier (flux:in_channel) (separateur:string) =
     let read_line ic =
       try
  	input_line ic (* Lire la ligne sans le retour de chariot *)
       with End_of_file -> "" 
     in
       let ligne = read_line flux in
       match ligne with
	 | "" -> []
	 | s -> (decouper_chaine s separateur)::(lire_fichier flux separateur)

  (* retourner_epoque_secondes : string -> string -> string -> string -> float       *)
  (* Retourne le nombre de secondes depuis l'année 1970 jusqu'à une date en prenant  *)
  (* comme heure par défaut minuit (0:00:00).                                        *)
  (* Exemple: let ep = retourner_epoque_secondes "2015-06-23" "-";;                  *)
  (* val ep : float = 1435032000.                                                    *)

  let retourner_epoque_secondes (date:string) (sdate: string) =
    let d = decouper_chaine date sdate in
    let yyyy = int_of_string (nth d 0) and mm = int_of_string (nth d 1) and dd = int_of_string (nth d 2) in
    let eg = {Unix.tm_sec = 0; tm_min = 0; tm_hour = 0; tm_mday = dd; tm_mon = mm-1;
	      tm_year = yyyy-1900; tm_wday = 0; tm_yday = 0; tm_isdst = false} in fst(Unix.mktime eg)

  (* Classes du TP *)

  class evenement (lch:string list) = 
    object(self)
      val categorie_evenement : string = nth lch 0
      val titre_evenement : string = nth lch 1
      val debut_evenement : string = nth lch 2
      val fin_evenement : string = nth lch 3
      val horaire_evenement : string = nth lch 4
      val cout_evenement : string = nth lch 5
      val description_evenement : string = nth lch 6
      val renseignement_evenement : string = nth lch 7
      val tel1_evenement : string = nth lch 8
      val tel2_evenement : string = nth lch 9
      val courriel_evenement : string = nth lch 10
      val url_evenement : string = nth lch 11
      val nomlieu_evenement : string = nth lch 12
      val complement_lieu_evenement : string = nth lch 13
      val adresse_evenement : string = nth lch 14
      val tel_lieu : string = nth lch 15
      val nom_arrondissement : string = nth lch 16

      method get_categorie_evenement = categorie_evenement
      method get_titre_evenement = titre_evenement
      method get_debut_evenement = debut_evenement
      method get_fin_evenement = fin_evenement
      method get_horaire_evenement = horaire_evenement
      method get_cout_evenement = cout_evenement
      method get_description_evenement = description_evenement
      method get_renseignement_evenement = renseignement_evenement
      method get_tel1_evenement = tel1_evenement
      method get_tel2_evenement = tel2_evenement
      method get_courriel_evenement = courriel_evenement
      method get_url_evenement = url_evenement
      method get_nomlieu_evenement = nomlieu_evenement
      method get_complement_lieu_evenement = complement_lieu_evenement
      method get_adresse_evenement = adresse_evenement
      method get_tel_lieu = tel_lieu
      method get_nom_arrondissement = nom_arrondissement

      (* Méthode à implanter *)
      
      (* afficher_evenement : unit *)
      method afficher_evenement = 
     print_string ("Titre: " ^ titre_evenement ^ ".\n");
     print_string ("Categorie: " ^ categorie_evenement ^ ".\n");
     print_string ("Lieu: " ^ nomlieu_evenement ^ ".\n");
     print_string ("Adresse: " ^ adresse_evenement ^ ".\n");
     print_string ("Arrondissement: " ^ nom_arrondissement ^ ".\n");
     print_string ("Telephone: " ^ tel1_evenement ^ ".\n");
     print_string ("Dates: " ^ debut_evenement ^ " au " ^ fin_evenement ^ ".\n");
     print_string ("Horaire: " ^ horaire_evenement ^ ".\n");
     let c = int_of_string cout_evenement in
     let ch = if c = 0 then "Gratuit" else if c = 1 then "Cout a l'entree" else if c = 2 then "Autre" else "Pas affiche" in
     print_string ("Cout: " ^ ch ^ ".\n")

    end

  class sysevenements (od:string) =
    object
	val origine_donnees : string = od 
	method get_origine_donnees = origine_donnees
    end


  class syseve_quebec (od:string) (vc:string) =
    object(self)
      inherit sysevenements od as parent
      val ville_concernee : string = vc
      val mutable liste_evenements : evenement list = []
      method get_ville_concernee = ville_concernee
      method get_liste_evenements = liste_evenements
      method set_liste_evenements (le:evenement list) = liste_evenements <- le
      method evenement_existe (e:evenement) = appartient e liste_evenements
      method retourner_nbr_evenements = length liste_evenements

      (* Méthodes à implanter *)
      
      (* ajouter_evenement : evenement -> unit *)
      method ajouter_evenement (e:evenement) = liste_evenements <- liste_evenements@[e]

      (* supprimer_evenement : evenement -> unit *)
      method supprimer_evenement (e:evenement) = 
	if ((length liste_evenements) = 0) then raise (Failure "Le systeme d'evenements est vide")
	    else (if self#evenement_existe e = false
	    then raise (Failure "Le systeme d'evenements ne contient pas cet evenement")
	    else enlever e self#get_liste_evenements)

      (* afficher_systeme_evenements : unit *)
      method afficher_systeme_evenements = iter (fun e -> e#afficher_evenement; print_newline()) liste_evenements

      (* ajouter_liste_evenements : string list list -> unit *)
      method ajouter_liste_evenements (lle:string list list) = (iter (fun le -> self#ajouter_evenement (new evenement le)) lle)

      (* charger_donnees_sysevenements : string -> unit *)
      method charger_donnees_sysevenements (fichier:string) =
	let ic = open_in fichier in
	let _ = input_line ic in (* ignorer la première ligne *)
	let liste_lignes = lire_fichier ic "|" in
	close_in ic; self#ajouter_liste_evenements liste_lignes

      (* trouver_selon_arrondissement : string -> evenement list *)
      method trouver_selon_arrondissement (na:string) =
	if (self#retourner_nbr_evenements = 0) then raise (Failure "Le systeme d'evenements est vide")
	    else filter (fun e -> (e#get_nom_arrondissement = na)) self#get_liste_evenements

      (* trouver_selon_categorie : string -> evenement list *)
      method trouver_selon_categorie (ge:string) = 
	if (self#retourner_nbr_evenements = 0) then raise (Failure "Le systeme d'evenements est vide")
	    else filter (fun e -> (e#get_categorie_evenement = ge)) self#get_liste_evenements

      (* lister_arrondissements : string list *)
      method lister_arrondissements =
	if (self#retourner_nbr_evenements = 0) then raise (Failure "Le systeme d'evenements est vide")
	    else uniques (map (fun e -> e#get_nom_arrondissement) self#get_liste_evenements)

      (* lister_categories_evenements : string list *)
      method lister_categories_evenements =
	if (self#retourner_nbr_evenements = 0 ) then raise (Failure "Le systeme d'evenements est vide")
	    else uniques (map (fun e -> e#get_categorie_evenement) self#get_liste_evenements)

      (* trier_evenements : int -> unit *)
      method trier_evenements (ordre:int) =
      
      initializer print_string ("Recherche dans " ^ (parent#get_origine_donnees) ^ 
				" de " ^ (self#get_ville_concernee) ^ ".");
				print_newline()


    end

  class app_sysevenements (nf:string) (i:bool) =
    object(self)
      val nom_fichier = nf
      val interface = i

       (* Méthodes à implanter *)
      
      (* sauvegarder_liste_evenements : evenement list -> out_channel -> unit *)      
      method sauvegarder_liste_evenements (le:evenement list) (flux:out_channel) =
	match le with
        | [] -> raise (Failure "Le systeme d'evenements est vide")
	| _::_ ->
	let afficheEv e = 
	  output_string flux ("Titre: " ^ e#get_titre_evenement ^ ".\n");
          output_string flux ("Categorie: " ^e#get_categorie_evenement ^ ".\n");
	  output_string flux ("Lieu: " ^ e#get_nomlieu_evenement ^ ".\n");       
          output_string flux ("Arrondissements: " ^ e#get_nom_arrondissement ^ ".\n");
	  output_string flux ("Telephone: " ^ e#get_tel1_evenement ^ ".\n");
	  output_string flux ("Dates: " ^ e#get_debut_evenement ^ " au " ^ e#get_fin_evenement ^ ".\n");
	  output_string flux ("Horaire: " ^ e#get_horaire_evenement ^ ".\n");
	     let c = int_of_string e#get_cout_evenement in
	     let ch = if c = 0 then "Gratuit" else if c = 1 then "Cout a l'entree" else if c = 2 then "Autre" else "Pas affiche" in
	     output_string flux ("Cout: " ^ ch ^ ".\n\n");
           in
        iter afficheEv le
	
      (* lancer_systeme_evenements : unit *)
     (* method lancer_systeme_evenements = *)

      (* lancer_interface_sevenements : unit *)
      method lancer_interface_sevenements =

	let syseve = new syseve_quebec "les donnees ouvertes" "la ville de Quebec" in
	syseve#charger_donnees_sysevenements "EVENEMENT.CSV" ;
	let liste_arrondissements = syseve#lister_arrondissements in
	let liste_categories = syseve#lister_categories_evenements in 
        let nArrond = ref 0 in
        let nCat = ref 0 in

	(* À compléter *)
	let top = openTk () in
	Wm.title_set top "Système d'événements";
	Wm.geometry_set top "370x580";
	let l1 = Label.create ~text:"Bienvenue a l'outil de recherche d'événements" top in
	let _ = Wm.title_set top "Listbox 1" in
        let arrondissements = Listbox.create ~selectmode:`Single top ~height:6 in
	let _ = Listbox.insert
	    ~index:`End
	    ~texts:(liste_arrondissements)
	    arrondissements in
	let categories = Listbox.create ~selectmode:`Single top in
	let _ = Listbox.insert
	     ~index:`End
	    ~texts:(liste_categories)
	    categories in

	let textArrond = Textvariable.create () in
	Textvariable.set textArrond " Arrondissement selectionné :";
	let a = Label.create ~textvariable:textArrond top in
	
	let textCateg = Textvariable.create () in
	Textvariable.set textCateg " Categorie selectionnée :";
	let c = Label.create ~textvariable:textCateg top in

	let v = Textvariable.create () in
        Textvariable.set v " ? " ;
	let l = Label.create ~textvariable:v 
	    ~background:(`Color "#FDF1B8")
	    ~foreground:(`Color "#0F056B")
	    top in

	let v2 = Textvariable.create() in
	Textvariable.set v2 " ? ";
	let k = Label.create ~textvariable:v2
	    ~background:(`Color "#FDF1B8")
	    ~foreground:(`Color "#0F056B")
	    top in

	let b = Button.create ~text:"Afficher l'arrondissement"
	    ~command:(fun () ->
	      try 
		 nArrond := ( match (List.hd (Listbox.curselection arrondissements)) with
		| `Num y -> y 
		| _ -> failwith "pas de sélection" );
	        Textvariable.set v (nth liste_arrondissements !nArrond)
		with _ -> (print_endline "pas de sélection"; flush stdout)
	       )
	    top in

	let b2 = Button.create ~text:"Afficher la catégorie"
	    ~command:(fun () ->
	      try 
		nCat := (match (List.hd (Listbox.curselection categories)) with
		| `Num y -> y 
		| _ -> failwith "pas de sélection");
	      	Textvariable.set v2 (List.nth liste_categories !nCat)
		with _ -> (print_endline "pas de sélection"; flush stdout)
			      )                                                                                              top in

	let makeWindowResults () =

	  let d = Toplevel.create top in
	  begin
	    Wm.title_set d "Résultats de la recherche";
	    Wm.geometry_set d "580x600";
	    let txt = Text.create ~height:58
		d in
	    let listeSelonArrond = (syseve#trouver_selon_arrondissement (nth liste_arrondissements !nArrond))  in
	    let listeFiltre = filter (fun e -> e#get_categorie_evenement = (nth liste_categories !nCat)) listeSelonArrond in 

	    let f = open_out "~results.txt" in
	    self#sauvegarder_liste_evenements listeFiltre f;
	    close_out f;
	    let r = open_in "~results.txt" in

	    let rec lire (flux:in_channel) =
	    let read_line ic =
	    try
	       input_line ic 
	    with End_of_file -> "EOF" 
	    in
	    let ligne = read_line flux in
	    match ligne with
	    | "" -> "\n" ^ lire flux
	    | "EOF" -> ""
	    | s ->  s ^ "\n" ^ (lire flux) in

	    let results = lire r in
	    let nbTrouves = "Nombre d'évènements trouvés :" ^ (string_of_int (List.length listeFiltre)) ^"\n\n" in

	    Text.insert (`End,[]) nbTrouves txt;
	    Text.insert (`End,[]) results txt;
	    close_in r;
	    pack [txt] ~expand:true;

	  end in

	let b3 = Button.create ~text:"Afficher les résultats"
	    ~command:(fun () ->  makeWindowResults () )
	    top in

	pack [l1];
	pack [arrondissements] ~fill:`X ~expand:false;           
	pack [categories] ~fill:`X ~ipady:80;  
	pack [a];
	pack [l];
	pack [c];
 	pack [k]; 
        pack [b] ~side:`Left;
	pack [b2] ~side:`Left;
	pack [b3] ~side:`Left;
	
	let _ = Printexc.print mainLoop () in
	print_endline "Merci et au revoir!";;


      initializer if interface then self#lancer_interface_sevenements else self#lancer_systeme_evenements

    end

end
