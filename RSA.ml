let mult a b n = 
  let result = (a mod n * b mod n) mod n in
  if result < 0 then result + n else result
;;

let rec pow a k n =
  if k = 0 then 1 
  else if k = 1 then a mod n
  else
    match k mod 2 with
    | 0 -> 
        let half = pow a (k/2) n in
        mult half half n
    | _ -> 
        let half = pow a (k/2) n in
        mult (mult half half n) a n
;;

let rec bezout a b =
  if b = 0 then (a, 1, 0) else
  let (d, u, v) = bezout b (a mod b) in
  let q = a / b in
  (d, v, u - q*v);;

let inv a zn = 
  let (d, u, _) = bezout a zn in
  if d <> 1 then 
    failwith "a n'est pas inversible modulo zn"
  else
    (u mod zn + zn) mod zn 
;;

let rec pgcd a b = 
  if b = 0 then a else
  pgcd b (a mod b);;

let eulers_phi n =
  let rec eulers_phi_aux n k =
    if k = n then 0 else
    let d = pgcd n k in
    match d with
    | 1 -> 1 + eulers_phi_aux n (k+1)
    | _ -> eulers_phi_aux n (k+1) in
    eulers_phi_aux n 1;;

let is_prime n =
  if n < 2 then false else
  let n_f = float_of_int n in
  let rec check i =
    if i > int_of_float (sqrt n_f) then true
    else if n mod i = 0 then false
    else check (i + 1)
  in
  check 2
    
let () = Random.self_init ();;

let random_number_gen () =
  Random.int 9000 + 10000
;;

let rec prime_gen bound =
  let rndt = random_number_gen () in
  if rndt > bound then prime_gen bound 
  else if is_prime rndt then rndt 
  else prime_gen bound
;;

let prime_couple_generator =
  let bound = 99999 in
  let a = prime_gen bound in
  let rec get_different_prime () =
    let b = prime_gen bound in
    if b = a then get_different_prime () else b
  in
  let b = get_different_prime () in
  (a, b)
;;

let (a, b) = prime_couple_generator;;
let n = a * b;;
let phi_rsa = (a - 1) * (b - 1);;
let e = 65537;;
let d = inv e phi_rsa;;

let char_to_code c =
  let ascii = Char.code c in
  if ascii >= 97 && ascii <= 122 then  
    ascii - 97
  else if ascii >= 65 && ascii <= 90 then  
    ascii - 65 + 26
  else
    failwith "Caractère invalide (seulement a-z et A-Z)"
;;

let code_to_char n =
  if n >= 0 && n <= 25 then
    Char.chr (n + 97)  
  else if n >= 26 && n <= 51 then
    Char.chr (n - 26 + 65)  
  else
    failwith "Code invalide (doit être entre 0 et 51)"
;;

let mot_to_int mot =
  if String.length mot <> 5 then
    failwith "Le mot doit contenir exactement 5 lettres"
  else
    let rec aux i acc =
      if i = 5 then acc
      else
        let code = char_to_code mot.[i] in
        aux (i + 1) (acc * 52 + code)
    in
    aux 0 0
;;


let int_to_mot n =
  let rec aux n i acc =
    if i = 0 then acc
    else
      let code = n mod 52 in
      let c = code_to_char code in
      aux (n / 52) (i - 1) (String.make 1 c ^ acc)
  in
  aux n 5 ""
;;


(* Interface utilisateur pour RSA *)

let afficher_cles () =
  Printf.printf "\n=== Clés RSA générées ===\n";
  Printf.printf "Nombre premier p: %d\n" a;
  Printf.printf "Nombre premier q: %d\n" b;
  Printf.printf "Modulo n = p×q: %d\n" n;
  Printf.printf "φ(n) = (p-1)×(q-1): %d\n" phi_rsa;
  Printf.printf "Clé publique (e, n): (%d, %d)\n" e n;
  Printf.printf "Clé privée (d, n): (%d, %d)\n" d n;
  Printf.printf "========================\n\n"
;;

let coder_mot mot =
  try
    let m = mot_to_int mot in
    let c = pow m e n in
    Printf.printf "Mot '%s' → Code numérique: %d → Chiffré: %d\n" mot m c;
    c
  with Failure msg ->
    Printf.printf "Erreur: %s\n" msg;
    0
;;

let decoder_nombre c =
  try
    let m = pow c d n in
    let mot = int_to_mot m in
    Printf.printf "Chiffré %d → Code numérique: %d → Mot: '%s'\n" c m mot;
    mot
  with Failure msg ->
    Printf.printf "Erreur: %s\n" msg;
    ""
;;

let menu_principal () =
  Printf.printf "\n╔════════════════════════════════════╗\n";
  Printf.printf "║   Système de cryptage RSA          ║\n";
  Printf.printf "╚════════════════════════════════════╝\n";
  Printf.printf "1. Afficher les clés RSA\n";
  Printf.printf "2. Coder un mot (5 lettres)\n";
  Printf.printf "3. Décoder un nombre\n";
  Printf.printf "4. Test complet (codage + décodage)\n";
  Printf.printf "5. Régénérer de nouvelles clés\n";
  Printf.printf "6. Quitter\n";
  Printf.printf "Votre choix: "
;;

let test_complet () =
  Printf.printf "\n=== Test complet ===\n";
  Printf.printf "Entrez un mot de 5 lettres: ";
  let mot = read_line () in
  if String.length mot <> 5 then
    Printf.printf "Erreur: Le mot doit contenir exactement 5 lettres\n"
  else begin
    Printf.printf "\n--- Codage ---\n";
    let c = coder_mot mot in
    Printf.printf "\n--- Décodage ---\n";
    let _ = decoder_nombre c in
    Printf.printf "\n✓ Test réussi!\n"
  end
;;

let rec boucle_principale () =
  menu_principal ();
  try
    let choix = read_int () in
    match choix with
    | 1 -> 
        afficher_cles ();
        boucle_principale ()
    | 2 ->
        Printf.printf "Entrez un mot de 5 lettres: ";
        let mot = read_line () in
        let _ = coder_mot mot in
        boucle_principale ()
    | 3 ->
        Printf.printf "Entrez le nombre à décoder: ";
        let c = read_int () in
        let _ = decoder_nombre c in
        boucle_principale ()
    | 4 ->
        test_complet ();
        boucle_principale ()
    | 5 ->
        Printf.printf "\n=== Génération de nouvelles clés ===\n";
        let (new_a, new_b) = prime_couple_generator in
        Printf.printf "Nouvelles clés générées!\n";
        Printf.printf "Note: Redémarrez le programme pour les utiliser.\n";
        boucle_principale ()
    | 6 ->
        Printf.printf "\nAu revoir!\n"
    | _ ->
        Printf.printf "Choix invalide. Réessayez.\n";
        boucle_principale ()
  with
  | Failure msg ->
      Printf.printf "Erreur: %s\n" msg;
      boucle_principale ()
  | _ ->
      Printf.printf "Erreur de saisie. Réessayez.\n";
      let _ = read_line () in
      boucle_principale ()
;;

let () =
  Printf.printf "\n";
  Printf.printf "╔════════════════════════════════════╗\n";
  Printf.printf "║  Bienvenue dans le système RSA!    ║\n";
  Printf.printf "╚════════════════════════════════════╝\n";
  afficher_cles ();
  boucle_principale ()
;;