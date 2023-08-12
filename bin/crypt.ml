open Bcrypt

let () =
  (* Hash a password *)
  let hashed_password = hash "pass" in

  (* Print the hashed password *)
  print_endline ("Hashed password: " ^ string_of_hash hashed_password);

  (* Check a password against its hash *)
  if verify "myPassword" hashed_password then
    print_endline "Password verification succeeded!"
  else
    print_endline "Password verification failed!"
