open Jingoo
open Lwt.Syntax
open Queries
open Bcrypt

let is_valid_input input =
  let regex = Str.regexp "^[a-zA-Z0-9]*$" in
  Str.string_match regex input 0

let login req =
  let csrf_token = Dream.csrf_token req in
  let template = Jg_template.from_file "templates/login.jingoo.html" ~models:[("csrf_token", Jg_types.Tstr csrf_token)] in
  Dream.html template
  
let register req =
  let csrf_token = Dream.csrf_token req in
  let template = Jg_template.from_file "templates/register.jingoo.html" ~models:[("csrf_token", Jg_types.Tstr csrf_token)] in
  Dream.html template
    
let handle_register req =
  match%lwt Dream.form req with
  | `Ok ["email", input_email; "password", input_password; "username", input_username] -> (
    if is_valid_input input_username = false then Dream.respond "Invalid username input. Can't contain special characters."
    else
    let hashed_password = hash input_password in 
    try%lwt
      match%lwt Dream.sql req @@ register_user input_email (string_of_hash hashed_password) input_username with 
      | [] -> Dream.respond "Something went wrong..."
      | ((id) :: _) -> (let%lwt () = Dream.set_session_field req "user" (string_of_int id) in
        let res = Dream.response "Success" in 
        Dream.set_header res "HX-Redirect" "/todos";
        Lwt.return res)
    with
    | exc -> let msg = Printexc.to_string exc in Dream.log "%s" msg; Dream.respond msg)
  | _ -> Dream.respond "Something went wrong..."


let handle_login req =
  match%lwt Dream.form req with
  | `Ok ["email", input_email; "password", input_password] -> (
    let escaped_email = Dream.html_escape input_email in
    try%lwt
      match%lwt Dream.sql req (get_user_by_email_query escaped_email) with
      | [] -> Dream.respond "Can't find user by this email address."
      | ((id, _email, password) :: _) -> (
        if verify input_password (hash_of_string password) then
          (let%lwt () = Dream.set_session_field req "user" (string_of_int id) in
          let res = Dream.response "Success" in 
          Dream.set_header res "HX-Redirect" "/todos";
          Lwt.return res)
        else Dream.respond "Incorrect password.")
    with
    | exc -> let msg = Printexc.to_string exc in Dream.log "%s" msg; Dream.respond msg)
  | _ -> Dream.respond "Something went wrong..."

let echo req = 
  let param = Dream.param req "word" in
  let template = Jg_template.from_file "templates/echo.jingoo.html" ~models:[("param", Jg_types.Tstr param)] in
  Dream.html template

let todos req =
  match Dream.session_field req "user" with
  | None -> Dream.empty `Forbidden
  | Some _user -> (
    let* todos = Dream.sql req get_todos_query in
    let todo_to_object (id, description, is_done) =
      Jg_types.Tobj [
        ("id", Jg_types.Tint id);
        ("description", Jg_types.Tstr description);
        ("done", Jg_types.Tbool is_done);
      ] in
    let template = Jg_template.from_file "templates/todos.jingoo.html" ~models:[("todos", Jg_types.Tlist (List.map todo_to_object todos));] in
    Dream.html template
  )

let logout req =
  let _ = Dream.invalidate_session req in
  Dream.redirect req "/login"

let error _error _debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in
  let template = Jg_template.from_file "templates/error.jingoo.html" ~models:[("code", Jg_types.Tint code); ("reason", Jg_types.Tstr reason)] in
  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.html template