open Jingoo
open Lwt.Syntax
open Queries

let login req =
  let csrf_token = Dream.csrf_token req in
  let template = Jg_template.from_file "templates/login.jingoo.html" ~models:[("csrf_token", Jg_types.Tstr csrf_token)] in
  Dream.html template

let handle_login req =
  match%lwt Dream.form req with
  | `Ok ["email", email; "password", password] -> (
    print_endline email; print_endline password;
    let%lwt () = Dream.set_session_field req "user" email in
    Dream.redirect req "/todos")
  | _ -> Dream.empty `Bad_Request

let echo req = 
  let param = Dream.param req "word" in
  let template = Jg_template.from_file "templates/echo.jingoo.html" ~models:[("param", Jg_types.Tstr param)] in
  Dream.html template

let todos req =
  let* todos = Dream.sql req get_todos_query in
  let todo_to_object (id, description, is_done) =
    Jg_types.Tobj [
      ("id", Jg_types.Tint id);
      ("description", Jg_types.Tstr description);
      ("done", Jg_types.Tbool is_done);
    ] in
  let template = Jg_template.from_file "templates/todos.jingoo.html" ~models:[("todos", Jg_types.Tlist (List.map todo_to_object todos));] in
  Dream.html template

let error _error _debug_info suggested_response =
  let status = Dream.status suggested_response in
  let code = Dream.status_to_int status
  and reason = Dream.status_to_string status in
  let template = Jg_template.from_file "templates/error.jingoo.html" ~models:[("code", Jg_types.Tint code); ("reason", Jg_types.Tstr reason)] in
  Dream.set_header suggested_response "Content-Type" Dream.text_html;
  Dream.html template