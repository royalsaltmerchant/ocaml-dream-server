open Jingoo
open Lwt.Syntax
open Queries

let echo_handler req = 
  let param = Dream.param req "word" in
  let template = Jg_template.from_file "templates/index.jingoo.html" ~models:[("param", Jg_types.Tstr param)] in
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