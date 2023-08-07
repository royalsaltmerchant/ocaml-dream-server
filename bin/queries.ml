open Lwt.Syntax

module type DB = Caqti_lwt.CONNECTION
module T = Caqti_type
let get_todos_query = 
  let query = 
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup3 int string bool))
    "SELECT id, description, done FROM todos" in
    fun (module Db : DB) ->
      let* todos_or_error = Db.collect_list query () in
      Caqti_lwt.or_fail todos_or_error