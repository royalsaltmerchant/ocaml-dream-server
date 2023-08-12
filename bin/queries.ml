open Lwt.Syntax

module type DB = Caqti_lwt.CONNECTION
module T = Caqti_type
let get_todos_query = 
  let query = 
    let open Caqti_request.Infix in
    (T.unit ->* T.(tup3 int string bool))
    "SELECT id, description, done FROM public.\"Todo\"" in
    fun (module Db : DB) ->
      let* todos_or_error = Db.collect_list query () in
      Caqti_lwt.or_fail todos_or_error

let get_user_by_email_query email =
  let query =
    let open Caqti_request.Infix in
    (T.string ->* T.(tup3 int string string))
    "SELECT * FROM public.\"User\" WHERE email = $1" in
    fun (module Db : DB) ->
      let* users_or_error = Db.collect_list query (email) in
      Caqti_lwt.or_fail users_or_error