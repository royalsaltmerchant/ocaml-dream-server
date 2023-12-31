open Handler

let print_path_middleware inner_handler req =
  let path = Dream.target req in
  Dream.log "REQUEST PATH: %s" path;
  let user = Dream.session_field req "user" in
  (match user with
  | Some username -> Dream.log "LOGGED IN USER: %s" username
  | None -> Dream.log "LOGGED IN USER: %s" "No user");
  inner_handler req

let () =
  print_endline "Server running at http://localhost:8080";
  Dream.run ~error_handler:(Dream.error_template error)
  @@ Dream.logger
  @@ Dream.set_secret (Sys.getenv "OCAML_DREAM_SERVER_SECRET") (* Set environment variable secret *)
  @@ Dream_livereload.inject_script ()
  @@ Dream.sql_pool "postgresql://localhost:5432/postgres"
  @@ Dream.sql_sessions
  @@ print_path_middleware
  @@ Dream.router [
    Dream_livereload.route (); 
    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/login" login;
    Dream.get "/register" register;
    Dream.post "/register" handle_register;
    Dream.post "/login" handle_login;
    Dream.get "/echo/:word" echo;
    Dream.get "/todos" todos;
    Dream.get "/logout" logout;
  ]