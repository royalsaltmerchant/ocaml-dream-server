open Handler

let print_path_middleware inner_handler req =
  let path = Dream.target req in
  print_endline path;
  let user = Dream.session_field req "user" in
  (match user with
  | Some username -> print_endline username
  | None -> print_endline "No user");
  inner_handler req

let () =
  print_endline "Server running at http://localhost:8080";
  Dream.run
  @@ Dream.logger
  @@ Dream_livereload.inject_script ()
  @@ Dream.sql_pool "postgresql://localhost:5432/postgres"
  @@ Dream.sql_sessions
  @@ print_path_middleware
  @@ Dream.router [
    Dream_livereload.route (); 
    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/login" login;
    Dream.post "/login" handle_login;
    Dream.get "/echo/:word" echo_handler;
    Dream.get "/todos" todos;
  ]