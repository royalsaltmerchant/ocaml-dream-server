open Handler

let () =
  print_endline "Server running at http://localhost:8080";
  Dream.run
    @@ Dream.logger
    @@ Dream.sql_pool "postgresql://localhost:5432/postgres"
    @@ Dream.sql_sessions
    @@ Dream.router [
      Dream.get "/static/**" @@ Dream.static "static";
      Dream.get "/"
        (fun _ -> 
          Dream.html "Good morning, yeah hello world okay");
      Dream.get "/echo/:word" echo_handler;
      Dream.get "/todos" todos;
    ]