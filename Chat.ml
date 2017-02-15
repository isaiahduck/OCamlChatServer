open Core.Std
open Async.Std
open Core_kernel
open Str

exception Quit
exception Unknown_command

type prompt = Connect | Usr | Repl

let stdin : Reader.t = Lazy.force Reader.stdin
let stdout : Writer.t = Lazy.force Writer.stdout
let flush () = after (Core.Std.sec 0.)

(* [msg_ref] acts as a finite state machine classifier, determining
 * the stage in the communication
 *)
let msg_ref = ref Connect
let split_spaces = Str.regexp "[ \t]+"

(* [send_payload payload w] writes [payload] to [w]
 * requires:
 *  - [payload] a string representing the message to be sent
 *  - [w] a Writer.t
 *)
let send_payload payload w = Writer.write_line w payload

(* [send_message username msg w] sends [msg] to [username] on [w]
 * requires:
 *  - [username] a string representing the user with whom we are communicating
 *  - [msg] a string representing the message to be sent
 *  - [w] a Writer.t
 *)
let send_message username msg w =
  send_payload ("S " ^ username ^ " " ^ msg) w

(* [print_help_message ()] prints a meaningful help message *)
let print_help_message () = print_string ("This chat server " ^
  "supports the following commands:\n\t- S <username> <message> " ^
  ": sends <message> to <username>, creating \n\t\ta conversation between " ^
  "current user and <username> if one does \n\t\tnot already exist \n \t- Q : " ^
  "quits the chat server\n\t- L : lists the users" ^
  "\n\t- H : redisplays this help message\n")

(* [display_message ()] prints the appropriate message depening on the value
 * of the msg_ref ref.
 *)
let display_message () =
  match !msg_ref with
  | Connect -> print_string
    ("Enter the IPv4 address of the chat server to which you would like to " ^
     "connect.\nType 'Q' to quit.\n> ");
  | Usr     -> print_string
    ("Please enter a username that contains no spaces and contains between 4 " ^
     "and 20 \ncharacters. Type 'Q' to quit.\n> ");
  | Repl    -> print_string "> "

(* [get_connections w] sends the 'L' command to server to ask for connections *)
let get_connections w = (print_string "Connections:\n"; send_payload "L" w)

(* [connect input] connects to server at [input] and port 3110 returning
 * (Reader.t * Writer.t) Deferred.t
 * requires: [input] a string representing the IPv4 address of the server
 *)
let connect input =
  let t = Tcp.to_host_and_port input 3110 in
  (Tcp.connect t) >>= fun (_,r,w) -> return (r,w)

(* [check_username username w] first checks that [username] is valid (containing
 * no spaces and contains between 4 and 20 characters) before sending [username]
 * to the server through [w] to verify if [username] is unique
 * requires:
 *  - [username] a string representing the username we want to verify
 *  - [w] a Writer.t
 *)
let check_username username w =
  let split = Str.split split_spaces username in
  if not ((List.length split) = 1) then
    failwith "Username cannot have spaces."
  else
    let username_check_size = List.nth_exn split 0 in
    let size = String.length username_check_size in
    if size < 4 || size > 20 then
      failwith "Username must contain between 4 and 20 characters."
    else
      (print_string "Checking with server...\n";
      send_payload ("U " ^ username) w)

(* [do' c w] examines [c] and performs the appropriate server or client
 * commands, or raises errors which are handled in [repl]
 * requires:
 *  - [c] a string representing user input
 *  - [w] a Writer.t
 *)
let do' c w =
  let arg_list = Str.bounded_split split_spaces c 3 in
  let argc = List.length arg_list in
    match argc with
    | 0 -> print_string "> "
    | 1 ->
      let command = Core_string.lowercase (List.nth_exn arg_list 0) in
        begin match command with
        | "q" -> raise Quit
        | "l" -> get_connections w
        | _   -> raise Unknown_command
        end
    | 2 ->
      let command       = Core_string.lowercase (List.nth_exn arg_list 0) in
        begin match command with
        | "q" -> raise Quit
        | _   -> raise Unknown_command
        end
    | 3 ->
      let command       = Core_string.lowercase (List.nth_exn arg_list 0) in
      let to_username   = List.nth_exn arg_list 1 in
      let message       = List.nth_exn arg_list 2 in
        begin match command with
        | "q" -> raise Quit
        | "s" -> send_message to_username message w
        | _   -> raise Unknown_command
        end
    | _ -> raise Unknown_command

(* [read r] reads in [r] and returns a string Deferred.t
 * requires: [r] a Reader.t
 *)
let read r =
  Reader.read_line r >>= fun res ->
  match res with
  | `Eof  -> return ""
  | `Ok s -> return s

(* [read_print_loop r] same as [read] but also prints out the resulting string,
 * recursing back on the writer to capturing multiline strings
 * requires: [r] a Writer.t
 *)
let rec read_print_loop r =
  Reader.read_line r >>= fun res ->
  match res with
  | `Eof -> return ()
  | `Ok s ->
    match s with
    | "~OGMACO~" -> return ()
    | s' -> (printf "%s\n" s'); read_print_loop r

(* [repl r w] returns unit Deferred.t after prompting the user to enter an
 * IPv4 address to connect to a server, enter a valid username, or send messages
 * at the prompt.
 * requires:
 *  - [r] a Reader.t
 *  - [w] a Writer.t
 *)
let rec repl r w : unit Deferred.t =
  display_message ();
  Reader.read_line stdin >>= fun res ->
  match res with
    | `Eof     -> (print_string "done\n"; exit 0)
    | `Ok line ->
      let input = Core_string.lowercase line in
      match !msg_ref with
      | Connect ->
        if input = "q" then exit 0
        else
          let split_periods = Str.regexp "[.]" in
          let octets = Str.split split_periods input in
          if not ((List.length octets) = 4) then
            (print_string "IPv4 address must be separated by 3 periods.\n";
            repl r w)
          else begin
            print_string ("Connecting to server @ " ^ input
              ^ ":3110 ...\n");
            try_with (fun () -> connect line) >>= function
            | Ok (r,w) ->
              (print_string "Connected successfully\n";
              msg_ref := Usr;
              repl r w)
            | Error _  ->
              (print_string "Connection failed. Try again\n";
              repl r w)
          end
      | Usr ->
        if input = "" then repl r w
        else if input = "q" then exit 0
        else begin
          try_with (fun () -> return (check_username input w)) >>= function
          | Ok () ->
            Writer.flushed w >>= fun () ->
            read r >>= fun s ->
              if Core_string.substr_index s ~pattern:"Reprompt" = None then
                (print_string "This is a valid username.\nType 'H' for help.\n";
                msg_ref := Repl;
                repl r w)
              else
                (print_string "This username has already been taken. Try again.\n";
                repl r w)
          | Error _ ->
                (print_string ("Username must contain between 4 and 20 " ^
                "characters. Try again\n");
                repl r w)
        end
      | Repl ->
        if input = "" then repl r w
        else if input = "q" then exit 0
        else if input = "h" then (print_help_message (); repl r w)
        else
          try_with (fun () -> return (do' input w)) >>= function
          | Ok () ->
            Writer.flushed w >>= fun () ->
            read_print_loop r >>= fun _ ->
            repl r w
          | Error _ ->
            (print_string "Unknown command.\n";
            repl r w)

let _ = repl stdin stdout
let _ = Scheduler.go ()