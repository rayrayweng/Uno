(** Derived from A2 command.ml *)
open Deck

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Play of object_phrase
  | Draw
  | Uno of object_phrase
  | NumCards
  | Turn
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

let parse_color str =
  let trimmed_str = String.trim str in 
  let command_lst = String.split_on_char ' ' trimmed_str in
  let filtered_lst = List.filter (fun x -> String.length x > 0) command_lst in
  if List.length filtered_lst > 0 then 
    match List.hd filtered_lst with
    | "red" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else Red
    | "blue" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else Blue
    | "green" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else Green
    | "yellow" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else Yellow
    | "" -> raise Empty 
    | _ -> raise Malformed
  else raise Empty

(** [parse str] parses a player's input into a [command]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is malformed if it is not one of the commands, or there is a missing object
    phrase or an extra object phrase.*)
let parse str =
  let trimmed_str = String.trim str in 
  let command_lst = String.split_on_char ' ' trimmed_str in
  let filtered_lst = List.filter (fun x -> String.length x > 0) command_lst in
  if List.length filtered_lst > 0 then 
    match List.hd filtered_lst with
    | "play" -> if (List.length (List.tl (filtered_lst)) = 0) then raise Malformed else Play (List.tl (filtered_lst))
    | "draw" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else Draw
    | "uno" -> if (List.length (List.tl (filtered_lst)) = 0) then raise Malformed else Uno (List.tl (filtered_lst))
    | "numcards" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else NumCards
    | "turn" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else Turn
    | "quit" -> if (List.length (List.tl (filtered_lst)) > 0) then raise Malformed else Quit 
    | "" -> raise Empty 
    | _ -> raise Malformed
  else raise Empty