open Unix

(** type of the b+ tree for messages that cannot be delivered at that time.
  * Client and key types are defined as in Client.mli
 *)

type chat = string list 
type btree = 
    | Hole 
    | Leaf of (int * chat) option array 
    | Index of (int * btree) option array


(** [add_message btree key msg] takes in the b+ tree of session key 
  * indices with messages, a session key, and a message (as a list), and 
  * either appends this message to the list of messages already stored at this
  * session key, or inputs a new entry of this session key if it doesn't 
  * already exist and stores the message, and returns the updated tree *)
val add_message : btree -> int -> string list -> btree


(** [return_chat btree session msg] takes in the b+ tree of session key 
  * indices with messages, and a session key, and either returns the list of
  * messages stored at this session key, or returns None if this key is not
  * in the tree *)
val return_chat : btree -> int -> string list option
