open Unix
open Array
(* Note: we chose to use a tuple for our tree structure because it's easy to 
 * match on, however we also convert these tuples to *)
(* tree and node types for btree with D = 3, where D is the minimum occupancy *)
type chat = string list 
type btree = 
    | Hole 
    | Leaf of (int * chat) option array 
    | Index of (int * btree) option array

let leaf_size = 6
let idx_size = 7

let rec idx_lst lst key idx =
    match lst with
    | [] -> -1
    | h::t -> if (key = h) then idx
              else idx_lst t key (idx+1)

(* will remove all elements = -1 in the list *)
let rec reduce_lst lst =
    match lst with
    | [] -> lst
    | h::t -> if h = -2 then reduce_lst t
              else h::(reduce_lst t)

(* returns index if key is in leaf node, -1 if key not found *)
let check_leaf leaf_arr key =
    let key_arr = Array.map
    (fun x -> match x with (Some(k,v)) -> k | None -> -1) leaf_arr in
    let key_lst = Array.to_list key_arr in
    let red_lst = reduce_lst key_lst in
    let key_idx = idx_lst red_lst key 0
    in key_idx

(* helper function that determines if leaf entry has room for another key-value pair
let has_space leaf_tuple *)
let has_space arr = 
    let count = Array.fold_left (fun acc x -> if x = None then acc else acc+1) 0 arr
  in count 

(* recurses through list of keys *)
(* precondition: already checked if key is less than the first elt *)
(* returns the index of the key that we need to insert after *)
let rec find_idx key_lst key idx =
    match key_lst with
    | [] -> -1
    | h::s::t -> if ((key >= h) && (key < s)) then idx
                 else find_idx (s::t) key (idx+1)
    | h::[] -> if (key >= h) then idx else -1


(* takes in array of leaf elements *)
(* precondition: already checked if key is less than the first elt *)
(* return index of elt we want to insert after *)
let ins_loc arr key idx = 
  match arr.(idx) with 
  | Some (k,v) ->
    if key < k then (-1)
    else
        let key_arr = Array.map
        (fun x -> match x with Some (k,v) -> k | None -> -2) arr in
        let key_lst = Array.to_list key_arr in
        let red_lst = reduce_lst key_lst in
        (* key that we need to insert new key after: *)
        let prev_key = find_idx red_lst key 0 in
        prev_key
  | None -> failwith "this shouldn't happen"

(* handles splitting the leaf node into a leaf node w/3 entries 
 * and a leaf node w/4 entries *)
let split_3_and_4 idx temp_arr leaf_arr key new_value = 
    let () = Array.blit temp_arr (idx+1) temp_arr (idx+2) (leaf_size-(idx+1)) in
    let () = Array.set temp_arr (idx+1) (Some (key, new_value)) in
    let () = Array.blit temp_arr 0 leaf_arr 0 3 in
    let () = Array.set leaf_arr 3 None in
    let () = Array.set leaf_arr 4 None in
    let () = Array.set leaf_arr 5 None in
    let new_leaf_arr = Array.make leaf_size None in
    let () = Array.blit temp_arr 3 new_leaf_arr 0 4 in
    match new_leaf_arr.(0) with
    | Some (k,v) -> (k, Leaf (new_leaf_arr))
    | None -> failwith "You won't..."

let split_leaf leaf_arr key new_value =
  let temp_arr = Array.make (leaf_size+1) None in
  let () = Array.blit leaf_arr 0 temp_arr 0 leaf_size in 
  match temp_arr.(0) with
  | Some (k,v) ->    
    if key < k then
        split_3_and_4 (-1) temp_arr leaf_arr key new_value
    else
        let idx = ins_loc leaf_arr key 0 in
        split_3_and_4 idx temp_arr leaf_arr key new_value
  | None -> failwith "can't happen"

let split_idx idx_arr key new_value = 
    let temp_arr = Array.make (idx_size+1) None in
    let () = Array.blit idx_arr 0 temp_arr 0 idx_size in
    match temp_arr.(1) with
    | Some (k,v) ->
        if key < k then
            let () = Array.blit temp_arr 1 temp_arr 2 idx_size in
            let () = Array.set temp_arr 1 (Some (key, new_value)) in
            let () = Array.blit temp_arr 0 idx_arr 0 4 in
            let () = Array.set idx_arr 4 None in
            let () = Array.set idx_arr 5 None in
            let () = Array.set idx_arr 6 None in
            let new_idx_arr = Array.make idx_size None in
            let () = Array.blit temp_arr 4 new_idx_arr 0 4 in
            begin 
            match new_idx_arr.(0) with
            | Some (k,v) ->
                let split_key = k in
                let () = Array.set new_idx_arr 0 (Some ((-1), v)) in
                (split_key, Index (new_idx_arr))
            | None -> failwith "Not here"
            end
    else 
        let idx = ins_loc idx_arr key 1 in
        let () = Array.blit temp_arr (idx+1) temp_arr (idx+2) (idx_size-(idx+1)) in
        let () = Array.set temp_arr (idx+1) (Some (key, new_value)) in
        let () = Array.blit temp_arr 0 idx_arr 0 4 in
        let () = Array.set idx_arr 4 None in
        let () = Array.set idx_arr 5 None in
        let () = Array.set idx_arr 6 None in
        let new_idx_arr = Array.make idx_size None in
        let () = Array.blit temp_arr 4 new_idx_arr 0 4 in
        begin
        match new_idx_arr.(0) with
        | Some (k,v) -> 
            let split_key = k in
            let () = Array.set new_idx_arr 0 (Some ((-1), v)) in
            (split_key, Index (new_idx_arr))
        | None -> failwith "fail"
        end
    | None -> failwith "Not today"


let rec insert (key:int) new_value tuple = 
    match tuple with 
    | (_,btree) -> 
        
        (match btree with 
        | Hole -> 
            (* make empty array for leaf *)
            let leaf_arr = Array.make leaf_size None in 
            let () = Array.set leaf_arr 0 (Some (key, new_value)) in 
            let leaf = Leaf (leaf_arr) in
            ((-1), leaf)

        | Leaf leaf_arr -> 
            let check = check_leaf leaf_arr key in
            if check <> (-1) then
                begin
                match leaf_arr.(check) with 
                | Some (k,v) -> let new_v = v@new_value in
                    let () = Array.set leaf_arr check (Some(k,new_v)) in
                    ((-1), Leaf (leaf_arr))

                | None -> failwith "Invalid node"
                end
            else
            let num_elts = has_space leaf_arr in 
            if num_elts < leaf_size then 
                let idx = ins_loc leaf_arr key 0 in
                let () = Array.blit leaf_arr (idx+1) leaf_arr (idx+2) (num_elts-(idx+1)) in
                let () = Array.set leaf_arr (idx+1) (Some (key, new_value)) in
                ((-1), btree)
            else 
                split_leaf leaf_arr key new_value
    | Index idx_arr ->
        let idx = ins_loc idx_arr key 1 in
        begin
        match idx_arr.(idx) with
        | Some (k,v) -> 
            begin
            match insert key new_value ((-1), v) with
            | (-1, child) -> (-1, btree)
            | (split_key, child) ->  
                let num_elts = has_space idx_arr in 
                if num_elts < idx_size then
                    let idx = ins_loc idx_arr split_key 1 in 
                    let () = Array.blit idx_arr (idx+1) idx_arr (idx+2) (num_elts - (idx+1)) in
                    let () = Array.set idx_arr (idx+1) (Some (split_key, child)) in
                    ((-1), btree)
                else 
                    split_idx idx_arr key child
            end
        | None -> failwith "Why here" 
        end)

(* precondition: already checked if key is less than the first elt or
 * greater than the last elt *)
(* helper function to find the key whose pointer we should follow *)
(* returns index of the key ki where ki <= key < ki+1 *)
let rec loop_lst key_lst key idx =
    match key_lst with
    | [] -> -1
    | h::s::t -> if ((key >= h) && (key < s)) then idx
                 else loop_lst (s::t) key (idx+1)
    | h::[] -> if (key >= h) then idx else -1

(* find index of element with key ki where ki <= key < ki+1 *)
let find_elt_idx index_arr key =
    let key_arr = Array.map
    (fun x -> match x with (Some(k,v)) -> k | None -> -1) index_arr in
    let key_lst = Array.to_list key_arr in
    let red_lst = reduce_lst key_lst in
    let key_found = loop_lst red_lst key 0 in
    key_found



let rec search btree key = 
    match btree with
    | Hole -> None
    | Leaf leaf_arr -> 
        (* check_leaf returns index of key in leaf_arr,
         * or -1 if the key is not in the leaf *)
        let check = check_leaf leaf_arr key in
        if check = -1 then None
        else
            (match leaf_arr.(check) with
            | None -> failwith "Invalid entry1"
            | Some (k, v) -> Some (v))
    | Index idx_arr ->
        (match idx_arr.(1) with
        | None -> failwith "Invalid search1"
        | Some (k, v) -> 
            if key < k then 
            (match idx_arr.(0) with 
            | Some (k0,v0) -> search v0 key
            | None -> failwith "fail")
            else 
                (match idx_arr.((has_space idx_arr)-1) with
                | None -> failwith "Invalid entry2"
                | Some (kM, vM) -> 
                    if key >= kM then search vM key
                    else
                        (* find index of element with key ki
                         * where ki <= key < ki+1 *)
                        let index_found = find_elt_idx idx_arr key in
                        let element = if (index_found <> -1) 
                        then idx_arr.(index_found) else None in
                        (match element with
                        | None -> failwith "Invalid entry3"
                        | Some (ki, vi) -> search vi key)))

let return_chat btree key = 
    search btree key


let add_message btree key new_value = 
    match insert key new_value (-1, btree) with
    | (-1,btree') -> btree'
    (* root was split *)
    | (k,btree') -> 
        let new_root_arr = Array.make idx_size None in
        let () = Array.set new_root_arr 1 (Some (k, btree')) in
        let () = Array.set new_root_arr 0 (Some ((-1), btree)) in
        Index (new_root_arr)


  