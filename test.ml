open OUnit2
open BPlusTree

let tree = Hole
let tree1 = Leaf [|Some (1, ["1"]); None; None; None; None; None|]
let tree11 = Leaf [|Some (1, ["1";"1"]); None; None; None; None; None|]
let tree2 = Leaf [|Some (1, ["1";"1"]); Some (2, ["2"]); None; None; None; None|]
let treeF = Leaf [|Some (1, ["1";"1"]); Some (2, ["2"]); Some (3, ["3"]); Some (4, ["4"]); Some (5, ["5"]); None|]
let treeFF = Leaf [|Some (1, ["1";"1"]); Some (2, ["2"]); Some (3, ["3"]); Some (4, ["4"]); Some (5, ["5"]); Some (6, ["6"])|]

let treeI = Index [|Some (-1, Leaf [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;None|]);
    Some (4,Leaf [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]);Some (7, ["7"]); None; None|]);
    None; None; None; None; None|]

let treeI2 = Index [|Some (-1, Leaf [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;None|]);
    Some (4,Leaf [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]);Some (7, ["7"]); Some (8, ["8"]); None|]);
    None; None; None; None; None|]

let treeA = Index [|Some (-1, Leaf [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;None|]);
    Some (4,Leaf [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]);Some (7, ["7"]); Some (8, ["8"]); Some (9, ["9"])|]);
    None; None; None; None; None|]

let treeB = Index [|Some
      (-1,
       Leaf
        [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;
          None|]);
     Some
      (4,
       Leaf
        [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]); None; None;
          None|]);
     Some
      (7,
       Leaf
        [|Some (7, ["7"]); Some (8, ["8"]); Some (9, ["9"]); Some (10, ["10"]);
          None; None|]);
     None; None; None; None|]

let treeC =
Index [|Some
      (-1,
       Leaf
        [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;
          None|]);
     Some
      (4,
       Leaf
        [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]); None; None;
          None|]);
     Some
      (7,
       Leaf
        [|Some (7, ["7"]); Some (8, ["8"; "8"]); Some (9, ["9"]);
          Some (10, ["10"]); None; None|]);
     None; None; None; None|]

let treeD =
Index [|Some
      (-1,
       Leaf
        [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;
          None|]);
     Some
      (4,
       Leaf
        [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]); None; None;
          None|]);
     Some
      (7,
       Leaf
        [|Some (7, ["7"]); Some (8, ["8"; "8"]); Some (9, ["9"]);
          Some (10, ["10"]); Some (11, ["11"]); Some (12, ["12"])|]);
     None; None; None; None|]

let treeE =
Index [|Some
      (-1,
       Leaf
        [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;
          None|]);
     Some
      (4,
       Leaf
        [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]); None; None;
          None|]);
     Some
      (7,
       Leaf
        [|Some (7, ["7"]); Some (8, ["8"; "8"]); Some (9, ["9"]); None; None;
          None|]);
     Some
      (10,
       Leaf
        [|Some (10, ["10"]); Some (11, ["11"]); Some (12, ["12"]);
          Some (13, ["13"]); None; None|]);
     None; None; None|]

let treeF =
  Index [|Some
      (-1,
       Leaf
        [|Some (1, ["1"; "2"]); Some (2, ["2"]); Some (3, ["3"]); None; None;
          None|]);
     Some
      (4,
       Leaf
        [|Some (4, ["4"; "9"]); Some (5, ["5"]); Some (6, ["6"]); None; None;
          None|]);
     Some
      (7,
       Leaf
        [|Some (7, ["7"; "hello!"]); Some (8, ["8"; "8"]); Some (9, ["9"]);
          None; None; None|]);
     Some
      (10,
       Leaf
        [|Some (10, ["10"]); Some (11, ["11"]); Some (12, ["12"]);
          Some (13, ["13"]); None; None|]);
     None; None; None|]

let b_plus_test = [

  "insert hole"  >::
  (fun _ -> assert_equal
    (tree1)
    (add_message tree 1 ["1"]));

  "insert 1 again"  >::
  (fun _ -> assert_equal
    (tree11)
    (add_message tree1 1 ["1"]));


  "insert 2"  >::
  (fun _ -> assert_equal
    (tree2)
    (add_message tree11 2 ["2"]));

  "insert to full"  >::
  (fun _ -> assert_equal
    (treeFF)
    (add_message treeFF 6 ["6"]));

  "insert to full2"  >::
  (fun _ -> assert_equal
    (treeI2)
    (add_message treeI 8 ["8"]));

  "insert to index add message"  >::
  (fun _ -> assert_equal
    (treeC)
    (add_message treeB 8 ["8"]));

  "insert to index add message"  >::
  (fun _ -> assert_equal
    (treeE)
    (add_message treeD 13 ["13"]));

  "return chat 3"  >::
  (fun _ -> assert_equal
    (Some ["3"])
    (return_chat treeE 3));

  "return chat 10"  >::
  (fun _ -> assert_equal
    (Some ["10"])
    (return_chat treeE 10));

  "return chat 1"  >::
  (fun _ -> assert_equal
    (Some ["1"; "2"])
    (return_chat treeE 1));

  "insert to old message"  >::
  (fun _ -> assert_equal
    (treeF)
    (add_message treeE 7 ["hello!"]));

]

let _ = run_test_tt_main ("suite" >::: b_plus_test )
