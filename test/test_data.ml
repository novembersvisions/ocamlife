open OUnit2
open Test
open Ocamlife.Data

(* get_data, search, search2, find_entry, data_to_list, remove_data_list *)

let suite =
  "Data.ml"
  >::: [
         ( "get_data empty no limit" >:: fun _ ->
           assert_equal "\n -- \n"
             (get_data "data/for_testing/test1_quotes.csv" None)
             ~printer:(fun x -> x) );
         ( "get_data empty limit" >:: fun _ ->
           assert_equal "\n -- \n"
             (get_data "data/for_testing/test1_quotes.csv" (Some 1))
             ~printer:(fun x -> x) );
         ( "get_data column no limit" >:: fun _ ->
           assert_equal "\nquote 1\nquote 2\nquote 3\n"
             (get_data "data/for_testing/test2_quotes.csv" None)
             ~printer:(fun x -> x) );
         ( "get_data column limit 1" >:: fun _ ->
           assert_equal "\nquote 1\n"
             (get_data "data/for_testing/test2_quotes.csv" (Some 1))
             ~printer:(fun x -> x) );
         ( "get_data column limit 2" >:: fun _ ->
           assert_equal "\nquote 1\nquote 2\n"
             (get_data "data/for_testing/test2_quotes.csv" (Some 2))
             ~printer:(fun x -> x) );
         ( "get_data column limit 0" >:: fun _ ->
           assert_equal "\n\n"
             (get_data "data/for_testing/test2_quotes.csv" (Some 0))
             ~printer:(fun x -> x) );
         ( "get_data rows no limit" >:: fun _ ->
           assert_equal "\na b c\nd e f\ng h i\n"
             (get_data "data/for_testing/test_data.csv" None) ~printer:(fun x ->
               x) );
         ( "get_data rows limit" >:: fun _ ->
           assert_equal "\na b c\nd e f\n"
             (get_data "data/for_testing/test_data.csv" (Some 2))
             ~printer:(fun x -> x) );
         ( "search first row" >:: fun _ ->
           assert_equal true
             (search "a" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search false but in csv" >:: fun _ ->
           assert_equal false
             (search "b" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search false not in csv" >:: fun _ ->
           assert_equal false
             (search "z" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search empty string" >:: fun _ ->
           assert_equal false
             (search "" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search second row" >:: fun _ ->
           assert_equal true
             (search "d" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search last row" >:: fun _ ->
           assert_equal true
             (search "g" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search duplicates" >:: fun _ ->
           assert_equal true
             (search "a" "data/for_testing/test2_data.csv")
             ~printer:string_of_bool );
         ( "search2 not in csv" >:: fun _ ->
           assert_equal false
             (search2 "x" "z" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search2 first row" >:: fun _ ->
           assert_equal true
             (search2 "a" "b" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search2 second row" >:: fun _ ->
           assert_equal true
             (search2 "d" "e" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search2 last row" >:: fun _ ->
           assert_equal true
             (search2 "g" "h" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search2 first but not second" >:: fun _ ->
           assert_equal false
             (search2 "g" "e" "data/for_testing/test_data.csv")
             ~printer:string_of_bool );
         ( "search2 same identifier" >:: fun _ ->
           assert_equal true
             (search2 "a" "a" "data/for_testing/test2_data.csv")
             ~printer:string_of_bool );
         ( "find_entry unique id" >:: fun _ ->
           assert_equal "\ng a i\n"
             (find_entry "g" "data/for_testing/test2_data.csv")
             ~printer:(fun x -> x) );
         ( "find_entry duplicate ids" >:: fun _ ->
           assert_equal "\na b c\n"
             (find_entry "a" "data/for_testing/test2_data.csv")
             ~printer:(fun x -> x) );
         ( "data_to_list empty" >:: fun _ ->
           assert_equal []
             (data_to_list "data/for_testing/test1_quotes.csv")
             ~printer:Test.string_of_list );
         ( "data_to_list three rows" >:: fun _ ->
           assert_equal
             [ "a"; "b"; "c"; "a"; "a"; "f"; "g"; "a"; "i" ]
             (data_to_list "data/for_testing/test2_data.csv")
             ~printer:Test.string_of_list );
         ( "data_to_list three cols" >:: fun _ ->
           assert_equal
             [ "quote 1"; "quote 2"; "quote 3" ]
             (data_to_list "data/for_testing/test2_quotes.csv")
             ~printer:Test.string_of_list );
         ( "remove_data_list one entry" >:: fun _ ->
           assert_equal []
             (remove_data_list [ [ "a" ] ] "a")
             ~printer:Test.string_of_list_list );
         ( "remove_data_list one row first entry" >:: fun _ ->
           assert_equal []
             (remove_data_list [ [ "a"; "b"; "c" ] ] "a")
             ~printer:Test.string_of_list_list );
         ( "remove_data_list one row second entry" >:: fun _ ->
           assert_equal []
             (remove_data_list [ [ "a"; "b"; "c" ] ] "b")
             ~printer:Test.string_of_list_list );
         ( "remove_data_list one row third entry" >:: fun _ ->
           assert_equal []
             (remove_data_list [ [ "a"; "b"; "c" ] ] "c")
             ~printer:Test.string_of_list_list );
         ( "remove_data_list two rows" >:: fun _ ->
           assert_equal
             [ [ "a"; "b"; "c" ] ]
             (remove_data_list [ [ "a"; "b"; "c" ]; [ "d"; "e"; "f" ] ] "d")
             ~printer:Test.string_of_list_list );
         ( "remove_data_list two rows duplicate ids" >:: fun _ ->
           assert_equal
             [ [ "a"; "e"; "f" ] ]
             (remove_data_list [ [ "a"; "b"; "c" ]; [ "a"; "e"; "f" ] ] "a")
             ~printer:Test.string_of_list_list );
         ( "remove_data_list three rows" >:: fun _ ->
           assert_equal
             [ [ "a"; "b"; "c" ]; [ "a"; "e"; "f" ] ]
             (remove_data_list
                [ [ "a"; "b"; "c" ]; [ "a"; "e"; "f" ]; [ "n"; "p"; "f" ] ]
                "n")
             ~printer:Test.string_of_list_list );
         ( "remove_data_list id not found" >:: fun _ ->
           assert_raises Not_found (fun () ->
               remove_data_list
                 [ [ "a"; "b"; "c" ]; [ "a"; "e"; "f" ]; [ "n"; "p"; "f" ] ]
                 "x") );
         ( "remove_data_list empty id" >:: fun _ ->
           assert_raises Not_found (fun () ->
               remove_data_list
                 [ [ "a"; "b"; "c" ]; [ "a"; "e"; "f" ]; [ "n"; "p"; "f" ] ]
                 "") );
         ( "remove_data_list empty" >:: fun _ ->
           assert_raises Not_found (fun () -> remove_data_list [] "") );
       ]

let () = run_tests suite
let () = print_endline "data tests succeeded"
