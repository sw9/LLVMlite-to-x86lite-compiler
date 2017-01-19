open Assert
open Gradedtests
   
(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let c_link_test c_files path args =
  let ll_ast = Driver.parse_file path in
  let output_path = !Platform.output_path in
  let dot_s_file = Platform.gen_name output_path "test" ".s" in
  let exec_file = Platform.gen_name output_path "exec" "" in
  let asm_ast = Backend.compile_prog ll_ast in
  let asm_str = X86.string_of_prog asm_ast in
  let _ = Driver.write_file dot_s_file asm_str in
  let _ = Platform.link (dot_s_file::c_files) exec_file in
  let args = String.concat " " args in
  let result = Driver.run_executable args exec_file in
  let _ = Platform.sh (Printf.sprintf "rm -f %s %s" dot_s_file exec_file) Platform.ignore_error in
  Int64.of_int result

let executed_c_link tests =
  List.map (fun (c_file, fn, args, ans) ->
      (fn ^ ":" ^ (String.concat " " args)), assert_eqf (fun () -> c_link_test c_file fn args) ans)
           tests

let arithmetic_tests =
  [ "llprograms/add_twice.ll", 29L 
  ; "llprograms/sub_neg.ll", 255L (* Why, oh why, does the termianl only report the last byte? *)
  ; "llprograms/arith_combo.ll", 4L
  ; "llprograms/return_intermediate.ll", 18L ]

let noam_lucas_tests =
  [ "palindrome.ll", 0x01L ]


let c_link_test = [
    ["c_weighted_sum.c"], "llprograms/weighted_sum.ll", [], 204L
  ]

let sum_tree_test = ["sum_tree.ll", 116L]

let mumick_sibner_tests =
  [ "llprograms/gcd_euclidian.ll", 2L
  ]

let binary_search_tests =
  [ "llprograms/binarysearch.ll", 8L]

let provided_tests : suite = [
    GradedTest ("Posted Piazza Test Case", 5,
                executed binary_search_tests
               );
    GradedTest ("Other Student Piazza Tests", 5,
                executed arithmetic_tests
               );
    GradedTest ("Other Student Piazza Tests", 5,
                executed arithmetic_tests
               );  GradedTest ("Other Student Piazza Tests", 5,
                               executed noam_lucas_tests
                              );  GradedTest ("Other Student Piazza Tests", 5,
                                              executed  mumick_sibner_tests
                                             );
    GradedTest ("Other Student Piazza Tests", 5,
                executed_c_link c_link_test
               )
  ]
