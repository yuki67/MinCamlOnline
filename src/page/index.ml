open JsExt

let (||>) x f = f x; x

let find_and_set_text_area id =
  match Document.getElementById id with
    None -> failwith "not found"
  | Some area -> area
                 ||> TextArea.set_cols 80
                 ||> TextArea.set_rows 20
                 ||> TextArea.set_readonly

let syntax_area = find_and_set_text_area "syntax"
let knormal_area = find_and_set_text_area "knormal"
let alpha_area = find_and_set_text_area "alpha"
let optimize_area = find_and_set_text_area "optimize"
let closure_area = find_and_set_text_area "closure"
let virtual_area = find_and_set_text_area "virtual"
let simm_area = find_and_set_text_area "simm"
let reg_alloc_area = find_and_set_text_area "reg"

let text_areas = [
  syntax_area;
  knormal_area;
  alpha_area;
  optimize_area;
  closure_area;
  virtual_area;
  simm_area;
  reg_alloc_area;
]

let program_area =
  match Document.getElementById "program" with
    None -> failwith "not found: program"
  | Some area -> area
                 ||> TextArea.set_cols 80
                 ||> TextArea.set_rows 20

let compile () =
  List.iter (TextArea.set_text "") text_areas;

  let x = TextArea.get_text program_area in
  let x = FromString.syntax x in
  let x = Typing.f x in
  TextArea.set_text (Syntax.string x) syntax_area;
  let x = KNormal.f x in
  TextArea.set_text (KNormal.string x) knormal_area;
  let x = Alpha.f x in
  TextArea.set_text (KNormal.string x) alpha_area;
  let x = Main.iter !Main.limit x in
  TextArea.set_text (KNormal.string x) optimize_area;
  let x = Closure.f x in
  TextArea.set_text (Closure.string x) closure_area;
  let x = Virtual.f x in
  TextArea.set_text (Asm.string x) virtual_area;
  let x = Simm.f x in
  TextArea.set_text (Asm.string x) simm_area;
  let x = RegAlloc.f x in
  TextArea.set_text (Asm.string x) reg_alloc_area

let compile_button =
  match Document.getElementById "compile" with
    None -> failwith "not found: compile"
  | Some button ->
    button
    ||> Button.set_text "compile"
    ||> Button.set_onclick compile

(* from min-caml/test *)
let programs = [
  ("-- select program --", "");
  ("fib", "let rec fib n =
  if n <= 1 then n else
  fib (n - 1) + fib (n - 2) in
print_int (fib 30)");
  ("ackermann", "let rec ack x y =
  if x <= 0 then y + 1 else
  if y <= 0 then ack (x - 1) 1 else
  ack (x - 1) (ack x (y - 1)) in
print_int (ack 3 10)");
  ("adder", "let rec make_adder x =
  let rec adder y = x + y in
  adder in
print_int ((make_adder 3) 7)");
  ("cls-rec", "let x = 10 in
let rec f y =
  if y = 0 then 0 else
  x + f (y - 1) in
print_int (f 123)");
  ("even-odd", "let t = 123 in
let f = 456 in
let rec even x =
  let rec odd x =
    if x > 0 then even (x - 1) else
    if x < 0 then even (x + 1) else
    f in
  if x > 0 then odd (x - 1) else
  if x < 0 then odd (x + 1) else
  t in
print_int (even 789)");
  ("funcomp", "let rec compose f g =
  let rec composed x = g (f x) in
  composed in
let rec dbl x = x + x in
let rec inc x = x + 1 in
let rec dec x = x - 1 in
let h = compose inc (compose dbl dec) in
print_int (h 123)");
  ("gcd", "let rec gcd m n =
  if m = 0 then n else
  if m <= n then gcd m (n - m) else
  gcd n (m - n) in
print_int (gcd 21600 337500)");
  ("inprod-loop", "let rec inprod v1 v2 acc i =
  if i < 0 then acc else
  inprod v1 v2 (acc +. v1.(i) *. v2.(i)) (i - 1) in
let v1 = Array.make 3 1.23 in
let v2 = Array.make 3 4.56 in
print_int (truncate (1000000. *. inprod v1 v2 0. 2))");
  ("inprod-rec", "let rec inprod v1 v2 i =
  if i < 0 then 0.0 else
  v1.(i) *. v2.(i) +. inprod v1 v2 (i - 1) in
let v1 = Array.make 3 1.23 in
let v2 = Array.make 3 4.56 in
print_int (truncate (1000000. *. inprod v1 v2 2))");
  ("inprod", "let rec getx v = (let (x, y, z) = v in x) in
let rec gety v = (let (x, y, z) = v in y) in
let rec getz v = (let (x, y, z) = v in z) in
let rec inprod v1 v2 =
  getx v1 *. getx v2 +. gety v1 *. gety v2 +. getz v1 *. getz v2 in
print_int (truncate (1000000. *. inprod (1., 2., 3.) (4., 5., 6.)))");
  ("non-tail-if", "let x = truncate 1.23 in
let y = truncate 4.56 in
let z = truncate (-.7.89) in
print_int
  ((if z < 0 then y else x) +
   (if x > 0 then z else y) +
   (if y < 0 then x else z))");
  ("non-tail-if2", "let rec f _ = 12345 in
let y = Array.make 10 3 in
let x = 67890 in
print_int (if y.(0) = 3 then f () + y.(1) + x else 7)");
  ("shuffle", "let rec foo a b c d e f =
  print_int a;
  print_int b;
  print_int c;
  print_int d;
  print_int e;
  print_int f in
let rec bar a b c d e f =
  foo b a d e f c in
bar 1 2 3 4 5 6");
  ("sum-tail", "let rec sum acc x =
                  if x <= 0 then acc else
                    sum (acc + x) (x - 1) in
print_int (sum 0 10000)");
  ("sum", "let rec sum x =
             if x <= 0 then 0 else
               sum (x - 1) + x in
print_int (sum 10000)");
]

let program_selector =
  match Document.getElementById "program_selector" with
    None -> failwith "not found: program_selector"
  | Some selector ->
    List.iter (fun (name, program) -> Select.add_option name program selector) programs;
    selector

let _ =
  program_selector
  ||> Select.set_onchange
    (fun () ->
       TextArea.set_text (Select.get_value program_selector) program_area)
