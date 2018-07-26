
(* from min-caml/test *)
let v = [
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
  ("mandelbrot (raise error)", "
let rec dbl f = f +. f in
let rec yloop y =
  if y >= 400 then () else
  let rec xloop x y =
    if x >= 400 then () else
                let cr = dbl (float_of_int x) /. 400.0 -. 1.5 in
                let ci = dbl (float_of_int y) /. 400.0 -. 1.0 in
                let rec iloop i zr zi zr2 zi2 cr ci =
                  if i = 0 then print_int 1 else
                  let tr = zr2 -. zi2 +. cr in
                  let ti = dbl zr *. zi +. ci in
                  let zr = tr in
                  let zi = ti in
                  let zr2 = zr *. zr in
                  let zi2 = zi *. zi in
                  if zr2 +. zi2 > 2.0 *. 2.0 then print_int 0 else
                  iloop (i - 1) zr zi zr2 zi2 cr ci in
                iloop 1000 0.0 0.0 0.0 0.0 cr ci;
    xloop (x + 1) y in
  xloop 0 y;
  yloop (y + 1) in
yloop 0");
]
