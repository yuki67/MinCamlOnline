open JsExt

let (||>) x f = f x; x

let find_and_set_text_area id =
  match Document.getElementById id with
    None -> failwith "not found"
  | Some area -> area
                 ||> TextArea.set_cols 160
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

let program_area =
  match Document.getElementById "program" with
    None -> failwith "not found: program"
  | Some area -> area
                 ||> TextArea.set_cols 80
                 ||> TextArea.set_rows 20
                 ||> TextArea.set_text "let x = 1 in print_int 1"

let compile () =
  TextArea.get_text program_area
  |> FromString.syntax
     ||> (fun e -> TextArea.set_text (Syntax.string e) syntax_area)
  |> Typing.f
  |> KNormal.f
     ||> (fun e -> TextArea.set_text (KNormal.string e) knormal_area)
  |> Alpha.f
     ||> (fun e -> TextArea.set_text (KNormal.string e) alpha_area)
  |> Main.iter !Main.limit
     ||> (fun e -> TextArea.set_text (KNormal.string e) optimize_area)
  |> Closure.f
     ||> (fun e -> TextArea.set_text (Closure.string e) closure_area)
  |> Virtual.f
     ||> (fun e -> TextArea.set_text (Asm.string e) virtual_area)
  |> Simm.f
     ||> (fun e -> TextArea.set_text (Asm.string e) simm_area)
  |> RegAlloc.f
     ||> (fun e -> TextArea.set_text (Asm.string e) reg_alloc_area)
  |> ignore

let compile_button =
  Js.log "hoge";
  match Document.getElementById "compile" with
    None -> failwith "not found: compile"
  | Some button -> Js.log button;
    button
    ||> Button.set_text "compile"
    ||> Button.set_onclick compile

let _ = ()
