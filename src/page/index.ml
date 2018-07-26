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
let emit_area = find_and_set_text_area "emit"

let text_areas = [
  syntax_area;
  knormal_area;
  alpha_area;
  optimize_area;
  closure_area;
  virtual_area;
  simm_area;
  reg_alloc_area;
  emit_area;
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
  TextArea.set_text (Asm.string x) reg_alloc_area;
  TextArea.set_text (Emit.f x) emit_area

let compile_button =
  match Document.getElementById "compile" with
    None -> failwith "not found: compile"
  | Some button ->
    button
    ||> Button.set_text "compile"
    ||> Button.set_onclick compile

let program_selector =
  match Document.getElementById "program_selector" with
    None -> failwith "not found: program_selector"
  | Some selector ->
    List.iter
      (fun (name, program) -> Select.add_option name program selector)
      SamplePrograms.v;
    selector

let _ =
  program_selector
  ||> Select.set_onchange
    (fun () ->
       TextArea.set_text (Select.get_value program_selector) program_area)
