external _insertBefore : 'a -> 'b -> 'c -> unit  = "insertBefore" [@@bs.send]

module Element =
struct
  type t
  external set : t -> string -> 'b -> unit = "" [@@bs.set_index]
  external get : t -> string -> 'b = "" [@@bs.get_index]
  external appendChild : 'a -> unit = "" [@@bs.send.pipe: 'a]
  let insertBefore place elem = _insertBefore (get place "parentNode") elem place
  let hide elem = set (get elem "style") "display" "none"
  let show elem = set (get elem "style") "display" "inline"
end

module Document =
struct
  type t
  external document : t = "document" [@@bs.val]
  external body : Element.t = "document.body" [@@bs.val]
  external current_script : Element.t = "document.currentScript" [@@bs.val]
  external _createElement : t -> string -> 'a = "createElement" [@@bs.send]
  external _getElementById : t -> string -> 'a option =  "getElementById" [@@bs.return null_to_opt] [@@bs.send]
  let createElement str = _createElement document str
  let getElementById str = _getElementById document str
  let just_put elem = Element.insertBefore current_script elem
  let new_line () = just_put (createElement "br")
end

module Context =
struct
  type t
  external clearRect : int -> int -> int -> int -> unit = "" [@@bs.send.pipe: t]
  external beginPath : unit = "" [@@bs.send.pipe: t]
  external stroke : unit = "" [@@bs.send.pipe: t]
  external moveTo : int -> int -> unit = "" [@@bs.send.pipe: t]
  external lineTo : int -> int -> unit = "" [@@bs.send.pipe: t]
  external fillRect : int -> int -> int -> int -> unit = "" [@@bs.send.pipe: t]
  external arc : int -> int -> int -> float -> float -> Js.boolean -> unit = "" [@@bs.send.pipe: t]
end

module Canvas =
struct
  type t
  external set : t -> string -> 'b -> unit = "" [@@bs.set_index]
  external get : t -> string -> 'b = "" [@@bs.get_index]
  external getContext : t -> string -> Context.t = "" [@@bs.send]
  let create () : t = Document.createElement "canvas"
  let set_width n canvas = set canvas "width" (string_of_int n)
  let set_height n canvas = set canvas "height" (string_of_int n)
  let insertBefore place elem = _insertBefore (get place "parentNode") elem place
end

module Range =
struct
  type t
  external set : t -> string -> 'b -> unit = "" [@@bs.set_index]
  external get : t -> string -> 'b = "" [@@bs.get_index]
  let get_value elem : float = float_of_string (get elem "value")
  let _set_float str v elem = set elem str (Printf.sprintf "%f" v)
  let set_max = _set_float "max"
  let set_min  = _set_float "min"
  let set_step = _set_float "step"
  let set_value = _set_float "value"
  let set_oninput (f:unit -> unit) elem = set elem "oninput" f
  let create () : t =
    let elem = Document.createElement "input" in
    set elem "type" "range";
    elem
  let hide elem = set (get elem "style") "display" "none"
  let show elem = set (get elem "style") "display" "inline"
end

module Button =
struct
  type t
  external set : t -> string -> 'b -> unit = "" [@@bs.set_index]
  external get : t -> string -> 'b = "" [@@bs.get_index]
  let set_text (str : string) elem = set elem "value" str
  let set_onclick (f : unit -> unit) elem = set elem "onclick" f
  let create () : t =
    let elem = Document.createElement "input" in
    set elem "type" "button";
    elem
end

module Span =
struct
  type t
  external set : t -> string -> 'b -> unit = "" [@@bs.set_index]
  external get : t -> string -> 'b = "" [@@bs.get_index]
  let create () : t = Document.createElement "span"
  let set_text (str:string) span = set span "textContent" str
  let hide elem = set (get elem "style") "display" "none"
  let show elem = set (get elem "style") "display" "inline"
end

module Select =
struct
  type t
  external set : t -> string -> 'b -> unit = "" [@@bs.set_index]
  external get : t -> string -> 'b = "" [@@bs.get_index]
  let create () : t = Document.createElement "select"
  let get_value select = get select "value"
  let add_option (descr : string) value select =
    let opt = Document.createElement "option" in
    Element.set opt "textContent" descr;
    Element.set opt "value" value;
    select |> Element.appendChild opt
  let set_onchange (f : unit -> unit) select = set select "onchange" f
end

module TextArea =
struct
  type t
  external set : t -> string -> 'b -> unit = "" [@@bs.set_index]
  external get : t -> string -> 'b = "" [@@bs.get_index]
  let create () : t = Document.createElement "textarea"
  let get_text textarea : string = get textarea "value"
  let set_text (str:string) textarea = set textarea "value" str
  let set_cols (v:int) textarea = set textarea "cols" (string_of_int v)
  let set_rows (v:int) textarea = set textarea "rows" (string_of_int v)
  let set_readonly textarea = set textarea "readOnly" (Js.Boolean.to_js_boolean true)
end
