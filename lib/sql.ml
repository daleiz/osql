module Type =
struct
  type t =
    | Text
    | Int
    | Float
    | Bool
    [@@deriving show {with_path=false}]

  let to_string = show
end

type attr = {name : string; domain : Type.t}
  [@@deriving show {with_path=false}]

let make_attribute name domain = {name;domain}

module Schema =
struct
  type t = attr list
    [@@deriving show]

  exception Error of t * string

  let to_string = show
end

type stream_name = string [@@deriving show]
type schema = Schema.t [@@deriving show]

type column_name = string [@@deriving show]
type column_names = column_name list [@@deriving show]

type num_comparison_op = 
  | Equal
  | Less
  | Greater 

type literal_value = 
  | StrValue of string
  | IntValue of int 
  | FloatValue of float 
  | BoolValue of bool 

type num_comparsion_expr = {
 operator : num_comparison_op; 
 column : column_name; 
 operand : literal_value; 
}

let make_comparison_expr op col r = {operator=op; column=col; operand=r;} 

type column =
  | All
  | AllOf of stream_name 
  | Item of string 
  [@@deriving show {with_path=false}]

type select = {
  columns : column list;
  from : stream_name;
  where : num_comparsion_expr option;
  group : column_name option;
  having : num_comparsion_expr option;
}

let make_select c f w g h = {columns=c; from=f; where=w; group=g; having=h;} 

type stmt =
  | Create of stream_name * schema 
  | Insert of stream_name * string list option * literal_value list list  
  | Select of select

(*
open Schema

let test = [{name="a";domain=Type.Int}; {name="b";domain=Type.Int}; {name="c";domain=Type.Text};];;

let () = print test
let () = print (project ["b";"c";"b"] test)
let () = print (project ["b";"d"] test)
let () = print (rename test "a" "new_a")
*)
