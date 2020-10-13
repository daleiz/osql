type sql_data_type =
  | String 
  | Int
  | Float
  | Bool

let sql_data_type_to_str t =  
  match t with
    String -> "string" 
  | Int -> "int" 
  | Float -> "float" 
  | Bool -> "bool" 

type literal_value = 
  | StrValue of string
  | IntValue of int 
  | FloatValue of float 
  | BoolValue of bool 

let literal_value_to_str v = 
  match v with
    StrValue s -> "\"" ^ s ^ "\""
  |IntValue i -> Int.to_string i 
  |FloatValue f -> Float.to_string f 
  |BoolValue b -> Bool.to_string b 

let literal_values_to_str vs = 
  match vs with
    [] -> ""
  | [x] -> "(" ^ (literal_value_to_str x)  ^ ")" 
  | x :: xs -> 
    let fs = literal_value_to_str x in 
    let xss = List.map literal_value_to_str xs in 
    let f acc s = acc ^ ", " ^ s in 
    let rs = List.fold_left f fs xss in
    "(" ^ rs ^ ")"

type num_comparison_op = 
  | Equal
  | Less
  | Greater 

let num_comparison_op_to_str op = 
  match op with
    Equal -> "="
  | Less -> "<"
  | Greater -> ">"

type num_comparsion_expr = {
  operator : num_comparison_op; 
  column : string; 
  operand : literal_value; 
}

let num_comparison_expr_to_str {operator; column; operand} = 
  column ^ " " ^ (num_comparison_op_to_str operator) ^ " " ^ (literal_value_to_str operand)

let make_comparison_expr op col r = {operator=op; column=col; operand=r;} 

type selected_column =
  | All
  | AllOf of string 
  | Item of string 

let selected_column_to_str s =
  match s with
    All -> "*"
  | AllOf stream_name -> stream_name ^ ".*"
  | Item col -> col 

let selected_columns_to_str cols = 
  match cols with
    [x] -> selected_column_to_str x 
  | x :: xs -> 
    let fs = selected_column_to_str x in 
    let xss = List.map selected_column_to_str xs in 
    let f acc s = acc ^ ", " ^ s in 
    List.fold_left f fs xss 
  | [] -> ""

type select = {
  selected_columns : selected_column list;
  from : string;
  where : num_comparsion_expr option;
  group : string option;
  having : num_comparsion_expr option;
}

let select_to_str {selected_columns; from; where; group; having} = 
  let s = 
    "select " ^ 
    (selected_columns_to_str selected_columns) ^
    " from " ^ 
    from 
  in let ws = 
       match where with
         Some w -> " where " ^ (num_comparison_expr_to_str w)
       | None -> ""
  in let gs =  
       match group with
         Some g -> " group by " ^ g 
       | None -> ""
  in let hs =  
       match having with
         Some h -> " having " ^ (num_comparison_expr_to_str h)
       | None -> ""
  in s ^ ws ^ gs ^ hs 

let make_select c f w g h = {selected_columns=c; from=f; where=w; group=g; having=h;} 

type attr = {name : string; domain : sql_data_type}

let attr_to_str {name; domain} = name ^ " " ^ (sql_data_type_to_str domain)

let attrs_to_str attrs =
  match attrs with
    [] -> ""
  | [x] -> "(" ^ (attr_to_str x)  ^ ")" 
  | x :: xs -> 
    let fs = attr_to_str x in 
    let xss = List.map attr_to_str xs in 
    let f acc s = acc ^ ", " ^ s in 
    let rs = List.fold_left f fs xss in
    "(" ^ rs ^ ")"

let make_attribute name domain = {name;domain}

type stmt =
  | Create of string * attr list 
  | Insert of string * string list option * literal_value list list  
  | Select of select

let stmt_to_str statement = 
  match statement with
    Create (name, schema) -> "create stream " ^ name ^ " " ^ (attrs_to_str schema)
  | Insert (name, cols, values) -> 
    begin
      let s = "insert into " ^ name in
      let cs = 
        match cols with 
          None -> ""
        | Some cs -> 
          begin
            match cs with
              [] -> ""
            | [x] -> "(" ^ x ^ ")" 
            | x :: xs -> 
              let fs = x in 
              let f acc s = acc ^ ", " ^ s in 
              let rs = List.fold_left f fs xs in

              " (" ^ rs ^ ")"
          end
      in let vs =  
           match values with
             [] -> ""
           | [x] -> "(" ^ (literal_values_to_str x)  ^ ")" 
           | x :: xs -> 
             let fs = literal_values_to_str x in 
             let xss = List.map literal_values_to_str xs in 
             let f acc s = acc ^ ", " ^ s in 
             let rs = List.fold_left f fs xss in
             " (" ^ rs ^ ")"
      in s ^ cs ^ " values" ^ vs
    end
  |Select select -> select_to_str select 
