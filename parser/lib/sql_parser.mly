%{
  open Sql
%}


%token <int> INTEGER
%token <string> IDENT STRING 
%token <float> FLOAT
%token TRUE FALSE 
%token LPAREN RPAREN COMMA EOF DOT ASTERISK
%token CREATE STREAM IF NOT EXISTS 
%token INSERT INTO VALUES
%token SELECT FROM WHERE GROUP BY HAVING 
%token EQUAL LESS GREATER
%token T_STRING T_INTEGER T_FLOAT T_BOOLEAN

%start <Sql.stmt> input

%%

input: statement EOF { $1 }

statement: CREATE STREAM if_not_exists? name=stream_name schema=stream_definition
              {
                Create (name, schema)
              }
         | INSERT INTO target=stream_name names=sequence(IDENT)? VALUES values=sequence(sequence(literal_value)) 
              {
                Insert ( target, names, values )
              }
         | SELECT r=commas(selected_column) f=from  w=where?  g=group? h=having?
              {
                Select (make_select r f w g h) 
              }

if_not_exists: IF NOT EXISTS { }

stream_definition: s=sequence(column_def) { s }

column_def: name=IDENT t=sql_type { make_attribute name t }

sql_type: 
               | T_STRING { String }
               | T_INTEGER { Int }
               | T_FLOAT { Float }
               | T_BOOLEAN { Bool }

literal_value:
    | s=STRING { StrValue s }
    | i=INTEGER { IntValue i }
    | f=FLOAT { FloatValue f }
    | TRUE { BoolValue true } 
    | FALSE { BoolValue false }

selected_column:
       | stream_name DOT ASTERISK { Sql.AllOf $1 }
       | ASTERISK { Sql.All }
       | name=IDENT { Sql.Item name }

from: FROM s=stream_name { s }

where: WHERE e=comparison_expr { e }

group: GROUP BY k=IDENT { k }

having: HAVING e=comparison_expr { e }

comparison_expr: column=IDENT op=comparison_op r=literal_value {make_comparison_expr op column r }

comparison_op: 
  | EQUAL { Sql.Equal } 
  | LESS { Sql.Less } 
  | GREATER { Sql.Greater }

%inline stream_name: name=IDENT { name }

%inline commas(X): l=separated_nonempty_list(COMMA,X) { l }
(* (x1,x2,...,xn) *)
%inline sequence_(X): LPAREN l=commas(X) { l }
%inline sequence(X): l=sequence_(X) RPAREN { l }

