%{

open Syntax
open Location

let set s = Syntax.r := s

let expression pos_start pos_end desc =
  set (Syntax.sexp_of_expression_desc desc);
  { pexp_loc={pos_start; pos_end};
    pexp_desc=desc
  }

let pattern pos_start pos_end desc =
  set (Syntax.sexp_of_pattern_desc desc);
  { ppat_loc={pos_start; pos_end};
    ppat_desc=desc
  }

let longident pos_start pos_end desc =
  set (Longident.sexp_of_longident_desc desc);
  { Longident.pident_loc={pos_start; pos_end};
    pident_desc=desc
  }

%}

%token <int> INT
%token <float> FLOAT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%right SEMICOLON
%token SEMISEMI
%token EOF

%token TRUE FALSE
%token NOT

%token PLUS MINUS PLUS_DOT MINUS_DOT
%token AST SLASH AST_DOT SLASH_DOT

%left PLUS MINUS /* lowest precedence */

%token EQUAL

%token IF THEN ELSE
%token LET IN REC AND

%token DOT
%token COMMA
%left COMMA
%token LESS_MINUS

%token MATCH WITH

%token FUN ARROW

%token <string> LIDENT
%token <string> UIDENT

%token UNDERBAR
%nonassoc below_COMMA

%{
open Syntax
open Lexing

%}

%start <Syntax.declaration list> toplevel
%%

toplevel:
| stmt = statement EOF { [stmt] }
| stmt = statement m = toplevel { stmt :: m}

statement:
| e = expression SEMISEMI { Exp e }

pattern:
| p = simple_pattern        { p }
| psec = pattern_tuple %prec below_COMMA
  { pattern $startpos $endpos (Ppat_tuple (List.rev psec)) }

%inline simple_pattern:
| UNDERBAR                  { pattern $startpos $endpos Ppat_any }
| LPAREN RPAREN             { pattern $startpos $endpos Ppat_unit }
| v = lident                { pattern $startpos $endpos (Ppat_var v) }
| LPAREN p = pattern RPAREN { p }


pattern_tuple:
| seq = pattern_tuple; COMMA; p = simple_pattern { p :: seq }
| p = simple_pattern                                    { [p] }

ident:
| l = LIDENT { longident $startpos $endpos (Longident.Pident l) }
| u = UIDENT DOT seq = ident { longident $startpos $endpos (Longident.Pdot (u, seq)) }

lident:
| l = LIDENT { longident $startpos $endpos (Longident.Pident l) }

rec_flag:
  /* empty */ { false }
| REC         { true }

let_binding:
| LET r = rec_flag p = pattern EQUAL e = expression
  { (r, (p, e)) }
let_bindings:
| l = let_binding { let (r, pair) = l in (r, [pair]) }
| l = let_binding seq = and_let_bindings { let (r, pair) = l in (r, pair::seq) }

and_let_bindings:
| /* empty */ { [] }
| AND p = pattern EQUAL e = expression seq = and_let_bindings
  { (p, e) :: seq }

expression:
| i = INT
  { expression $startpos $endpos (Int i) }
| f = FLOAT
  { expression $startpos $endpos (Float f) }
| LPAREN e = expression RPAREN
  { e }
| e1 = expression SEMICOLON e2 = expression
    { expression $startpos $endpos (Seq (e1, e2))}
| id = ident { expression $startpos $endpos (Var id) }
| l = let_bindings IN e = expression
  { let (r, seq) = l in
    let e = Let (r, seq, e) in
    expression $startpos $endpos e
  }
