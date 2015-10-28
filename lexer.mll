{
open Batteries
open Parsing
open Parser
}

let space = [' ' '\t' '\n' '\r']

let digit = ['0'-'9']
let chars = ['a'-'z' 'A'-'Z' '_' ] | digit
let lower = ['a'-'z']
let upper = ['A'-'Z']

let lident = lower chars*
let uident = upper chars*

let int = ['1'-'9'] ['0'-'9']* | '0'

let float = int? '.' digit+

rule token = parse
| space+
    { token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| "true"
    { TRUE }
| "false"
    { FALSE }
| "not"
    { NOT }
| float
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| int
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '-'
    { MINUS }
| '+'
    { PLUS }
| "-."
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| '='
    { EQUAL }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| ','
    { COMMA }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| ";;"
    { SEMISEMI }
| "_"
    { UNDERBAR }
| eof
    { EOF }
| "t"
    { TRUE }
| "nil"
    { FALSE }
| "match"
    { MATCH }
| "with"
    { WITH }
| "fun"
    { FUN }
| "->"
    { ARROW }
| lident
    { LIDENT(Lexing.lexeme lexbuf) }
| uident
    { UIDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
	   (Lexing.lexeme_start lexbuf)
	   (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
