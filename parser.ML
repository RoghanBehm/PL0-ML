signature PARSER =
sig
  type 'a parser
  val item: token parser
  val optional: 'a parser -> 'a option parser
  val many: 'a parser -> 'a list parser
  val op >>= : 'a parser * ('a -> 'b parser) -> 'b parser
  val op -- : 'a parser * 'b parser -> ('a * 'b) parser
  val op || : 'a parser * 'a parser -> 'a parser
  val op >> : ('a -> 'b) -> 'a parser -> 'b parser
                                                  
end

structure Parser: PARSER =
struct
type 'a parser = token list -> ('a * token list) option
                                                 
datatype cond = BareExp of expr |
                Compare of relop * expr * expr
     and relop = Eq | Neq | Lt | Gt | Leq | Geq
datatype expr =
         Var   of string
         | Num   of int
         | BinOp of binop * expr * expr
         | Neg   of expr
                        
     and binop = Plus | Minus | Times | Divide
                                            
datatype stmt = Assign of string * expr | Call of string | Read of string | Write of expr |
                While of cond * stmt | If of cond * stmt * stmt | Begin of stmt list 
                                                                               
datatype block = Block of {
  consts     : (string * int) list,
  vars       : string list,
  types      : string list,
  procedures : (string * block) list,
  body       : stmt
}

type program = block
                                                          
  infix 1 >>=
  infix 2 ||
  infix 3 --
  infix 4 >>


         
                                                
  fun optional p input =
      case p input of
          SOME (x, xs) => SOME (SOME x, xs)
       | NONE => SOME (NONE, input)

  fun many p input =
      case p input of
          NONE => SOME ([], input) 
       |  SOME (x, xs) =>
          (case many p xs of
              NONE => SOME ([x], xs) (* unreachable *)
            | SOME (y, ys) => SOME (x::y, ys))
                             
  fun (p >> f) input =
      case p input of
          NONE => NONE
        | SOME (x, xs) => (f x, xs) 
              
  fun (p >>= f) input =
      case p input of
          NONE => NONE
       |  SOME (x, xs) => f x xs

  fun (p -- q) input =
      case p input of
          NONE => NONE
       |  SOME (x, xs) =>
          (case q xs of
              NONE => NONE
            | SOME (y, ys) => SOME ((x, y), rest))


  fun (p |-- q) input = p -- q >> (fn (_, b) => b)
  fun (p --| q) input = p -- q >> (fn (a, _) => a)
                          
  fun (p || q) input =
      case p input of
          SOME res => SOME res
        | NONE => q input


  fun LValue token = IDENTIFIER
end
