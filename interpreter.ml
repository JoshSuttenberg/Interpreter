open Printf

type const = Int of int | Bool of bool | Error |  String of string | Name of string

type command = Push of const | Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | Cat | And | Or | Not
                             | Less_than | Equal | If | Bind | Quit
let explode s =
 let rec exp i l =
  if i < 0 then l else exp (i - 1) (s.[i] :: l) in
   exp (String.length s - 1) []

let implode l =
 let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let read_lines inc =
 let rec loop acc =
   match input_line inc with
   | l -> loop (l :: acc)
   | exception End_of_file -> List.rev acc
 in
 loop []


let is_int x =
     let int_of_string_opt x =
         try
             Some (int_of_string x)
         with
            | Failure "int_of_string" -> None
     in
         match (int_of_string_opt x) with
             | Some(_) -> true
             | None -> false

let is_string x =
  match x with
  | '"' :: _ -> (
    match List.rev x with
    | '"' :: _ -> true
    | _ -> false
  )
  | _ -> false

let is_letter x =
  if (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') then true else false

let is_digit x =
  if (x >= '0' && x <= '9') then true else false

  (*name ::= <_>letter(letter | digit | _)* *)

let is_name x =
  let rec is_name' y =
    match y with
    | [] -> true
    | '_' :: xs -> is_name' xs
    | _ -> if is_letter (List.hd y) then is_name' (List.tl y)
           else if is_digit (List.hd y) then is_name' (List.tl y)
           else false
  in
    match x with
    | '_' :: xs -> if is_letter (List.hd xs) then is_name' xs else false
    | _ ->         if is_letter (List.hd x) then is_name' (List.tl x) else false

let rec take_off_quotes_from_string x =
  match x with
  | '"'::xs -> take_off_quotes_from_string (List.rev xs)
  | _ -> x

let create_const_with_type xs =
  match xs with
  | ':' :: 't' :: 'r' :: 'u' :: 'e' :: ':' :: [] -> Bool(true)
  | ':' :: 'f' :: 'a' :: 'l' :: 's' :: 'e' :: ':' :: [] -> Bool(false)
  | ':' :: 'e' :: 'r' :: 'r' :: 'e' :: 'r' :: ':' :: [] -> Error
  | _ -> if is_int (implode xs) then Int(int_of_string (implode xs))
         else if is_string xs then String(implode (take_off_quotes_from_string xs))
         else if is_name xs then Name(implode xs)
         else Error

let rec make_command_list string_list command_list variable_stack =
  match string_list with
  | [] -> (command_list, variable_stack)
  | "pop"::tl -> make_command_list tl (command_list @ [Pop]) variable_stack
  | "add"::tl -> make_command_list tl (command_list @ [Add]) variable_stack
  | "sub"::tl -> make_command_list tl (command_list @ [Sub]) variable_stack
  | "mul"::tl -> make_command_list tl (command_list @ [Mul]) variable_stack
  | "div"::tl -> make_command_list tl (command_list @ [Div]) variable_stack
  | "rem"::tl -> make_command_list tl (command_list @ [Rem]) variable_stack
  | "neg"::tl -> make_command_list tl (command_list @ [Neg]) variable_stack
  | "swap"::tl -> make_command_list tl (command_list @ [Swap]) variable_stack
  | "cat"::tl -> make_command_list tl (command_list @ [Cat]) variable_stack
  | "and"::tl -> make_command_list tl (command_list @ [And]) variable_stack
  | "or"::tl -> make_command_list tl (command_list @ [Or]) variable_stack
  | "not"::tl -> make_command_list tl (command_list @ [Not]) variable_stack
  | "equal"::tl -> make_command_list tl (command_list @ [Equal]) variable_stack
  | "lessThan"::tl -> make_command_list tl (command_list @ [Less_than]) variable_stack
  | "bind"::tl -> make_command_list tl (command_list @ [Bind]) variable_stack
  | "quit"::tl -> make_command_list tl (command_list @ [Quit]) variable_stack
  | hd::tl -> let chars = explode hd in
    match chars with
    | 'p' :: 'u' :: 's' :: 'h' :: ' ' :: xs -> make_command_list tl (command_list @ [(Push (create_const_with_type xs))]) variable_stack
    | _ -> make_command_list tl (command_list @ [(Push (Error))]) variable_stack

let const_to_string x =
  match x with
  | Int i -> string_of_int i
  | Bool b -> ":" ^ string_of_bool b ^ ":"
  | String s -> s
  | Name n -> n
  | Error -> ":error:"

let rec write_to_output l oc=
  match l with
  | [] -> ()
  | hd::tl-> Printf.fprintf oc "%s\n"(const_to_string hd); write_to_output tl oc

let pop l =
  match l with
  | [] -> Error::l
  | hd::tl -> tl

let add l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Int i, Int j -> Int(j + i) :: xs
                | _ -> Error :: a :: b :: xs

let sub l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Int i, Int j -> Int (j - i) :: xs
                | _ -> Error :: a :: b :: xs

let mul l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Int i, Int j -> Int(j * i) :: xs
                | _ -> Error :: a :: b :: xs

let div l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Int i, Int j -> if (i != 0) then Int(j / i) :: xs else Error :: a :: b :: xs
                | _ -> Error :: a :: b :: xs

let rem l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Int i, Int j -> if (i != 0) then Int(j mod i) :: xs else Error :: a :: b :: xs
                | _ -> Error :: a :: b :: xs

let neg l =
  match l with
  | [] -> Error :: l
  | a::xs -> match a with
                | Int i -> Int(-i) :: xs
                | _ -> Error :: a :: xs

let swap l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | _ -> b::a::xs

let cat l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | String i, String j -> String (j ^ i) :: xs
                | _ -> Error :: a :: b :: xs

let my_and l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Bool i, Bool j -> Bool (i && j) :: xs
                | _ -> Error :: a :: b :: xs

let my_or l =
match l with
| [] -> Error :: l
| a::[] -> Error :: l
| a::b::xs -> match a,b with
              | Bool i, Bool j -> Bool (i || j) :: xs
              | _ -> Error :: a :: b :: xs

let my_not l =
  match l with
  | [] -> Error :: l
  | a::xs -> match a with
             | Bool i -> Bool (not i) :: xs
             | _ -> Error :: a :: xs

let equal l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Int i, Int j -> if (i == j) then Bool(true) :: xs else Bool(false) :: xs
                | _ -> Error :: a :: b :: xs

let less_than l =
  match l with
  | [] -> Error :: l
  | a::[] -> Error :: l
  | a::b::xs -> match a,b with
                | Int i, Int j -> if (j < i) then Bool(true) :: xs else Bool(false) :: xs
                | _ -> Error :: a :: b :: xs

let rec compute_commands command_list output =
  match command_list with
  | Push a::tl -> compute_commands tl (a :: output)
  | Pop::tl -> compute_commands tl (pop output)
  | Add::tl -> compute_commands tl (add output) (* call funciton to try to pop 2 things off the stack (ouput)*)
  | Sub::tl -> compute_commands tl (sub output)
  | Mul::tl -> compute_commands tl (mul output)
  | Div::tl -> compute_commands tl (div output)
  | Rem::tl -> compute_commands tl (rem output)
  | Neg::tl -> compute_commands tl (neg output)
  | Swap::tl -> compute_commands tl (swap output)
  | Cat::tl -> compute_commands tl (cat output)
  | And::tl -> compute_commands tl (my_and output)
  | Or::tl -> compute_commands tl (my_or output)
  | Not::tl -> compute_commands tl (my_not output)
  | Equal::tl -> compute_commands tl (equal output)
  | Less_than::tl -> compute_commands tl (less_than output)
  | Quit::tl -> output
  | _ -> output

(* String List (read_lines) -> Command List (make_command_list) -> Const List (compute_commands) -> unit*)
let interpreter (input: string) (output: string) : unit =
  let ic=open_in input in
    let oc=open_out output in
      let l=read_lines ic in (*String List*)
        let res= make_command_list l [] [] in (*Command List*)
          let commands = fst res in
            let stack = compute_commands commands (snd res) in (*Const list that will at the end of computation, be output*)
              let _= write_to_output stack oc in
        close_in ic;
      close_out oc

let x = interpreter "C:\\Users\\Josh\\AppData\\Local\\atom\\cse_305\\input" "C:\\Users\\Josh\\AppData\\Local\\atom\\cse_305\\output"


