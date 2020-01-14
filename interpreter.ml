(*Different types our stackValues can be and how we will represent their types for our interpreter to use *)
type const = Int of int | Bool of bool | Error | String of string | Name of string | Unit


(* Different types of commands we have to implement functions for*)
type command = Pushi of const | Pushb of const | Pushs of const | Pushn of const | Push of const | Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | Quit
             | Cat | And | Or | Not | LessThan | Equal | If | Bind | Let | End

(* find the value v to which key name is bound, if any, in the assocation list *)
let rec fetch (name:const) (m:(const * const) list) : const =
  match m with
    [] -> Error(*if name is not in memory then simply return back name*)
  | hd::tl ->( match hd with
                (n,value) -> if n = name
                               then ( match value with
                                       Name(x)-> fetch value m
                                      | _-> value(*If name is binded to value return the value in place of name*) )
                               else fetch name tl
              | _ -> fetch name tl     )

(***************************************************************************************************************)
(*************************************PART 1 BASIC COMPUTATION**************************************************)
(***)
let pushi (num:const) (s:const list) : const list  =
  match num with
    Int(x) -> Int(x)::s
  | _ -> Error::s

(***)
let pushs (str:const) (s:const list) : const list =
  match str with
    String(x) -> if String.get x 0 = '"' then String(x)::s else Error::s
  | _ -> Error::s


(***)
(*List containing characters that are allowed to be at the 0th index of a Name(str) data type*)
let name0List = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';
                 'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'_']

(*Recursive function that runs thru name0List and returns true if any first index matches any of the allowed characters *)
let rec confirmName string name0List =
  match name0List with
  | [] -> false
  | hd :: tl -> if String.get string 0 = hd
                then true
                else confirmName string tl


let pushn (n:const) (s:const list) : const list =
  match n with
    Name(name) -> if (confirmName name name0List) = true
                  then Name(name)::s
                  else Error::s
  | _ -> Error::s


(***)
let pushb (boolean:const) (s:const list) : const list=
  match boolean  with
    Bool (x) -> Bool(x)::s
  | _ -> Error::s



(***)
let push (e:const) (s:const list) : const list=
  match e with
    Error -> Error::s
  | Unit -> Unit::s
  | _ -> Error::s


(***)
let popF (s:const list) : const list=
  match s with
    [] -> Error::[]
  | hd::tl -> tl


(***)
let addF (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | v::[] -> Error::v::[]
  | Int(v2)::(v1)::s ->( match v1 with
                          Int(x) -> Int(v2 + x)::s
                         | Name(xs) -> (match (fetch v1 m) with
                                        Int(x) -> Int(v2 + x)::s
                                       | _ ->  Error::Int(v2)::(v1)::s)
                         | _ -> Error::Int(v2)::(v1)::s )
  | (v2)::Int(v1)::s ->( match v2 with
                          Int(x) -> Int(x + v1)::s
                         | Name(xs) -> (match (fetch v2 m) with
                                        Int(x) -> Int(x + v1)::s
                                       | _ ->  Error::v2::Int(v1)::s)
                         | _ -> Error::v2::Int(v1)::s )
  | v2::v1::s -> (match (v2,v1) with
                  (Name(xs),Name(ys))-> (match ((fetch v2 m), (fetch v1 m)) with
                                       (Int(x), Int(y)) -> Int(x+y)::s
                                       | _ -> Error::v2::v1::s            )
                 | _ -> Error::v2::v1::s      )
  | _ -> Error::s


(***)
let subF (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | v::[] -> Error::v::[]
  | Int(v2)::(v1)::s ->( match v1 with
                          Int(x) -> Int(x - v2)::s
                         | Name(xs) -> (match (fetch v1 m) with
                                        Int(x) -> Int(x - v2)::s
                                       | _ ->  Error::Int(v2)::(v1)::s)
                         | _ -> Error::Int(v2)::(v1)::s )
  | (v2)::Int(v1)::s ->( match v2 with
                          Int(x) -> Int(v1 - x)::s
                         | Name(xs) -> ( match (fetch v2 m) with
                                         Int(x) -> Int(v1 - x)::s
                                        | _ ->  Error::v2::Int(v1)::s)
                         | _ -> Error::v2::Int(v1)::s )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(xs),Name(ys))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                           (Int(x), Int(y)) -> Int(y-x)::s
                                          | _ -> Error::v2::v1::s            )
                  | _ -> Error::v2::v1::s      )
  | _ -> Error::s

(***)
let mulF (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | v::[] -> Error::v::[]
  | Int(v2)::(v1)::s ->( match v1 with
                          Int(x) -> Int(v2 * x)::s
                         | Name(xs) -> ( match (fetch v1 m) with
                                         Int(x) -> Int(v2 * x)::s
                                        | _ ->  Error::Int(v2)::(v1)::s)
                         | _ -> Error::Int(v2)::(v1)::s )
  | (v2)::Int(v1)::s ->( match v2 with
                          Int(x) -> Int(x * v1)::s
                         | Name(xs) -> ( match (fetch v2 m) with
                                         Int(x) -> Int(x * v1)::s
                                        | _ ->  Error::v2::Int(v1)::s)
                         | _ -> Error::v2::Int(v1)::s )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(xs),Name(ys))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                           (Int(x), Int(y)) -> Int(x*y)::s
                                          | _ -> Error::v2::v1::s            )
                   | _ -> Error::v2::v1::s      )
  | _ -> Error::s


(***)
let divF (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | coxnst::[] -> Error::coxnst::[]
  | Int(v2)::(v1)::s ->( match (v1,Int(v2)) with
                          (Int(x),Int(v2)) -> if v2 = 0 then Error::Int(v2)::(v1)::s else Int(x / v2)::s
                         | (Name(xs),Int(v2)) -> (match ((fetch v1 m),Int(v2)) with
                                                 (Int(x),Int(v2)) -> (if v2 = 0 then Error::Int(v2)::(v1)::s else Int(x / v2)::s)
                                                 | _ ->  Error::Int(v2)::(v1)::s          )
                         | _ -> Error::Int(v2)::(v1)::s )
  | v2::Int(v1)::s ->( match (v2,Int(v1)) with
                        (Int(x),Int(v1)) -> if x = 0 then Error::v2::Int(v1)::s else Int(v1 / x)::s
                       | (Name(xs),Int(v1)) -> (match ((fetch v2 m),Int(v1)) with
                                               (Int(x),Int(v1)) -> (if x = 0 then Error::v2::Int(v1)::s else Int(v1 / x)::s)
                                               | _ ->  Error::v2::Int(v1)::s                  )
                       | _ -> Error::v2::Int(v1)::s )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(xs),Name(ys))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                           (Int(x), Int(y)) -> if x = 0 then Error::v2::v1::s else Int(y / x)::s
                                          | _ -> Error::v2::v1::s            )
                  | _ -> Error::v2::v1::s      )
  | _ -> Error::s


(***)
let remF (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | coxnst::[] -> Error::coxnst::[]
  | Int(v2)::(v1)::s ->( match (v1,Int(v2)) with
                          (Int(x),Int(v2)) -> if v2 = 0 then Error::Int(v2)::(v1)::s else Int(x mod v2)::s
                         | (Name(xs),Int(v2)) -> ( match ((fetch v1 m),Int(v2)) with
                                                   (Int(x),Int(v2)) -> (if v2 = 0 then Error::Int(v2)::(v1)::s else Int(x mod v2)::s)
                                                  | _ ->  Error::Int(v2)::(v1)::s          )
                         | _ -> Error::Int(v2)::(v1)::s )
  | v2::Int(v1)::s ->( match (v2,Int(v1)) with
                        (Int(x),Int(v1)) -> if x = 0 then Error::v2::Int(v1)::s else Int(v1 mod x)::s
                       | (Name(xs),Int(v1)) -> ( match ((fetch v2 m),Int(v1)) with
                                                 (Int(x),Int(v1)) -> (if x = 0 then Error::v2::Int(v1)::s else Int(v1 mod x)::s)
                                                | _ ->  Error::v2::Int(v1)::s                  )
                       | _ -> Error::v2::Int(v1)::s )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(xs),Name(ys))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                             (Int(x), Int(y)) -> if x = 0 then Error::v2::v1::s else Int(y / x)::s
                                           | _ -> Error::v2::v1::s            )
                   | _ -> Error::v2::v1::s      )
  | _ -> Error::s


(***)
let negF (s:const list) (m:(const * const) list): const list=
  match s with
    [] -> Error::[]
  | v1::s -> match v1 with
              Name(xx)-> ( match (fetch v1 m) with
                           Int(b) -> Int(-1 *b)::s
                         | _ -> Error::v1::s        )

            | Int(x) -> Int(-1 * x)::s
            | _ -> Error::v1::s



(***)
let swapF (s:const list) : const list=
  match s with
    [] -> Error::[]
  | v1::v2::s -> v2::v1::s
  | v::[] -> Error::v::[]

(***)
(*********PARTS NEEDED TO PRINT OUT STACK TO OUTPUT ONCE QUIT COMMAND IS CALLED*********)

(* Helper function: file output function. It takes a bool value and
   write it to the output file. *)
let file_write oc str = Printf.fprintf oc "%s\n" str; print_endline str

(* Recursive Function that runs thru our stackList and gives outputs as strings according to different stackValue type*)
let rec printStackToOutput oc (str:string list) =
  match str with
   [] -> []
  | hd::tl ->  file_write oc hd; printStackToOutput oc tl


let rec stackValsToString (s: const list) : string list =
  match s with
   [] -> []
  | hd::tl -> (match hd with
      | Int(x) -> (string_of_int x)::(stackValsToString tl)
      | Bool(x) -> ((":"^(string_of_bool x)^":"))::(stackValsToString tl)
      | String(x) ->  (let len = (String.length x) - 2 in String.sub x 1 len)::(stackValsToString tl)
      | Name(x) ->  x::(stackValsToString tl)
      | Error -> ":error:"::(stackValsToString tl)
      | Unit -> ":unit:"::(stackValsToString tl)
    )

let quitF (s:const list) : const list =
  s

(***************************************************************************************************************)
(*************************************PART 2 VARIABLES AND SCOPE************************************************)
(***)
let cat (s:const list) (m:(const * const) list): const list=
  match s with
    [] -> Error::[]
  | String(v2)::String(v1)::s ->  String( (let len = (String.length v1) - 1 in String.sub v1 0 len) ^ (let len = (String.length v2) - 1 in String.sub v2 1 len) )::s
  | String(v2)::(v1)::s ->( match v1 with
                             String(x) -> String( (let len = (String.length x) - 1 in String.sub x 0 len) ^ (let len = (String.length v2) - 1 in String.sub v2 1 len) )::s
                            | Name(xs) -> ( match (fetch v1 m) with
                                            String(x) -> String( (let len = (String.length x) - 1 in String.sub x 0 len) ^ (let len = (String.length v2) - 1 in String.sub v2 1 len) )::s
                                           | _ ->  Error::String(v2)::(v1)::s )
                            | _ -> Error::String(v2)::(v1)::s )
  | (v2)::String(v1)::s ->( match v2 with
                             String(x) -> String( (let len = (String.length v1) - 1 in String.sub v1 0 len) ^ (let len = (String.length x) - 1 in String.sub x 1 len) )::s
                            | Name(xs) -> ( match (fetch v2 m) with
                                            String(x) -> String( (let len = (String.length v1) - 1 in String.sub v1 0 len) ^ (let len = (String.length x) - 1 in String.sub x 1 len) )::s
                                           | _ ->  Error::(v2)::String(v1)::s )
                            | _ -> Error::(v2)::String(v1)::s                         )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(n1),Name(n2))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                           (String(x), String(y)) -> String( (let len = (String.length y) - 1 in String.sub y 0 len) ^ (let len = (String.length x) - 1 in String.sub x 1 len) )::s
                                          | _ -> Error::v2::v1::s            )
                  | _ -> Error::v2::v1::s      )
  | _ -> Error::s


(***)
let andF (s:const list) (m:(const * const) list): const list=
  match s with
    [] -> Error::[]
  | v1::[] -> Error::v1::[]
  | Bool(v2)::Bool(v1)::s -> if v2 = true
                             then ( if v1 = (true) then Bool(true)::s else Bool(false)::s )
                             else  Bool(false)::s
  | Bool(v2)::(v1)::s ->( match v1 with
                           Bool(x) -> if v2 = true
                                      then ( if x = (true) then Bool(true)::s else Bool(false)::s )
                                      else  Bool(false)::s
                          | Name(sx) -> ( match (fetch v1 m) with
                                          Bool(x) -> if v2 = true
                                                     then ( if x = (true) then Bool(true)::s else Bool(false)::s )
                                                     else  Bool(false)::s
                                         | _ ->  Error::Bool(v2)::(v1)::s )
                          | _ -> Error::Bool(v2)::(v1)::s )
  | (v2)::Bool(v1)::s ->( match v2 with
                           Bool(x) -> if x = true
                                      then ( if v1 = (true) then Bool(true)::s else Bool(false)::s )
                                      else  Bool(false)::s
                          | Name(sx) -> ( match (fetch v2 m) with
                                          Bool(x) -> if x = true
                                                     then ( if v1 = (true) then Bool(true)::s else Bool(false)::s )
                                                     else  Bool(false)::s
                                         | _ ->  Error::(v2)::Bool(v1)::s )
                          | _ -> Error::(v2)::Bool(v1)::s                         )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(sx),Name(sy))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                           (Bool(x), Bool(y)) -> if x = true
                                                                 then ( if y = (true) then Bool(true)::s else Bool(false)::s )
                                                                 else  Bool(false)::s
                                          | _ -> Error::v2::v1::s            )
                   | _ -> Error::v2::v1::s      )
  | _ -> Error::s


(***)
let orF (s:const list) (m:(const * const) list): const list=
  match s with
    [] -> Error::[]
  | v1::[] -> Error::v1::[]
  | Bool(v2)::Bool(v1)::s -> if v2 = (false)
                             then ( if v1 = (true) then Bool(true)::s else Bool(false)::s )
                             else  Bool(true)::s
  | Bool(v2)::(v1)::s ->( match v1 with
                           Bool(x) -> if v2 = false
                                      then ( if x = (true) then Bool(true)::s else Bool(false)::s )
                                      else  Bool(true)::s
                          | Name(sx) -> ( match (fetch v1 m) with
                                          Bool(x) -> if v2 = false
                                                     then ( if x = (true) then Bool(true)::s else Bool(false)::s )
                                                     else  Bool(true)::s
                                         | _ ->  Error::Bool(v2)::(v1)::s )
                          | _ -> Error::Bool(v2)::(v1)::s    )
  | (v2)::Bool(v1)::s ->( match v2 with
                           Bool(x) -> if x = true
                                      then Bool(true)::s
                                      else (if v1 = true then Bool(true)::s else Bool(false)::s )
                          | Name(xs) -> ( match (fetch v2 m) with
                                          Bool(x) -> if x = true
                                                     then Bool(true)::s
                                                     else (if v1 = true then Bool(true)::s else Bool(false)::s )
                                          | _ ->  Error::(v2)::Bool(v1)::s )
                          | _ -> Error::(v2)::Bool(v1)::s                         )
  | v2::v1::s -> ( match (v2,v1) with
                   (Name(xs),Name(ys))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                          (Bool(x), Bool(y)) -> if x = true
                                                                then Bool(true)::s
                                                                else (if y = true then Bool(true)::s else Bool(false)::s)
                                         | _ -> Error::v2::v1::s            )
                    | _ -> Error::v2::v1::s      )
  | _ -> Error::s

(***)
let notF (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | v::s -> match v with
              Name(xs) -> ( match (fetch v m) with
                            Bool(x) -> if x = (true)
                                       then Bool(false)::s
                                       else Bool(true)::s
                           | _ -> Error::v::s )
              | Bool(x) -> if x = (true)
                           then Bool(false)::s
                           else Bool(true)::s
             | _ -> Error::v::s

(***)
let equal (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | v1::[] -> Error::v1::[]

  | Int(v2)::(v1)::s ->( match v1 with
                          Int(x) -> if v2 = x
                                    then Bool(true)::s
                                    else Bool(false)::s
                         | Name(xs) -> ( match (fetch v1 m) with
                                         Int(x) -> if v2 = x
                                                   then Bool(true)::s
                                                   else Bool(false)::s
                                         | _ ->  Error::Int(v2)::(v1)::s)
                         | _ -> Error::Int(v2)::(v1)::s )

  | (v2)::Int(v1)::s ->( match v2 with
                          Int(x) -> if x = v1
                                    then Bool(true)::s
                                    else Bool(false)::s
                         | Name(xs) -> ( match (fetch v2 m) with
                                         Int(x) -> if x = v1
                                                   then Bool(true)::s
                                                   else Bool(false)::s
                                        | _ ->  Error::(v2)::Int(v1)::s   )
                         | _ -> Error::(v2)::Int(v1)::s )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(xs),Name(sy))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                           (Int(x), Int(y)) -> if x = y
                                                               then Bool(true)::s
                                                               else Bool(false)::s
                                          | _ -> Error::v2::v1::s            )
                   | _ -> Error::v2::v1::s      )


  | _ -> Error::s

(***)
let lessThan (s:const list) (m:(const * const) list) : const list=
  match s with
    [] -> Error::[]
  | v1::[] -> Error::v1::[]
  | Int(v2)::(v1)::s ->( match v1 with
                          Int(x) -> if v2 > x
                                    then Bool(true)::s
                                    else Bool(false)::s
                         | Name(xs) -> ( match (fetch v1 m) with
                                         Int(x) -> if v2 > x
                                                   then Bool(true)::s
                                                   else Bool(false)::s
                                        | _ ->  Error::Int(v2)::(v1)::s)
                         | _ -> Error::Int(v2)::(v1)::s )

  | (v2)::Int(v1)::s ->( match v2 with
                          Int(x) -> if x > v1
                                    then Bool(true)::s
                                    else Bool(false)::s
                         | Name(xs) -> ( match (fetch v2 m) with
                                         Int(x) -> if x > v1
                                                   then Bool(true)::s
                                                   else Bool(false)::s
                                        | _ ->  Error::(v2)::Int(v1)::s   )
                         | _ -> Error::(v2)::Int(v1)::s )
  | v2::v1::s -> ( match (v2,v1) with
                    (Name(sx),Name(sy))-> ( match ((fetch v2 m), (fetch v1 m)) with
                                           (Int(x), Int(y)) -> if x > y
                                                               then Bool(true)::s
                                                               else Bool(false)::s
                                          | _ -> Error::v2::v1::s            )
                   | _ -> Error::v2::v1::s      )

  | _ -> Error::s

(***)
let callIf (s:const list) (m:(const * const) list): const list=
  match s with
    [] -> Error::[]
  | v1::[] -> Error::v1::[]
  | v2::v1::[] -> Error::v2::v1::[]
  | v3::v2::Bool(v1)::s -> if v1 = true
                           then v3::s
                           else v2::s
  | v3::v2::v1::s ->( match v1 with
                       Name(xs)-> ( match (fetch v1 m) with
                                    Bool(b) -> if (b = true) then v3::s else v2::s
                                   | _ -> Error::v3::v2::v1::s        )
                      | Bool(x) -> if x = true
                                   then v3::s
                                   else v2::s
                      | _ -> Error::v3::v2::v1::s               )
  | _ -> Error::s


(***)
(* insert a binding from key k to value v in association list d, if entry already located, replace value *)
let insert (name:const) (value:const) (m:(const * const) list) : (const * const) list =
  (name,value)::m


(***)
let returnBind ( x: const list * (const*const) list ) : const list =
  match x with
    (sL,m) -> sL

let returnMemory ( x: const list * (const*const) list ) : (const*const) list =
  match x with
    (sL,m) -> m

let bind (s:const list) (m:(const * const) list): const list * (const * const) list =
  match (s, m) with
    ([], m) -> (Error::[], m)
  | (v1::[], m) -> (Error::v1::[], m)
  | (v1::(x1)::s, m) -> ( match x1 with
                           Name(x)-> if v1 = Error
                                     then (Error::v1::(x1)::s,m)
                                     else (Unit::s, (insert x1 v1 m ) )
                           | _ -> (Error::v1::(x1)::s, m)       )
  | _ -> (Error::s, m)


(***)
let returnRemainingCommands ( x: command list * const list) : command list =
  match x with
    (cL,sL) -> cL

let returnStack ( x: command list * const list) : const list =
  match x with
    (cL,sL) -> sL




(***************************************************************************************************************)
(*************************************PART 3 FUNCTIONS**********************************************************)



(***************************************************************************************************************)
(******************************FUNCTIONS NEEDED TO PARSE COMMANDS APPROPRIATELY*********************************)

let checkIntStr s =
  try int_of_string s |> ignore; true
  with Failure _ -> false
                      (*Parsing Function to convert the stringlist from loop_read into a commandlist *)
let rec  stringToCommandList (string_Com_List: string list) : (command list) =
  match string_Com_List with
    [] -> []
  | hd::rest ->
    let representation = String.split_on_char ' ' hd in
    match representation with
      "pushi"::tl::[] ->  if (checkIntStr tl) = true then Pushi(Int(int_of_string tl))::(stringToCommandList rest) else Pushi(Error)::(stringToCommandList rest)
    | "pushb"::tl::[] -> if tl = ":false:"
                         then Pushb(Bool(false))::(stringToCommandList rest)
                         else (if tl = ":true:" then Pushb(Bool(true))::(stringToCommandList rest) else Pushb(Error)::(stringToCommandList rest) )
    | "pushs"::tl -> Pushs(String((let x = (String.concat (String.make 1 ' ') (String.split_on_char ' ' hd)) in let len = (String.length x) - 6 in String.sub x 6 len)))::(stringToCommandList rest)
    | "pushn"::tl::[] -> Pushn(Name(tl))::(stringToCommandList rest)
    | "push"::tl::[] -> if tl = ":unit:"
                        then Push(Unit)::(stringToCommandList rest)
                        else Push(Error)::(stringToCommandList rest)
    | "pop"::tl -> Pop::(stringToCommandList rest)
    | "add"::tl -> Add::(stringToCommandList rest)
    | "sub"::tl -> Sub::(stringToCommandList rest)
    | "mul"::tl -> Mul::(stringToCommandList rest)
    | "div"::tl -> Div::(stringToCommandList rest)
    | "rem"::tl -> Rem::(stringToCommandList rest)
    | "neg"::tl -> Neg::(stringToCommandList rest)
    | "swap"::tl -> Swap::(stringToCommandList rest)
    | "quit"::tl -> Quit::(stringToCommandList rest)
    (*Part 2 *)
    | "cat"::tl -> Cat::(stringToCommandList rest)
    | "and"::tl -> And::(stringToCommandList rest)
    | "or"::tl -> Or::(stringToCommandList rest)
    | "not"::tl -> Not::(stringToCommandList rest)
    | "lessThan"::tl -> LessThan::(stringToCommandList rest)
    | "equal"::tl -> Equal::(stringToCommandList rest)
    | "if"::tl -> If::(stringToCommandList rest)
    | "bind"::tl -> Bind::(stringToCommandList rest)
    | "let"::tl -> Let::(stringToCommandList rest)
    | "end"::tl -> End::(stringToCommandList rest)
    | _ -> []



(*Recursive function that executes all commands up until the End command in our scope and returns the remainder of the command list and the top const added to the head of our orginal stack *)
let rec letFunction (commandList:command list) (xs:const list) (originalS:const list) (m:(const * const) list) : (command list * const list) =
  match (commandList, xs, originalS, m ) with
    ([], hd::xs, originalS, m ) -> print_endline("letFunctionEmptyCL returned"); ([], hd::originalS)
  | (End::tl, hd::xs, originalS, m ) -> print_endline("letFunction returned"); (tl, hd::originalS)
  | (End::tl, [], originalS, m ) -> print_endline("letFunction[] returned"); (tl, originalS)
  | ((Pushi(const) )::tl, xs, originalS, m ) ->  (letFunction tl (pushi const xs) originalS m )
  | ((Pushb(const) )::tl, xs, originalS, m ) -> (letFunction tl (pushb const xs) originalS m )
  | ((Pushs(const) )::tl, xs, originalS, m ) -> (letFunction tl (pushs const xs) originalS m )
  | ((Pushn(const) )::tl, xs, originalS, m ) -> (letFunction tl (pushn const xs) originalS m )
  | ((Push(const) )::tl, xs, originalS, m )-> (letFunction tl (push const xs) originalS m )
  | (Pop::tl, xs, originalS, m ) -> (letFunction tl (popF xs) originalS m )
  | (Add::tl, xs, originalS, m ) -> (letFunction tl (addF xs m) originalS m )
  | (Sub::tl, xs, originalS, m ) -> (letFunction tl (subF xs m) originalS m )
  | (Mul::tl, xs, originalS, m ) ->  (letFunction tl (mulF xs m) originalS m )
  | (Div::tl, xs, originalS, m ) -> (letFunction tl (divF xs m) originalS m )
  | (Rem::tl, xs, originalS, m ) -> (letFunction tl (remF xs m) originalS m )
  | (Neg::tl, xs, originalS, m ) -> (letFunction tl (negF xs m) originalS m )
  | (Swap::tl, xs, originalS, m ) -> (letFunction tl (swapF xs) originalS m )
  | (Quit::tl, xs, originalS, m ) -> (letFunction tl (quitF xs) originalS m )
  | (Cat::tl, xs, originalS, m ) ->  (letFunction tl (cat xs m) originalS m )
  | (And::tl, xs, originalS, m ) -> (letFunction tl (andF xs m) originalS m )
  | (Or::tl, xs, originalS, m ) -> (letFunction tl (orF xs m) originalS m )
  | (Not::tl, xs, originalS, m ) -> (letFunction tl (notF xs m) originalS m )
  | (LessThan::tl, xs, originalS, m ) -> (letFunction tl (lessThan xs m) originalS m )
  | (Equal::tl, xs, originalS, m ) -> (letFunction tl (equal xs m) originalS m )
  | (If::tl, xs, originalS, m ) -> (letFunction tl (callIf xs m) originalS m )
  | (Bind::tl, xs, originalS, m ) -> (letFunction tl (returnBind (bind xs m)) originalS (returnMemory (bind xs m)) )
  | (Let::tl, xs, originalS, m ) ->  (letFunction (returnRemainingCommands (letFunction tl [] xs m)) [] (returnStack (letFunction tl [] xs m)) m )

let rec printMem (m:(const * const) list) =
  match m with
    [] -> []
  | hd::tl -> print_endline("mem entry"); printMem tl
(******************************************************************************************)
let interpreter ( (input : string), (output : string )) : unit =
  (* Here we open an input channel for first argument, input,
     and bind it to a variable ic so that we can refer it
     later in loop_read function. *)
  let ic = open_in input in

  (* Use the second argument as file name to open an output channel,
     and bind it to variable oc for later reference. *)
  let oc = open_out output in

  (* Helper function: file input function. It reads file line by line
      and return the result as a list of string.  *)
  let rec loop_read acc =
    try

      let l = input_line ic in loop_read (l::acc)
    with

    | End_of_file -> List.rev acc
  in

  (*This is a list of strings containing commands read from the input file. *)
  let string_Com_List = loop_read [] in

  (*Function to convert a our entire string list containing commands from the input file
    into a list storing command elements*)
  let commandList = stringToCommandList string_Com_List in


  (*Recursive function that executes all commands in our commandList onto our stack*)
  let rec runThruCommandList (commandList:command list) (s:const list) (m:(const * const) list) : const list =
    match (commandList, s, m) with
      ([], s, m) -> printMem m; s
    | ((Pushi(const) )::tl, s, m) ->print_endline("pushi active");  (runThruCommandList tl (pushi const s) m)
    | ((Pushb(const) )::tl, s, m) ->print_endline("pushb active");  (runThruCommandList tl (pushb const s) m)
    | ((Pushs(const) )::tl, s, m) ->print_endline("pushs active");  (runThruCommandList tl (pushs const s) m)
    | ((Pushn(const) )::tl, s, m) ->print_endline("pushn active"); (runThruCommandList tl (pushn const s) m)
    | ((Push(const) )::tl, s, m)->print_endline("push active"); (runThruCommandList tl (push const s) m)
    | (Pop::tl, s, m) ->print_endline("pop active"); (runThruCommandList tl (popF s) m)
    | (Add::tl, s, m) ->print_endline("add active");  (runThruCommandList tl (addF s m) m)
    | (Sub::tl, s, m) ->print_endline("sub active"); (runThruCommandList tl (subF s m) m)
    | (Mul::tl, s, m) ->print_endline("mul active");  (runThruCommandList tl (mulF s m) m)
    | (Div::tl, s, m) ->print_endline("div active"); (runThruCommandList tl (divF s m) m)
    | (Rem::tl, s, m) ->print_endline("rem active"); (runThruCommandList tl (remF s m) m)
    | (Neg::tl, s, m) ->print_endline("neg active"); (runThruCommandList tl (negF s m) m)
    | (Swap::tl, s, m) ->print_endline("swap active"); (runThruCommandList tl (swapF s) m)
    | (Quit::tl, s, m) ->print_endline("quit active"); (runThruCommandList tl (quitF s) m)
    | (Cat::tl, s, m) ->print_endline("cat active");  (runThruCommandList tl (cat s m)  m)
    | (And::tl, s, m) ->print_endline("and active"); (runThruCommandList tl (andF s m) m)
    | (Or::tl, s, m) ->print_endline("or active");  (runThruCommandList tl (orF s m) m)
    | (Not::tl, s, m) ->print_endline("not active"); (runThruCommandList tl (notF s m) m)
    | (LessThan::tl, s, m) ->print_endline("lessThan active"); (runThruCommandList tl (lessThan s m) m)
    | (Equal::tl, s, m) ->print_endline("equal active"); (runThruCommandList tl (equal s m) m)
    | (If::tl, s, m) ->print_endline("if active"); (runThruCommandList tl (callIf s m) m)
    | (Bind::tl, s, m) ->print_endline("bind active"); (runThruCommandList tl (returnBind (bind s m)) (returnMemory (bind s m)))
    | (Let::tl, s, m) -> print_endline("let end active"); (runThruCommandList (returnRemainingCommands (letFunction tl [] s m)) (returnStack (letFunction tl [] s m)) m)
    | (End::tl, s, m) -> print_endline("end active"); (runThruCommandList tl s m)
  in


  let s(*s here is a const list being returned from runThruCommandList*) =
    runThruCommandList commandList [] [] in

  let stringStack(*stringStack is the stringlist conversion of our const list 's' that need to be outputted rom each command*)
    = print_endline("stringStack iniatated"); stackValsToString s
  in

  printStackToOutput oc stringStack;
  print_endline("printStackToOutput");
  close_in ic;
  close_out oc;
;;
interpreter ("sample.txt","out.txt")
