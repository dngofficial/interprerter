type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT

type command = ADD | SUB | MUL | DIV | PUSH of stackValue | PRINTLN | SWAP | NEG | POP | TOSTRING | DEBUG | REM | CAT
| AND | OR | NOT | EQUAL | LESSTHAN | IF | BIND

let interpreter (input, output) : unit =
    let ic = open_in input in
    let oc = open_out output in
    
    let rec loop_read acc =
        try
            let l = String.trim (input_line ic) in loop_read (l :: acc)
        with End_of_file -> List.rev acc
    in
    
    let strList = loop_read [] in

    let split_first_space s =
        match String.index_opt s ' ' with
        | Some idx ->
            let first_part = String.sub s 0 idx in
            let second_part = String.sub s (idx + 1) (String.length s - idx - 1) in
            (first_part, Some second_part)
        | None -> (s, None)
    in
    
    let file_write oc str_val = Printf.fprintf oc "%s\n" str_val in
    
    let str2sv s =
        let is_int s = try ignore (int_of_string s); true with Failure _ -> false in
        if is_int s then INT (int_of_string s)
        else if s = ":true:" then BOOL true
        else if s = ":false:" then BOOL false
        else if s = ":error:" then ERROR
        else if s = ":unit:" then UNIT
        else if String.length s > 1 && s.[0] = '"' && s.[String.length s - 1] = '"' then
            STRING (String.sub s 1 (String.length s - 2))
        else NAME s
    in
    
    let str2com s =
        let (cmd, arg) = split_first_space s in
        match cmd, arg with
        | "push", Some n -> Some (PUSH (str2sv n))
        | "add", None -> Some ADD
        | "sub", None -> Some SUB
        | "mul", None -> Some MUL
        | "div", None -> Some DIV
        | "println", None -> Some PRINTLN
        | "swap", None -> Some SWAP
        | "neg", None -> Some NEG
        | "pop", None -> Some POP
        | "tostring", None -> Some TOSTRING
        | "debug", None -> Some DEBUG
        | "rem", None -> Some REM
        | "cat", None -> Some CAT
        | "and", None -> Some AND
        | "or", None -> Some OR
        | "equal", None -> Some EQUAL
        | "lessThan", None -> Some LESSTHAN
        | "if", None -> Some IF
        | "not", None -> Some NOT
        | "bind", None -> Some BIND
    
        | _ -> None
    in
    
    let convert_strings_to_commands str_list =
        List.filter_map str2com str_list
    in

    
    
    let sv2str s = 
        match s with
        | INT n -> string_of_int n
        | BOOL true -> ":true:"
        | BOOL false -> ":false:"
        | ERROR -> ":error:"
        | UNIT -> ":unit:"
        | STRING s -> s
        | NAME n -> n
    in

    let print_stack stack =
        let rec print_items = function
          | [] -> ()
          | v :: rest ->
              Printf.printf "| %-10s |\n" (sv2str v);
              print_items rest
        in
        print_endline "+------------+";
        print_items stack;
        print_endline "+------------+"
    in

    let print_env env =
        let print_binding (name, value) =
            Printf.printf "%s -> %s\n" name (sv2str value)
        in
        print_endline "Environment:";
        List.iter print_binding env;
        print_endline "----------------"
    in
    
    
    let rec processor cl stack env =
    
        match (cl, stack) with
        | (ADD :: rest, INT a :: INT b :: restStack) -> processor rest  (INT (a + b)::restStack) env
        | (SUB :: rest, INT a :: INT b :: restStack) -> processor rest (INT (b - a) :: restStack) env
        | (MUL :: rest, INT a :: INT b :: restStack) -> processor rest (INT (a * b) :: restStack) env
        | (DIV :: rest, INT 0 :: INT _ :: restStack) -> processor rest (ERROR :: stack)  env
        | (DIV :: rest, INT a :: INT b :: restStack) -> processor rest (INT (b / a) :: restStack) env
        | (REM :: rest, INT 0 :: INT _ :: restStack) -> processor rest (ERROR :: stack) env
        | (REM :: rest, INT a :: INT b :: restStack) -> processor rest (INT (b mod a) :: restStack) env
        | (PUSH v :: rest, _) -> processor rest (v :: stack) env
        | (PRINTLN :: rest, v :: restStack) ->
            file_write oc (sv2str v);
            processor rest restStack  env
        | (SWAP :: rest, v1 :: v2 :: restStack) -> processor rest (v2 :: v1 :: restStack) env
        | (NEG :: rest, INT n :: restStack) -> processor rest (INT (-n) :: restStack) env
        | (NEG :: rest, n :: restStack) -> processor rest (ERROR :: n :: restStack) env
        | (POP :: rest, _ :: restStack) -> processor rest restStack env
        | (TOSTRING :: rest, v :: restStack) -> processor rest (STRING (sv2str v) :: restStack) env
        | (TOSTRING :: rest, []) -> processor rest (ERROR :: stack) env
        | (CAT :: rest, STRING a :: STRING b :: restStack) -> processor rest (STRING (b ^ a) :: restStack) env
        | (AND :: rest, BOOL a :: BOOL b :: restStack) -> processor rest (BOOL (b && a) :: restStack) env
        | (OR :: rest, BOOL a :: BOOL b :: restStack) -> processor rest (BOOL (b || a) :: restStack) env
        | (NOT :: rest, BOOL a :: restStack) -> processor rest (BOOL (not a) :: restStack) env
        | (EQUAL :: rest, INT a :: INT b :: restStack) -> processor rest (BOOL (a = b) :: restStack) env
        | (LESSTHAN :: rest, INT a :: INT b :: restStack) -> processor rest (BOOL (b < a) :: restStack) env
        | (IF :: rest, a :: b :: BOOL c :: restStack) -> 
            if c then
                processor rest (a :: stack) env
            else
                processor rest (b :: stack) env
        | (DEBUG :: rest, stack) -> 
            print_stack stack;  (* Print the entire stack *)
            print_env env;      
            processor rest stack env
        | (BIND :: rest, a :: NAME b :: restStack) -> 
            (match a with
            | NAME x -> (match (List.assoc_opt x env) with
                         | Some z -> processor rest (UNIT :: restStack) ((b, a) :: env)
                         | None -> processor rest (ERROR :: a :: NAME b :: restStack) env)
            | _ ->  processor rest (UNIT :: restStack) ((b, a) :: env))
        | (_ :: rest, _) -> processor rest (ERROR :: stack) env

        | ([], _) -> stack 
    in
    
    let commands = convert_strings_to_commands strList in
    let _ = processor commands [] [] in

    close_out oc
;;


interpreter ("input1.txt", "output.txt")