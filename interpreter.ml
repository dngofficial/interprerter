type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT

type command = ADD | SUB | MUL | DIV | PUSH of stackValue | PRINTLN | SWAP | NEG | POP | TOSTRING | DEBUG | REM | CAT
| AND | OR | NOT | EQUAL | LESSTHAN | IF | BIND | LET | END


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
        let is_float s = try ignore (float_of_string s); true with Failure _ -> false in
        if is_int s then INT (int_of_string s)
        else if is_float s then ERROR
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
        | "let", None -> Some LET
        | "end", None -> Some END


    
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

    (*let print_env env =
        let print_binding (name, value) =
            Printf.printf "%s -> %s\n" name (sv2str value)
        in
        print_endline "Environment:";
        List.iter print_binding env;
        print_endline "----------------"
    in *)

    
    let rec resolve_value v env_stack =
        match v with
        | NAME n -> (
            match env_stack with
            | x :: xs -> (
                match List.assoc_opt n x with
                | Some value -> value
                | None -> resolve_value v xs  (* keep passing NAME n *)
              )
            | [] -> ERROR
          )
    | _ -> v
    in
    
    
    let rec processor cl stack stack_stack env env_stack =
    
        match (cl, stack) with  
        | (PUSH v :: rest, _) -> processor rest (v :: stack) stack_stack env env_stack
        | (ADD :: rest, v1 :: v2 :: restStack) -> (
            match (resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack)) with
            | INT a, INT b -> processor rest (INT (a + b) :: restStack) stack_stack env  env_stack
            | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
          )        
        | (SUB :: rest, v1 :: v2 :: restStack) -> (
            match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
            | INT a, INT b -> processor rest (INT (b - a) :: restStack) stack_stack env env_stack
            | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack)
    
        | MUL :: rest, v1 :: v2 :: restStack -> (
            match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
            | INT a, INT b -> processor rest (INT (a * b) :: restStack) stack_stack env env_stack
            | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
          )


        | DIV :: rest, v1 :: v2 :: restStack -> (
        match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
        | INT 0, INT _ -> processor rest (ERROR :: restStack) stack_stack env env_stack
        | INT a, INT b -> processor rest (INT (b / a) :: restStack) stack_stack env env_stack
        | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
        ) 

        | REM :: rest, v1 :: v2 :: restStack -> (
            match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
        | INT 0, INT _ -> processor rest (ERROR :: restStack) stack_stack env env_stack
        | INT a, INT b -> processor rest (INT (b mod a) :: restStack) stack_stack env env_stack
        | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
        ) 

        | (PRINTLN :: rest, v :: restStack) ->
            file_write oc (sv2str v);
            processor rest restStack stack_stack env env_stack
        | (SWAP :: rest, v1 :: v2 :: restStack) -> processor rest (v2 :: v1 :: restStack) stack_stack env env_stack

        | NEG :: rest, v1 :: restStack -> (
            match resolve_value v1 (env::env_stack) with
        | INT n -> processor rest (INT (-n) :: restStack) stack_stack env env_stack
        | _ -> processor rest (ERROR :: v1 :: restStack) stack_stack env env_stack

        ) 
        | (POP :: rest, _ :: restStack) -> processor rest restStack stack_stack env env_stack
        | (TOSTRING :: rest, v :: restStack) -> processor rest (STRING (sv2str v) :: restStack) stack_stack env env_stack
        | (TOSTRING :: rest, []) -> processor rest (ERROR :: stack) stack_stack env env_stack


        | CAT :: rest, v1 :: v2 :: restStack -> (
            match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
            | STRING a ,STRING b -> processor rest (STRING (b ^ a) :: restStack) stack_stack env env_stack
            | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
          )

        | AND :: rest, v1 :: v2 :: restStack -> (
        match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
        | BOOL a , BOOL b -> processor rest (BOOL (b && a) :: restStack) stack_stack env env_stack
        | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
        )

        | OR :: rest, v1 :: v2 :: restStack -> (
        match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
        | BOOL a , BOOL b -> processor rest (BOOL (b || a) :: restStack) stack_stack env env_stack
        | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
        )

        | NOT :: rest, v1 :: restStack -> (
            match resolve_value v1 (env::env_stack) with
            | BOOL a -> processor rest (BOOL (not a) :: restStack) stack_stack env env_stack
            | _ -> processor rest (ERROR :: v1 :: restStack) stack_stack env env_stack
            )

        | EQUAL :: rest, v1 :: v2 :: restStack -> (
            match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
            | INT a, INT b -> processor rest (BOOL (a = b) :: restStack) stack_stack env env_stack
            | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
            )

        | LESSTHAN :: rest, v1 :: v2 :: restStack -> (
            match resolve_value v1 (env::env_stack), resolve_value v2 (env::env_stack) with
            | INT a, INT b -> processor rest (BOOL (b < a) :: restStack) stack_stack env env_stack
            | _ -> processor rest (ERROR :: v1 :: v2 :: restStack) stack_stack env env_stack
            )

        | IF :: rest, a :: b :: c :: restStack -> (
            match a ,  b , resolve_value c (env::env_stack) with
            | a , b , BOOL c ->  if c then
                processor rest (a :: restStack) stack_stack env env_stack
            else
                processor rest (b :: restStack) stack_stack env env_stack
            | _ -> processor rest (ERROR :: a :: b :: c :: restStack) stack_stack env env_stack
            )
        | (DEBUG :: rest, restStack) -> 
            print_stack restStack;
    
    (* Print each environment in the env_stack *)
    let rec print_all_envs i = function
      | [] -> ()
      | env :: rest ->
          Printf.printf "Environment level %d:\n" i;
          List.iter (fun (name, value) ->
            Printf.printf "  %s -> %s\n" name (sv2str value)
          ) env;
          print_endline "----------------";
          print_all_envs (i + 1) rest
    in

    print_all_envs 0 (env :: env_stack);

    processor rest restStack stack_stack env env_stack
    | (BIND :: rest, a :: NAME b :: restStack) -> (
        (*print_endline (sv2str (resolve_value a (env::env_stack)));*)
        let resolved = resolve_value a (env::env_stack) in
        match resolved with
        | ERROR -> processor rest (ERROR :: a :: NAME b :: restStack) stack_stack env env_stack
        | _ ->  processor rest (UNIT :: restStack) stack_stack ((b, resolved) :: env) env_stack
        )
    | LET :: rest, stack -> processor rest [] (stack :: stack_stack) [] (env :: env_stack)
    | END :: rest, v :: _ -> (
        match (stack_stack, env_stack) with
        | last_stack :: ss_tail, last_env :: es_tail ->
            processor rest (v :: last_stack) ss_tail last_env es_tail
        | _ -> processor rest (ERROR :: stack) stack_stack env env_stack)

    | (_ :: rest, _) -> processor rest (ERROR :: stack) stack_stack env env_stack
    | ([], _) -> stack 
    in
    
    let commands = convert_strings_to_commands strList in
    let _ = processor commands [] [] [] [] in

    close_out oc
;;


interpreter ("input1.txt", "output.txt")