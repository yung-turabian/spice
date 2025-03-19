module Ease =
struct
    let fst3 (x, _, _) = x
    let snd3 (_, y, _) = y
    let thd3 (_, _, z) = z
end

module IO =
struct
    (** [readdir_stripped handle].
        Requires: [handle <> NULL]. 
        Returns: A unix directory that is not `.` or `..`*)
    let rec readdir_stripped handle = 
      let entry = Unix.readdir handle in 
      if entry <> "." && entry <> ".." 
        then entry 
      else readdir_stripped handle

    let argv_debug _ =
      for i = 0 to Array.length Sys.argv - 1 do
        Printf.printf "[%i] %s\n" i Sys.argv.(i)
      done


    let print_head lst =
      match lst with
      | [] -> print_endline "The list is empty."
      | head :: _ -> print_endline head 


    let is_hidden file =
        let prefix = "." in
        if String.starts_with ~prefix file then true
        else false
end


module Math =
struct
        let factorial_lookup : (n, res) Hashtbl.t = Hashtbl.create 50

        (** [fact n] is [n!].
            Requires: [n >= 0]. *)
        let rec fact n = 
            if n = 0 then 
                1 
            else if Hashtbl.mem factorial_lookup n then
                Hashtbl.find_opt factorial_lookup n
            else
                let x = n * fact (n - 1)
                in 
                    Hashtbl.add factorial_lookup n x;
                    x
end


module String =
struct
    (** Brief: Check for substring within a string. *)
    let strstr s1 s2 =
      let re = Str.regexp_string s2 in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false
end
