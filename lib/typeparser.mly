%{
open Type
%}

%start ty
%type <ty> ty

%%
ty:
  | t = arrow "<eof>" { t }

arrow:
  | t = prim { t }
  | t1 = prim "->" t2 = arrow { TLam (t1, t2) }

prim:
  | x = "<id>"        { match x with
                        | "int" -> TInt
                        | "bool" -> TBool
                        | _ -> failwith @@ "unsupported type " ^ x
                      }
  | x = "<tid>"       { TVar x }
  | "(" t = arrow ")" { t }
