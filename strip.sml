(*
MarkPat
     (ListPat
        [MarkPat
           (FlatAppPat
              [{fixity=SOME (SYMBOL (0wx350,"x")),
                item=MarkPat (VarPat [SYMBOL (0wx348,"x")],(35,36)),
                region=(35,36)},
               {fixity=SOME (SYMBOL (0wx675C,"::")),
                item=MarkPat (VarPat [SYMBOL (0wx6754,"::")],(36,38)),
                region=(36,38)},
               {fixity=SOME (SYMBOL (0wx6F93,"xs")),
                item=MarkPat (VarPat [SYMBOL (0wx6F8B,"xs")],(38,40)),
                region=(38,40)}],(35,40))],(34,41))]
*)

local
  open Ast

  fun stripPatFit {item, fixity, region, ...} =
    { item = stripPat item, fixity = fixity, region = region }

  and stripPat (MarkPat (p,_)) = stripPat p
    | stripPat (ListPat ps) = ListPat (map stripPat ps)
    | stripPat (FlatAppPat fits) = FlatAppPat (map stripPatFit fits)
    | stripPat (p as (VarPat _)) = p
    | stripPat x = raise Fail "stripPat"
in
  val stripPats = stripPat

  fun stripExceptFirst (MarkPat (p, r)) = MarkPat (stripPats p, r)
    | stripExceptFirst _ = raise Fail "stripExceptFirst"
end
