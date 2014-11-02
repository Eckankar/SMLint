(*
SeqDec
    [MarkDec
       (FunDec
          ([MarkFb
              (Fb
                 ([Clause
                     {exp=MarkExp
                            (FlatAppExp
                               [{fixity=NONE,
                                 item=MarkExp
                                        (ListExp
                                           [FlatAppExp
                                              [{fixity=SOME
                                                         (SYMBOL (0wx350,"x")),
                                                item=MarkExp
                                                       (VarExp
                                                          [SYMBOL (0wx348,"x")],
                                                        (18,19)),
                                                region=(18,19)}]],(17,20)),
                                 region=(17,20)},
                                {fixity=SOME (SYMBOL (0wx318,"@")),
                                 item=MarkExp
                                        (VarExp [SYMBOL (0wx310,"@")],(21,22)),
                                 region=(21,22)},
                                {fixity=SOME (SYMBOL (0wx6F93,"xs")),
                                 item=MarkExp
                                        (VarExp [SYMBOL (0wx6F8B,"xs")],
                                         (23,25)),region=(23,25)}],(17,25)),
                      pats=[{fixity=SOME (SYMBOL (0wxE171C,"foo")),
                             item=MarkPat
                                    (VarPat [SYMBOL (0wxE1714,"foo")],(6,9)),
                             region=(6,9)},
                            {fixity=SOME (SYMBOL (0wx350,"x")),
                             item=MarkPat
                                    (VarPat [SYMBOL (0wx348,"x")],(10,11)),
                             region=(10,11)},
                            {fixity=SOME (SYMBOL (0wx6F93,"xs")),
                             item=MarkPat
                                    (VarPat [SYMBOL (0wx6F8B,"xs")],(12,14)),
                             region=(12,14)}],resultty=NONE}],false),(6,25))],
           []),(2,25))] : Ast.dec
*)


local
  open Ast
  exception UnhandledExp of exp

  fun concatMap f = List.concat o map f

  fun exPatFixItem {item,fixity,region} = item

  fun exPatRule (Rule {pat, exp}) = pat :: exPatExp exp

  and exPatExp (MarkExp (exp, _)) = exPatExp exp
    | exPatExp (CaseExp {expr, rules}) =
        exPatExp expr @ concatMap exPatRule rules
    | exPatExp (IntExp _) = []
    | exPatExp (VarExp _) = []
    | exPatExp (FlatAppExp efis) = concatMap (exPatExp o exPatFixItem) efis
    | exPatExp e = raise UnhandledExp e


  and exPatClause (Clause {pats, exp, ...}) =
    map exPatFixItem pats @ exPatExp exp

  and exPatFB (MarkFb (fb, _)) = exPatFB fb
    | exPatFB (Fb (clauses, _) ) = concatMap exPatClause clauses

  and exPat (SeqDec decs) = concatMap exPat decs
    | exPat (MarkDec (dec, _)) = exPat dec
    | exPat (FunDec (fbs, _)) = concatMap exPatFB fbs
    | exPat x = raise Fail "Unhandled Dec"
in
  val extractPatterns = exPat
end
