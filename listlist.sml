(* Catches [x :: xs] *)
local
  open Ast Symbol
  exception NoGood

  fun concatMap f = List.concat o map f

  fun reportPat (MarkPat (p, pos)) =
        (reportPat p handle NoGood => [("Cons inside list pattern", pos)])
    | reportPat (ListPat [
      FlatAppPat [_, { item = VarPat [SYMBOL (_, "::")], ... }, _]
          ]) = raise NoGood
    | reportPat (FlatAppPat [{ item = p , ... }]) = reportPat p
    | reportPat (VarPat _) = []
    | reportPat (_) = []
in
  fun listListRule ast =
    let val pats = map stripExceptFirst (extractPatterns ast)
    in concatMap reportPat pats end
end
