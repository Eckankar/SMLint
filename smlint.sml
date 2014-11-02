;use "extract.sml";
;use "strip.sml";
;use "listlist.sml";
local
val _ = Control.Print.printDepth := 100;

val filename = "test.sml"
val is = TextIO.openIn filename
val source = Source.newSource (filename, is, false, ErrorMsg.defaultConsumer ())
val ast = SmlFile.parse source
val _ = Source.closeSource source
val _ = TextIO.closeIn is
val is = TextIO.openIn filename
val srcLines = String.tokens (fn c => c = #"\n") (TextIO.inputAll is)
val _ = TextIO.closeIn is

fun lineOf n (l::ls) = if n < size l then l
                                     else lineOf (n - size l) ls
  | lineOf _ _ = raise Fail "lineOf"

val rep = listListRule ast;
val pats = map stripExceptFirst (extractPatterns ast)

in
val _ = print (String.concatWith "\n" (map
    (fn (m, (s, e)) => String.concat [
      m, " near:\n\n    ", lineOf s srcLines, "\n\n"])  rep))
end
