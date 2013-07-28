package dhg.nlp.data

object Clean {

  object CleanTok {
    def unapply(t: String): Option[String] = Some(t match {
      case "|" => "-VERTBAR-"
      case _ => t.replace("\\/", "/")
    })
  }

  object CleanPos {
    def unapply(p: String): Option[String] = Some(p match {
      case _ if p.startsWith("-") => p
      case _ if p.startsWith("NNP") => "NNP"
      case _ => p.take(2)
    })
  }

  val HasLetter = ".*[A-Za-z].*".r
  val LineRe = ".*----.*".r
  val Punctuation = (w: String) => Set(".", ",", "``", "''", "'", "`", "--", ":", ";", "(", ")", "[", "]", "{", "}", "-RRB-", "-LRB-", "?", "!", "-RCB-", "-LCB-", "-RSB-", "-LSB-", "...", "-", "_", "-VERTBAR-")(w.toUpperCase)

  val ValidLemma = (w: String) =>
    !Stopwords(w.toLowerCase) &&
      HasLetter.pattern.matcher(w).matches &&
      !Punctuation(w) &&
      !LineRe.pattern.matcher(w).matches

  val ValidPos = (p: String) => !Set(".", ":", ",", "``", "''", "-LRB-", "-RRB-", "#", "CC", "CD", "DT", "EX", "IN", "LS", "MD", "PDT", "POS", "PRP", "PRP$", "RP", "SYM", "TO", "UH", "WDT", "WP", "WP$", "WRB").map(_.take(2)).contains(p.take(2))

}
