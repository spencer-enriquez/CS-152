package Recognizers

/*
 * Problem 10: Recognizers -- INCOMPLETE
 * Follow not implemented correctly
 */
trait Recognizers {
  // matches(s) = s
  def matches(s: String): String => Boolean = {
    def r(s1: String) = s1.trim.equalsIgnoreCase(s)
    r _
  }

  // opt(r) = r?
  def opt(r: String => Boolean): String => Boolean = {
    def helper(s: String) = r(s) || s.isEmpty
    helper _
  }

  // pipe(r1, r2) = r1 | r2
  def pipe(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
    def r(s: String) = r1(s) || r2(s)
    r _
  }

  // follows(r1, r2) = r1 ~ r2
  def follows(r1: String => Boolean, r2: String => Boolean) : String => Boolean = {
    def helper(s: String): Boolean = {
      var r1Check = (false, 0, 0)
      var r2Check = (false, 0, 0)
      for (i <- 0 until s.size; j <- i until s.size if !r1Check._1 && !r2Check._1) {
        if (r1(s.substring(i, j)) && !r1Check._1) {r1Check = (true, i , j)}
        if (r2(s.substring(i, j)) && !r2Check._1) {r2Check = (true, i , j)}
      }
      
      r1Check._1 && r2Check._1  && (r1Check._2 < r2Check._2) && (r1Check._3 < r2Check._3)
    }
    helper _
  }

  // rep(r) = r*
  def rep(r: String => Boolean): String => Boolean = {
    //r1 uses recursion and iteration!
    def r1(s: String): Boolean = {
      var result = false
      if (s == "") result = true
      else {
        for(i <- 0 to s.length if !result)
          result = r(s.substring(0, i)) && r1(s.substring(i))
      }
      result
    }
    r1 _
  }
}