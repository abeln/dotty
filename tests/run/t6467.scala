



import collection._



object Test extends dotty.runtime.LegacyApp {

  def compare(s1: String, s2: String): Unit = {
    assert(s1 == s2, s1 + "\nvs.\n" + s2)
  }

  import java.lang.StringBuffer

  def appendInt(sb1: StringBuffer|Null, v: Int): StringBuffer = sb1.nn.append(v).nn
  def appendBuff(sb1: StringBuffer|Null, sb2: StringBuffer): StringBuffer = sb1.nn.append(sb2).nn

  compare(List(1, 2, 3, 4).aggregate(new StringBuffer)(appendInt, appendBuff).toString, "1234")
  compare(List(1, 2, 3, 4).par.aggregate(new StringBuffer)(appendInt, appendBuff).toString, "1234")
  compare(Seq(0 until 100: _*).aggregate(new StringBuffer)(appendInt, appendBuff).toString, (0 until 100).mkString)
  compare(Seq(0 until 100: _*).par.aggregate(new StringBuffer)(appendInt, appendBuff).toString, (0 until 100).mkString)

}
