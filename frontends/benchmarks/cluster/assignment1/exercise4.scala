object Test4 {
  def suffix(n: Int): String =
    val m = n % 100;
    val z = n % 10;
    if (m > 10 && m < 20) {"th"}
    else { z match{
      case 1 => "st"
      case 2 => "nd"
      case 3 => "rd"
      case _ => "th"
    }
    }
}

object Test2 {
  def suffix(n: Int): String = {
    if (n % 10 == 1) {
      return "st";
    } else if (n % 10 == 2) {
      return "nd";
    } else if (n % 10 == 3) {
      return "rd";
    } else {
      return "th"}
  }
}

object Test15 {
  def suffix(n: Int): String =
    n match
    {
      case n if (n % 100 >= 11) && (n % 100 <= 13) => "th"
      case n if (n % 10 == 1) => "st"
      case n if (n % 10 == 2) => "nd"
      case n if (n % 10 == 3) => "rd"
      case n => "th"
    }
}