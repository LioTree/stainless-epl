import scala.collection.immutable.ListMap

object Test4 {
  /* Exercise 20 */
  def election(votes: List[String]): ListMap[String, Int] =
    val listmap = ListMap[String, Int]();
    votes match {
      case Nil => ListMap[String, Int]()
      case x :: xs => if (listmap.contains(x)) {
        listmap.updated(x, listmap(x) + 1) ++ election(xs)
      } else {
        listmap.updated(x, listmap(x) + 1) ++ election(xs)
      }
    }
}

object Test9 {
  def election(votes: List[String]): ListMap[String, Int] = votes match {
    case Nil => scala.collection.immutable.ListMap[String, Int]();
    case x :: y => if (election(y) contains x) {
      election(y).updated(x, election(y)(x) + 1)
    } else {
      election(y).updated(x, 1)
    }
  }
}

object Test21 {
  def election(votes: List[String]): ListMap[String, Int] = {
    val results = ListMap[String, Int]();
    electionRec(votes, results)
  }

  def electionRec(votes: List[String], results: ListMap[String, Int]): ListMap[String, Int] = votes match {
    case Nil => ListMap[String, Int]()
    case v :: Nil => update(v, results)
    case v :: vs => electionRec(vs, update(v, results))
  }

  def update(vote: String, results: ListMap[String, Int]): ListMap[String, Int] =
    if (results contains vote) {
      results + (vote -> (results(vote) + 1))
    } else {
      results + (vote -> 1)
    };
}

object Test7 {
  import scala.annotation.tailrec

  def election(votes: List[String]): ListMap[String, Int] = {
    @tailrec
    def mapBuilder(votes: List[String], map: ListMap[String, Int]): ListMap[String, Int] = votes match {
      case Nil => map
      case vote :: votes => map.get(vote) match {
        case Some(count) =>
          mapBuilder(votes, map.updated(vote, count + 1))
        case None => mapBuilder(votes, map.updated(vote, 1))
      }
    }

    mapBuilder(votes, ListMap[String, Int]())
  }
}