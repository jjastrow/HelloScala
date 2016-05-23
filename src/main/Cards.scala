import scala.util.Random

object Cards {
  def main(args: Array[String]) = {

    def newDeck: List[Card] = {
      val deck = for {                                          // example of FOR Comprehension (with the 'yield')
        suit <- Suits()
        value <- 1 to 13                                        // defines a Range which is a Seq of Ints
      } yield Card(value, suit)                                 // yield with return results

      deck.toList
    }

    def shuffle(d: List[Card]) = Random.shuffle(d)              // shuffle the deck
    var brandNewDeck = newDeck
    var shuffledDeck = shuffle(newDeck)
    def draw(amount: Int): List[Card] = {
      val (ret, remainder) = shuffledDeck.splitAt(amount)
      shuffledDeck = remainder
      ret
    }


    println(s"Poker hand: ${
      draw(5).map(c => c)
    }")                          // show various hands such as Poker is 5 cards
    println("\n\nSHOW:")
    println(s"Brand new deck = ${newDeck}")
    println("\n\nSHOW:")
    println(s"After shuffle = ${shuffle(newDeck)}")
    println("\n AND, with Name of Card:\n")
    newDeck.foreach(c => println(s"${c.name} of ${c.suit}"))       // print each card like: "the Jack of Spades"
    println("\n\nSHOW:")
    println(s"A Full House:\n ${ FullHouse }")              // show what a Full House looks like

  }
}

case class Card(value: Int, suit: String) {
  def name = {
    value match {
      case 1 => "Ace"
      case 11 => "Jack"
      case 12 => "Queen"
      case 13 => "King"
      case _ => value
    }
  }
  override def toString = s"${name} of ${suit}"
}

object Suits {def apply() = Seq("Diamonds", "Clubs", "Hearts", "Spades")}

object FullHouse  {
  val randInt = Random.shuffle(1 to 13)
  val v1 = randInt.head
  val v2 = randInt.drop(1).head
  val randSuit = Random.shuffle(Suits())
  val s1 = randSuit.head
  val s2 = randSuit.drop(1).head
  val s3 = randSuit.drop(2).head
  val s4 = randSuit.drop(3).head
  def show = Seq(Card(v1,s1), Card(v1,s2), Card(v1,s3), Card(v2,s1), Card(v2,s4))
  override def toString = show.toList.map(c => c.toString + "\n").mkString
}
