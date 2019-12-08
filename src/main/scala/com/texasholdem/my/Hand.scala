package com.texasholdem.my

import scala.util.Random
import scala.io.StdIn

object Hand extends App {

  // Init constants to convert T,J,Q,K,A to Integer equivalent
  val CARD_T = 10
  val CARD_J = 11
  val CARD_Q = 12
  val CARD_K = 13
  val CARD_A = 14

  //  val input = StdIn.readLine("Start cards: ")

  // get input and process
  var input = "4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d"
  val emulateThreeOfKind = "AcAs4h8s7s Ad4s Ac4d As9s KhKd 5d6d"
  val emulateFlush = "AcAs4h8c7c Ad4s Ac4c As9c KhKd 5c6c"
  val emulateFourOfKind = "KcKs4h8c7c Ad4s Ac4c As9c KhKd 5c6c"
  val emulateStraight = "2c3s4h5c7c AdKs Ac4c As9c KhKd 5c6c"
  val emulateRoyalFlush = "AcKcQc5c7c JcTc Ac4c As9c KhKd 5c6c"
  val emulateStraightFlush = "9cKcQc5c7c JcTc Ac4c As9c KhKd 5c6c"
  val emulateException = "9cKcQc5c7c dddd"

  input = emulateException

  val inputArray = input.split(" ")

  val boardCards = inputArray.head
  println(s"board: ${boardCards}")

  val hands = inputArray.tail

  try {
    for (hand <- hands) checkStrength(boardCards + hand)
  } catch {
    case e: Exception => println(e)
  }

  //  val result = checkStrength("")
  //  println(result)

  def checkStrength(input: String): String = {
    if (input.length != 14) {
      error("Cards were not identified. Please, check if all the cards have been given.")
    }

    print("findBestCards...")
    val bestCombination = findTheBestCombination(input)
    println(bestCombination)
    println(getCombinationScore(bestCombination))

    //    println(s"checking Strength of string: ${input}")

    //    val isFlash = checkFlush(input)
    //    println("Flash: " + isFlash)

    //    val isFourOfKind = checkFourOfKind(input)
    //    println("FourOfKind: " + isFourOfKind)

    /*
    val cards = input.grouped(2).toList
    for (card <- cards) {
      println(s"Checking cards: ${card}")
      checkFlush(card)
    }
     */

    println("------")
    null
  }

  /*
  Function checks a string for the flush: 5 or more cards with the same suit
   */
  def checkFlush(x: String): Boolean = {
    val list = parseSuits(x).groupBy(identity).mapValues(_.size)
    list.values.exists(_ >= 5)
  }

  /*
  Function checks a string for the four of kind: 4 cards the same value
  */
  def checkFourOfKind(x: String): Boolean = {
    val list = parseCards(x).groupBy(identity).mapValues(_.size)
    list.values.exists(_ == 4)
  }

  /*
  Function checks a string for the three of kind: 3 cards the same value
  */
  def checkThreeOfKind(x: String): Boolean = {
    val list = parseCards(x).groupBy(identity).mapValues(_.size)
    list.values.exists(_ == 3)
  }

  /*
  Method gets all cards from a string, for example from a string: KcAc4cJc9c
  it returns: K A 4 J 9
   */
  def parseCards(input: String): IndexedSeq[Int] = {
    val cards = for (i <- 0 until input.length if i % 2 == 0) yield input.charAt(i).toChar
    val replacedCards = cards.map {
      case 'A' => CARD_A;
      case 'K' => CARD_K;
      case 'Q' => CARD_Q;
      case 'J' => CARD_J;
      case 'T' => CARD_T;
      case '2' => 2;
      case '3' => 3;
      case '4' => 4;
      case '5' => 5;
      case '6' => 6;
      case '7' => 7;
      case '8' => 8;
      case '9' => 9;
      case x => 0
    }.sorted
    if (replacedCards.contains(0)) error("Wrong card value in string: " + input)
    replacedCards
  }

  def parseSuits(input: String): IndexedSeq[Char] = {
    val suits = for (i <- 0 until input.length if i % 2 == 1) yield input.charAt(i)
    // check if suits have only allowed values (c s d h)
    for (suit <- suits) suit match {
      case 'c' => ;
      case 's' => ;
      case 'd' => ;
      case 'h' => ;
      case x => error("Wrong suit: " + x);
    }
    suits
  }

  def error(msg: String) {
    throw new Exception(msg)
  }

  /*
  The method create all possible combinations,
  then compare all of them by getting a score,
  and return the best one
   */
  def findTheBestCombination(allCards: String): String = {
    // split cards to array
    val cards = for (i <- 0 until allCards.length if i % 2 == 0) yield allCards.charAt(i).toString + allCards.charAt(i + 1).toString
    // create an array with all possible combinations
    val uniqueCombinations = cards.combinations(5).toList

    // init null variables to save the best combination
    var bestCombinationScore: Int = 0
    var bestCombination: String = null

    // loop all combinations and search for the best one by its score
    for (comb <- uniqueCombinations) {
      val combinationString = comb.mkString
      val combinationScore = getCombinationScore(combinationString)

      // compare next combination with previously saved best combination
      if (combinationScore > bestCombinationScore || bestCombination == null) {
        bestCombination = combinationString
        bestCombinationScore = combinationScore
      }
    }
    bestCombination
  }

  /*
  Method takes an array of cards and check it for a straight
   */
  def isStraight(cards: IndexedSeq[Int]): Boolean = {
    // if we have an Ace then we also should check the Ace as first card of straight (before 2)
    if (cards.contains(13)) {
      val secondVariant = cards.updated(4, 1).sorted
      if (secondVariant(0).to((secondVariant(0)) + 4).toVector == secondVariant)
        return true
    }
    if (cards(0).to((cards(0)) + 4).toVector == cards)
      return true
    false
  }

  /*
  I developed an integer equivalent of each of combination,
  as it is a way easier to sort winners
   */
  def getCombinationScore(cardsString: String): Int = {
    if (cardsString.length != 10) error("Wrong input string: " + cardsString)

    val cards = parseCards(cardsString)
    //    println(s"Original: ${cards}")

    // check for all types of Straight
    if (isStraight(cards)) {
      if (checkFlush(cardsString) && cards(0) == 10) return 1000 // Royal Flush
      if (checkFlush(cardsString)) return 900 // Straight Flush
      return 550 // just Straight
    }


    if (checkThreeOfKind(cardsString)) return 500


    if (checkFlush(cardsString)) return 600 // just Flush


    if (checkFourOfKind(cardsString)) return 800 // four of kind

    0
  }

  // I think I may be useful for kicker but not sure yet
  def getTheHighestCard(handCards: String): String = {
    val cards = parseCards(handCards)
    println(cards)
    for (card <- cards) println(s"${card} = ${getScoreOfCard(card.toString)}")
    ""
  }

  /*
  Method return a card's integer value for comparing possibility
   */
  def getScoreOfCard(card: String): Int = {
    var value = 0
    if (card == "J") value = 11
    else if (card == "Q") value = 13
    else if (card == "K") value = 14
    else if (card == "A") value = 15
    else value = card.toInt
    value
  }

}
