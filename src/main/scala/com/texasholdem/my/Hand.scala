package com.texasholdem.my

import scala.util.Random
import scala.io.StdIn

object Hand extends App {
  //  val ages = Seq(42, 75, 29, 64)
  //  println(s"The oldest person is ${ages.max}")
  //
  //  val Cards = Map(
  //    "Two" -> 2,
  //    "Three" -> 3,
  //    "For" -> 4,
  //    "Five" -> 5,
  //    "Six" -> 6,
  //    "Six" -> 7,
  //    "Six" -> 8,
  //    "Six" -> 9,
  //
  //    "Ten" -> 10,
  //    "Jack" -> 11,
  //    "Queen" -> 12,
  //    "King" -> 13,
  //    "Ace" -> 14,
  //  );
  //
  //  val Suits = Map(
  //    "Spades" -> 0,
  //    "Hearts" -> 1,
  //    "Diamonds" -> 2,
  //    "Clubs" -> 3
  //  );
  //  val SuitsSet = Set("Spades", "Hearts", "Diamonds", "Clubs")

  //
  //  val Suits = Map(
  //    "Spades" -> 0,
  //    "Hearts" -> 1,
  //    "Diamonds" -> 2,
  //    "Clubs" -> 3
  //  );

  //  val SuitsSet = Set("Spades", "Hearts", "Diamonds", "Clubs")

  //  val input = StdIn.readLine("Start cards: ")

  // get input and process
  var input = "4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d";
  val emulateThreeOfKind = "AcAs4h8s7s Ad4s Ac4d As9s KhKd 5d6d";
  val emulateFlush = "AcAs4h8c7c Ad4s Ac4c As9c KhKd 5c6c";
  val emulateFourOfKind = "KcKs4h8c7c Ad4s Ac4c As9c KhKd 5c6c";

  input = emulateFourOfKind

  val inputArray = input.split(" ")

  val boardCards = inputArray.head
  println(s"board: ${boardCards}")

  val hands = inputArray.tail
  for (hand <- hands) checkStrength(boardCards + hand)


  //  val result = checkStrength("")
  //  println(result)


  def checkStrength(input: String): String = {
    if (input.length != 14) {
      error("Wrong input in checkStrength()")
      return null
    }

    println("findBestCards...");
    findTheBestCombination(input)

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
  Method gets all cards from a string, for example from a string: KcAc4cJc9c
  it returns: K A 4 J 9
   */
  def parseCards(input: String): IndexedSeq[Char] = {
    for (i <- 0 until input.length if i % 2 == 0) yield input.charAt(i)
  }

  def parseSuits(input: String): IndexedSeq[Char] = {
    for (i <- 0 until input.length if i % 2 == 1) yield input.charAt(i)
  }

  def error(msg: String) {
    println(msg)
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
      if (combinationScore > bestCombination) {
        bestCombination = combinationString
        bestCombinationScore = combinationScore
      }
    }
    bestCombination
  }

  def getCombinationScore(cardsString: String): Int = {
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
