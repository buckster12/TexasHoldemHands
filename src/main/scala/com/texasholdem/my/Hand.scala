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

  // Points of combinations to manage sorting of winners
  val ROYAL_FLUSH = 1000
  val STRAIGHT_FLUSH = 900
  val FOUR_OF_KIND = 800
  val FULL_HOUSE = 700
  val FLUSH = 600
  val STRAIGHT = 550
  val THREE_OF_KIND = 500
  val TWO_PAIRS = 400
  val PAIR = 200

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

  //  input = emulateFourOfKind

  val inputArray = input.split(" ")

  val boardCards = inputArray.head
  println(s"board: ${boardCards}")

  val hands = inputArray.tail

  try {
    val winners = for (hand <- hands) yield (hand.toString, checkStrength(boardCards + hand).toString)
    printResult(winners.toList.sortBy(_._2))
  } catch {
    case e: Exception => println(e)
  }

  def printResult(winnersSorted: List[(String, String)]): Unit = {
    println(s"winners=${winnersSorted}")
    for (i <- 1 until winnersSorted.length + 1) {
      print(winnersSorted(i - 1)._1)
      if (i == winnersSorted.length) return
      if (winnersSorted(i - 1)._2 == winnersSorted(i)._2) print("=") else print(" ")
    }
  }

  def checkStrength(input: String): Int = {
    if (input.length != 14) {
      error("Cards were not identified. Please, check if all the cards have been given.")
    }

    println("findBestCards of: " + input)
    val bestCombination = findTheBestCombination(input)
    val bestCombinationScore = getCombinationScore(bestCombination)

    println(bestCombination)
    //    println()

    return bestCombinationScore


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

    //    println("------")
    //    null
  }

  /*
  Function checks a string for the flush: 5 or more cards with the same suit
   */
  def checkFlush(x: String): Boolean = {
    val list = parseSuits(x).groupBy(identity).mapValues(_.size)
    list.values.exists(_ >= 5)
  }

  /*
  Method gets all cards from a string, for example from a string: KcAc4cJc9c
  it returns: 4 9 11 13 14
   */
  def getIntegerListOfSortedCards(input: String): IndexedSeq[Int] = {
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
      //      println(s"look at combination: $combinationString")

      val combinationScore = getCombinationScore(combinationString)

      // compare next combination with previously saved best combination
      if (combinationScore > bestCombinationScore || bestCombination == null) {
        bestCombination = combinationString
        bestCombinationScore = combinationScore
      }
    }
    bestCombination
  }

  def getPair(sortedArray: IndexedSeq[Int], count: Int = 2): IndexedSeq[Int] = {
    var resultArray = IndexedSeq.empty[Int]
    // set the first card into an array
    resultArray = resultArray :+ sortedArray(0)
    // and loop all the elements to find a pair
    for (i <- sortedArray.tail) {
      if (i != resultArray.last) {
        resultArray = resultArray.empty
      }
      resultArray = resultArray :+ i
      if (resultArray.length == count) return resultArray
    }
    resultArray.empty
  }

  /*
  Method takes an array of cards and check it for a straight
   */
  def isStraight(cards: IndexedSeq[Int]): Boolean = {
    // if we have an Ace then we also should check the Ace as first card of straight (before 2)
    if (cards.contains(CARD_A)) {
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
  as it is a way easier to sort winners/
   */
  def getCombinationScore(cardsString: String): Int = {
    if (cardsString.length != 10) error("Wrong input string: " + cardsString)

    val cards = getIntegerListOfSortedCards(cardsString)
    if (cardsString == "4cKs4hKhKd") println(s"Original: ${cards}")

    // check for all types of Straight
    if (isStraight(cards)) {
      if (checkFlush(cardsString) && cards(0) == CARD_T) return ROYAL_FLUSH // Royal Flush
      if (checkFlush(cardsString)) return STRAIGHT_FLUSH + cards(4) // Straight Flush and the highest card's rank
      return STRAIGHT + cards(4) // just Straight + the highest card's rank
    }

    // just Flush + the highest card's rank
    // we need the highest card's rank to get the higher flush if there are two on hands
    if (checkFlush(cardsString)) return FLUSH + cards(4)

    // four of Kind
    val checkFourOfKind = getPair(cards, 4)
    if (checkFourOfKind.nonEmpty) {
      return FOUR_OF_KIND + cards.diff(checkFourOfKind)(0)
    }

    // Set of three
    // Full house
    val threeOfKind = getPair(cards, 3)
    if (threeOfKind.nonEmpty) {
      val leftCards = cards.diff(threeOfKind)
      println(s"found set: $threeOfKind")
      println(s"left cards: $leftCards")
      //      error("")
      val checkPair = getPair(leftCards)
      if (checkPair.isEmpty) {
        return THREE_OF_KIND + threeOfKind(0) + leftCards.last // set of three + card of set + high card
      } else {
        return FULL_HOUSE + cards(0) // Full house + high card
      }
    }

    //    println("get pair...")
    //    var foundPair = getPair(cards, 2)
    //    println(foundPair)

    0
  }

}
