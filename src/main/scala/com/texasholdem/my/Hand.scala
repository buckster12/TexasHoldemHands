package com.texasholdem.my

import sun.misc.Signal
import sun.misc.SignalHandler
import scala.collection.mutable.ArrayBuffer

object Hand extends App {

  // Init constants to convert T,J,Q,K,A to Integer equivalent
  val CARD_T = 10
  val CARD_J = 11
  val CARD_Q = 12
  val CARD_K = 13
  val CARD_A = 14

  // Points of combinations to sort of winners
  val ROYAL_FLUSH = 1000
  val STRAIGHT_FLUSH = 950
  val FOUR_OF_KIND = 900
  val FULL_HOUSE = 650
  val FLUSH = 600
  val STRAIGHT = 550
  val THREE_OF_KIND = 500
  val TWO_PAIRS = 200
  val PAIR = 100

  var inputLinesBuffer = ArrayBuffer[String]()
  val DEBUG = false

  /*
  EOF handler
   */
  Signal.handle(new Signal("INT"), new SignalHandler {
    def handle(sig: Signal): Unit = {
      processLines(inputLinesBuffer)
      System.exit(0)
    }
  })

  println("Input: ")
  // read lines and wait for an empty string (double enter) to start program
  //  val inputLines = Iterator.continually(io.StdIn.readLine).takeWhile(_.nonEmpty).toList
  //  inputLinesBuffer += Iterator.continually(io.StdIn.readLine).takeWhile(_.nonEmpty)

  try {
    do {
      inputLinesBuffer += io.StdIn.readLine("")
    } while (true)
  } catch {
    case e: Exception => println("error: " + e)
    case e: Throwable => println("error: " + e)
  }

  /*
  Gets a list of lies and parse them one by one
   */
  def processLines(lines: ArrayBuffer[String]): Unit = {
    //  def processLines(lines: List[String]): Unit = {
    println("Output: ")
    for (line <- lines) {
      if (line.nonEmpty) prepareOutput(line.trim)
    }
  }

  /*
  This functions gets one line, parse it and prints a result
   */
  def prepareOutput(input: String): Unit = {
    val inputArray = input.split(" ")
    val boardCards = inputArray.head
    val hands = inputArray.tail

    if (boardCards.length != 10) {
      println("Wrong format of board cards.")
    }
    else
      try {
        val winners = for (hand <- hands) yield (hand.toString, checkStrength(boardCards + hand).toString)
        printResult(winners.toList.sortBy(_._2))
        // DEBUG MODE
        if (DEBUG)
          println(winners.toList)

        println("")
      } catch {
        case e: Exception => println("Error: " + e.toString)
        case unknown: Throwable => println("Error: " + unknown.toString)
      }
  }

  /*
  Prints sorted array in one line
   */
  def printResult(winnersSorted: List[(String, String)]): Unit = {
    for (i <- 1 until winnersSorted.length + 1) {
      print(winnersSorted(i - 1)._1)
      if (i == winnersSorted.length) return
      if (winnersSorted(i - 1)._2 == winnersSorted(i)._2) print("=") else print(" ")
    }
    println("")
  }

  /*
  Takes a 7 cards line and check its best combination score
   */
  def checkStrength(input: String): Int = {
    if (input.length != 14) {
      error("Cards were not identified. Please, check if all the cards have been given.")
    }
    val bestCombination = findTheBestCombination(input)
    getCombinationScore(bestCombination)
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
  def getIntegerListOfSortedCards(input: String): List[Int] = {
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
    }.sorted.toList
    if (replacedCards.contains(0)) error("Wrong card value in string: " + input)
    replacedCards
  }

  def parseSuits(input: String): List[Char] = {
    val suits = for (i <- 0 until input.length if i % 2 == 1) yield input.charAt(i)
    // check if suits have only allowed values (c s d h)
    for (suit <- suits) suit match {
      case 'c' => ;
      case 's' => ;
      case 'd' => ;
      case 'h' => ;
      case x => error("Wrong suit: " + x);
    }
    suits.toList
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

  def getPair(sortedArray: List[Int], count: Int = 2): List[Int] = {
    var resultArray = List.empty[Int]
    // set the first card into an array
    resultArray = resultArray :+ sortedArray(0)
    // and loop all the elements to find a pair
    for (i <- sortedArray.tail) {
      if (i != resultArray.last) {
        resultArray = resultArray.empty
      }
      resultArray = resultArray :+ i
      if (resultArray.length == count) return resultArray.toList
    }
    resultArray.empty.toList
  }

  /*
  Method takes an array of cards and check it for a straight
   */
  def isStraight(cards: List[Int]): Boolean = {
    // if we have an Ace then we also should check the Ace as first card of straight (before 2)
    if (cards.contains(CARD_A)) {
      val secondVariant = cards.updated(4, 1).sorted
      if (secondVariant.head.to(secondVariant.head + 4).toList == secondVariant)
        return true
    }
    if (cards.head.to(cards.head + 4).toList == cards)
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
    //    if (cardsString == "4cKs4hKhKd") println(s"Original: ${cards}")

    // check for all types of Straight
    if (isStraight(cards)) {
      if (checkFlush(cardsString) && cards.head == CARD_T) return ROYAL_FLUSH // Royal Flush
      if (checkFlush(cardsString)) return STRAIGHT_FLUSH + cards.last // Straight Flush and the highest card's rank
      return STRAIGHT + cards(4) // just Straight + the highest card's rank
    }

    // just Flush + the highest card's rank
    // we need the highest card's rank to get the higher flush if there are two on hands
    if (checkFlush(cardsString)) return FLUSH + cards.last

    // four of Kind
    val checkFourOfKind = getPair(cards, 4)
    if (checkFourOfKind.nonEmpty) {
      return FOUR_OF_KIND + cards.diff(checkFourOfKind).head
    }

    // Set of three
    // Full house
    val threeOfKind = getPair(cards, 3)
    if (threeOfKind.nonEmpty) {
      val leftCards = cards.diff(threeOfKind)
      val checkPair = getPair(leftCards)
      if (checkPair.isEmpty) {
        return THREE_OF_KIND + threeOfKind.head + leftCards.last // set of three + card of set + high card
      } else {
        return FULL_HOUSE +
          Math.pow(threeOfKind.head, 2).toInt + // we need pow() to give extra bonus to set pair
          checkPair.head +
          cards.head // Full house + high card
      }
    }

    // just pairs
    val firstPair = getPair(cards, 2)
    if (firstPair.nonEmpty) {
      val leftCardsOfPair = cards.diff(firstPair)
      val secondPair = getPair(leftCardsOfPair, 2)
      if (secondPair.nonEmpty) {
        // return point of: two pairs + card of 1st pair + card of 2nd pair + high card
        return TWO_PAIRS +
          firstPair.head +
          Math.pow(secondPair.head, 2).toInt + // here we give an extra bonus of higher pair
          // to prevent situation when A and 2 pair will be lower than K and Q
          leftCardsOfPair.diff(secondPair).head

      } else
        return PAIR + firstPair.head + leftCardsOfPair.last
    }

    // or just return the highest card
    cards.last
  }

}
