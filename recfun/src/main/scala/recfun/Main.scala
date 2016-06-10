package recfun

import java.util

import cafesat.asts.fol.Trees.And
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c<1) 1 else if(c >= r) 1 else pascal(c-1, r-1) + pascal(c, r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balance(idx: Int, numOfLeft: Int): Boolean = {

      def hasNext():Boolean = {

        val lefts = chars.slice(idx, chars.length).filter(_ == '(')
        val rights = chars.slice(idx, chars.length).filter(_ == ')')

        return (lefts.length + rights.length) != 0

      }

      if(!hasNext()) return (numOfLeft==0)
      if(numOfLeft < 0 ) return false

      chars(idx) match {
        case '(' => balance(idx+1, numOfLeft+1)
        case ')' => balance(idx+1, numOfLeft-1)
        case _   => balance(idx+1, numOfLeft  )
      }


    }



    balance(0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if(coins.length == 1) if (money % coins.head == 0 ) return 1 else return 0

    val sorted = coins.sortWith(_ > _)
    val _coins = coins.filter( _ != sorted.head)

    var count = 0;
    var _money = money

    if(sorted.head > _money) return countChange(_money, _coins)
    while(sorted.head <= _money){
      count += countChange(_money, _coins)
      _money -= sorted.head
    }

    if(_money ==0 ) count += 1

    println(count)
    return count

  }
}
