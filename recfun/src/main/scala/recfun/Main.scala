package recfun

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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceInner(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) count == 0
        else if (count < 0) false
        else {
          val _count = {
            if (chars.head == '(') count + 1
            else if (chars.head == ')') count - 1
            else count
          }
          balanceInner(chars.tail, _count)
        }
      }
      balanceInner(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeInner(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money > 0 && coins.nonEmpty) countChangeInner(money - coins.head, coins) + countChangeInner(money, coins.tail)
        else 0
      }
      countChangeInner(money, coins.sorted)
    }
  }
