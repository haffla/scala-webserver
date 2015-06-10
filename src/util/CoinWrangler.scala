package util

object CoinWrangler {

  def calcCoinDenoms(werte: List[Int], betrag: Int): Int = {
    if(betrag < 0) 0
    else if(werte.isEmpty) {
      if(betrag == 0) 1 else 0
    }
    else {
      calcCoinDenoms(werte, betrag - werte.head) +
        calcCoinDenoms(werte.tail, betrag)
    }
  }

  def calcCoinDenomsList(werte: List[Int], betrag: Int): List[List[Int]] = {

    def doCalc(coins: List[Int], amount: Int, current: List[Int]): List[List[Int]] = {
      if(amount < 0) List()
      else if(coins.isEmpty) {
        if(amount == 0) List(current) else List()
      }
      else {
        doCalc(coins, amount - coins.head, coins.head::current) :::
          doCalc(coins.tail, amount, current)
      }
    }

    doCalc(werte, betrag, List())
  }

}
