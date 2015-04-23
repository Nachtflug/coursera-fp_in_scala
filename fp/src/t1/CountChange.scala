package t1

/**
 * Created by tripp.hu on 3/27/2015.
 */
object CountChange extends App{
  def countChange(money: Int, coins: List[Int]) :Int = {
    coins.sortWith((x,y) => x > y)     //先给币值从大到小排个序
    def loop(money: Int, coins: List[Int]): Int = {
      val moneyRest = money - coins.head
      0 +               //不能直接代码块开头（可能是无法确定类型，过不了编译？），所以加个零
      {                 //这个代码块检查硬币币值是否是 最后一个，不是的话递归除当前最大币值外余下情况
        if(coins.tail.isEmpty) 0
        else loop(money, coins.tail)
      } + {             //这个代码块检查 是否还有余下的钱，有就递归余下钱的情况
        if (moneyRest == 0) 1
        else if (moneyRest > 0) loop(moneyRest, coins)
        else 0
      }
    }
    loop(money, coins)
  }

  println(countChange(20,List(3,5,7,9)))
}
