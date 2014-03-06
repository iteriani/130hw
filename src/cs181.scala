/**
 * Created by Reginald on 3/4/14.
 */
class cs181 {

  def reconstruct(word:String):Array[String]={
    val orig = word.split("")
    var arr = word.split("")
    for(i <- 0 until word.length-1){
      arr.sortWith(_ < _)
      var i = 0
      while(i < arr.length){
        arr(i) = orig(i) + arr(i)
      }
    }
    arr
  }

  def main(args: Array[String]) = {
    var solution = reconstruct("TTCCTAACG$A")
    solution = solution.sortWith(_<_)
    println(solution(0))
  }

}