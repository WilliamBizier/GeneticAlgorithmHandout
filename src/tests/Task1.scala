package tests

import music.{Music, Song, SongRating}
import org.scalatest._
import statistics.Statistics

class Task1 extends FunSuite {

  //compare the doubles n shit
  val epsilon: Double = 0.001
  def compareDoubles( d1: Double, d2: Double): Boolean =
  {
    Math.abs(d1-d2) < epsilon
  }



  test("topk: greated to least ( and the top _ )") {
    val numbers: List[String]=List("1","11","111","1111","11111")
    val computed = Statistics.topK(numbers,(number: String) => number.length,3)
    val expected: List[String] = List("11111","1111","111")
    assert(expected == computed)
  }
  test("topk: greater than list ") {
    val numbers: List[String]=List("1","11","111","1111","11111")
    val computed = Statistics.topK(numbers,(number: String) => number.length,7)
    val expected: List[String] = List("11111","1111","111","11","1")
    assert(expected == computed)
  }

  test("bay average") {
    val input: List[Int] = List(2, 3, 4, 5, 6, 7, 8, 9)
    val f: Int => Double = (number: Int) => number.toDouble
    val baypride: Double = Statistics.bayesianAverage(input, f, 3, 3)
    val answer: Double = 4.81818181
    assert(compareDoubles(answer, baypride))
  }

  test("testing the music object and song cost function"){

    //the creation of songs
    val songunrated: Song = new Song(" balls in yo jaws","lebron","1234",List(new SongRating(5,5)))
    val song1: Song = new Song(" chestbrah","lebron","boobies",List(new SongRating(4,4)))
    val song2: Song = new Song(" JESSE FREESTYLE","lebron","balls",List(new SongRating(3,3)))
    val song3: Song = new Song(" WOMEN","lebron","5678",List(new SongRating(2,2)))
    val song4: Song = new Song(" the girl in lockwood","lebron","gerald",List(new SongRating(1,1)))
    val song5: Song = new Song(" ur mother ","lebron","theporn",List(new SongRating(5,5)))

    val thegrammys: Map[String,Int] = Map(
      "boobies" -> 4,
      "balls" -> 3,
      "5678" -> 2,
      "gerald" -> 1,
      "theporn" ->5
    )

    val costFunction: Song => Double = Music.songCostFunction(thegrammys)



    val computed:Double=costFunction(songunrated)
    val computed1:Double=costFunction(song1)
    val computed2:Double=costFunction(song2)
    val computed3:Double=costFunction(song3)
    val computed4:Double=costFunction(song4)
    val computed5:Double=costFunction(song5)

    println(computed)
    println(computed1)
    println(computed2)
    println(computed3)
    println(computed4)
    println(computed5)


    //0.05454545454545455
    //0.075
    //0.1111111111111111
    //1000.0
    //1000.0
    //0.09090909090909091

    assert(compareDoubles(computed,0.09090909090909091))
    assert(compareDoubles(computed1,0.075))
    assert(compareDoubles(computed2,0.1111111111111111))
    assert(compareDoubles(computed3,1000.0))
    assert(compareDoubles(computed4,1000.0))
    assert(compareDoubles(computed5,0.05454545454545455))




  }
}