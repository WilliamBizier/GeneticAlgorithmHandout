package genetics
import music.{Song, SongRating,Music}



import scala.util.Random


object GeneticAlgorithm {

  //list of doubles

  def listofrandos(numbergenes2: Int, reclist: List[Double]): List[Double] = {
    if (numbergenes2 == 0){
      reclist
    }else{
      val numgen: Double = Random.between(-100.0, 101.0)
      listofrandos(numbergenes2-1, reclist:+numgen)
    }
  }

  def listoflist(numberoftimes: Int, reclist: List[List[Double]],numberofgenes2:Int): List[List[Double]] = {
    if (numberoftimes == 0){
      println(reclist)
      reclist

    }else{
      val listgen: List[Double] = listofrandos(numberofgenes2,List())
      listoflist(numberoftimes-1,reclist:+listgen,numberofgenes2)
    }
  }

  def helperGeneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int,counter:Int,reclist:List[T]): List[T] = {
    if(counter == 0){
      reclist
    }else {
      val makelistoflist: List[List[Double]] = listoflist(420, List(), numberOfGenes)
      val applyinc: List[T] = makelistoflist.map(incubator(_))
      //println(applyinc)
      //balls lmao
      helperGeneticAlgorithm(incubator,costFunction,numberOfGenes,counter-1,reclist:::applyinc)
    }
  }

  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {
    val applyinc: List[T] = helperGeneticAlgorithm(incubator,costFunction,numberOfGenes,69,List())
    //println(applyinc.size)
    val applycostfunc: List[Double] = applyinc.map(costFunction(_))
    //println(applycostfunc)
    val sortedlist: List[T] = applyinc.sortWith(costFunction(_)<costFunction(_))
    //balls lmao
    val gigachad: T = sortedlist.head
    gigachad

  }



  def main(args: Array[String]): Unit = {
    val numbergen: Int = (Math.random * 100).toInt
    val numbergenneg: Int = (Math.random * 100).toInt
    val cock: List[Double] = listofrandos(4,List())
    println(cock)

    val latinas: List[List[Double]]=listoflist(4,List(),4)
    println(latinas)

    val balllist: List[Int] = List(1,2,3,4,5,6,7,8,9)
    val ballfirst: Int = balllist.head
    println(ballfirst)

    val song1: Song = new Song(" chestbrah","lebron","boobies",List(new SongRating(4,4)))
    val song2: Song = new Song(" JESSE FREESTYLE","lebron","balls",List(new SongRating(3,3)))
    val song3: Song = new Song(" WOMEN","lebron","5678",List(new SongRating(2,2)))
    val song4: Song = new Song(" the girl in lockwood","lebron","gerald",List(new SongRating(1,1)))
    val song5: Song = new Song(" ur mother ","lebron","theporn",List(new SongRating(5,5)))

    val listsong: List[Song] = List(song1,song2,song3,song4,song5)
    val mapthing: Map[String,Int] = Map("boobies"->4,"balls"->3,"5678"->2,"gerald"->1,"theporn"->5)
    println(geneticAlgorithm(Music.songIncubator(listsong),Music.songCostFunction(mapthing),5).title)

  }
}

