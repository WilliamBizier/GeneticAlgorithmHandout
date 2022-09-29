package statistics

object Statistics {

  //doesnt take empty list jit
  //call function on items in the list then find the average after that
  def average[T](data: List[T], f: T => Double): Double = {

    val applyf: List[Double] = data.map(f(_))
    val step1: Double =applyf.sum
    val step2: Int = data.length
    val step3: Double = step1/step2
    step3
    // TODO
  }

  //k the number of elements
  // f function applyed to the list[t]
  //f(_)
  def topK[T](ratings: List[T], f: T => Double, k:Int): List[T] = {
    val numberssorted: List[T] = ratings.sortBy(f(_))
    val liluzivert: List[T] = numberssorted.reverse
    if (liluzivert.length < k) {
      liluzivert
    }
    else {
      val slicedelements: List[T] = liluzivert.slice(0,k)
      slicedelements
    }
  }

  //apply the function to the list
  // add the shits to the end
  // take average
  def bayesianAverage[T](bayratings: List[T],f: T => Double, numoffakes: Int, valoffakes:Int): Double ={
    val applyf: List[Double] = bayratings.map(f(_))
    //list.fill(how many you want)( what u want )
    val listofbay: List[Double] = List.fill(numoffakes)(valoffakes.toDouble)
    val mergedlist: List[Double] = applyf ++ listofbay
    val sumoflist: Double = mergedlist.sum
    val averaged: Double = sumoflist/(bayratings.length+numoffakes)
    averaged


  }

}