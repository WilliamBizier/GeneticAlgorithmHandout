package music

import statistics.Statistics

class Song(val title: String, val artist: String, val youtubeId: String, val ratings: List[SongRating]) {

  def averageRating(): Double = {
    // This is an example of calling your average function to get the average rating of a song
    Statistics.average(this.ratings, (rating: SongRating) => rating.rating)
  }

  def averageEnergyRating(): Double = {
    // This is an example of calling your average function to get the average energy rating of a song
    Statistics.average(this.ratings, (rating: SongRating) => rating.energyLevel)
  }

  // TODO: Uncomment this method after writing Statistics.bayesianAverage
  // Compute the bayesian average of song ratings
  def bayesianRating(extraRatings: Int, valueOfExtraRatings: Int): Double = {
    Statistics.bayesianAverage(this.ratings, (rating: SongRating) => rating.rating, extraRatings, valueOfExtraRatings)
  }



  def addRating(theSongRating: SongRating): Song = {
    val newList: List[SongRating] = this.ratings:+theSongRating
    new Song(this.title,this.artist,this.youtubeId,newList)


  }
  /*
    //helper 2
  def filereaderhelper(filename: String, number: Int, inputmap: Map[String, Int]): Map[String, Int] = {
    val reader = filenameToListOfLines(filename)
    val therays: Array[String]=reader(number).split(",")
    if (number == reader.size - 1) {
      val endmap: Map[String, Int] = inputmap + (therays(0)->therays(3).toInt)
      endmap
    }
    else {
      val recursivemap: Map[String, Int] = inputmap + (therays(0)->therays(3).toInt)
      filereaderhelper(filename, number + 1, recursivemap)
    }
  }

    //returning function
    def readUserRatingsFromFile(thefile: String): Map[String, Int] = {
      val cock = filereaderhelper(thefile, 0, Map())
      cock

   */

  def addMultipleRatings(listOfSongRating: List[SongRating]): Song = {
    //https://www.scala-lang.org/api/2.13.8/scala/collection/immutable/List.html
    val newList: List[SongRating] = this.ratings:::listOfSongRating
    new Song(this.title,this.artist,this.youtubeId,newList)
  }


}
