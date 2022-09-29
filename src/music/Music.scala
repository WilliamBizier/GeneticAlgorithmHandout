package music

import music.Music._

import scala.io.{BufferedSource, Source}

object Music {

  def songCostFunction(ratingsoftheuser: Map[String, Int]): Song => Double = {
    (balls: Song) => {
      val rating: Int = ratingsoftheuser.getOrElse(balls.youtubeId, 3)
      if (rating <= 2) {
        1000.0
      }
      else {
        1 / (balls.bayesianRating(2, 3) * rating)
      }
    }
  }
  // TODO: Implement required methods as defined in the HW Doc

  /**
   * You may use this helper method to read a file and return a List of Strings containing the lines of the file
   *
   * @param filename The file to be read
   * @return The lines of the file as a List of Strings with 1 String per line
   */


  //helper

  def filenameToListOfLines(filename: String): List[String] = {
    val file: BufferedSource = Source.fromFile(filename)
    val lines: List[String] = file.getLines().toList
    file.close()
    lines
  }


  //helper 2
  def filereaderhelper(filename: String, number: Int, inputmap: Map[String, Int]): Map[String, Int] = {
    val reader = filenameToListOfLines(filename)
    val therays: Array[String] = reader(number).split(",")
    if (number == reader.size - 1) {
      val endmap: Map[String, Int] = inputmap + (therays(0) -> therays(3).toInt)
      endmap
    }
    else {
      val recursivemap: Map[String, Int] = inputmap + (therays(0) -> therays(3).toInt)
      filereaderhelper(filename, number + 1, recursivemap)
    }
  }

  //returning function
  def readUserRatingsFromFile(thefile: String): Map[String, Int] = {
    val cock = filereaderhelper(thefile, 0, Map())
    cock

  }


  //helper
  def helpReadSongsFromFileWithoutDuplicates(filename: String, number: Int, inputlist: List[Song]): List[Song] = {
    val reader = filenameToListOfLines(filename)
    val therays: Array[String] = reader(number).split(",")
    if (number == reader.size - 1) {
      val songRateAdd: SongRating = new SongRating(therays(3).toInt, therays(4).toInt)
      val songAdd: Song = new Song(therays(2), therays(1), therays(0), List(songRateAdd))
      val list1 = inputlist :+ songAdd
      list1
    }
    else {
      val songRateAdd: SongRating = new SongRating(therays(3).toInt, therays(4).toInt)
      val songAdd: Song = new Song(therays(2), therays(1), therays(0), List(songRateAdd))
      helpReadSongsFromFileWithoutDuplicates(filename, number + 1, inputlist :+ songAdd)
    }
  }

  //returning function
  def readSongsFromFileWithoutDuplicates(filename: String): List[Song] = {
    val cock = helpReadSongsFromFileWithoutDuplicates(filename, 0, List())
    cock

  }

  // TODO: Uncomment after implementing the required methods
  // Uses your methods to read all the data in a file with song ratings
   def readSongsFromFile(filename: String): List[Song] = {
      val songsWithDuplicates: List[Song] = readSongsFromFileWithoutDuplicates(filename)
      val songMap: Map[String, Song] = songListToMap(songsWithDuplicates)
      songMap.values.toList
  }

  // Can be used to test your application objective
  def songIncubator(songs: List[Song]): List[Double] => Song = {
    genes: List[Double] => {
      val geneSong: Int = (genes.head.abs * songs.length).toInt % songs.length
      songs(geneSong)
    }
  }

  // Can be used to test your application objective
  def playlistIncubator(songs: List[Song]): List[Double] => Playlist = {
    genes: List[Double] => {
      val incubatorForSongs: List[Double] => Song = songIncubator(songs)
      new Playlist(genes.map((gene: Double) => incubatorForSongs(List(gene))))
    }
  }

  //helper for songListToMap
  def helpersongListToMap(listofsong1:List[Song],index:Int, recmap:Map[String,Song]): Map[String,Song] = {
    //helps pull song out, so placeinlist is a singular song object

    if(index==listofsong1.size) {
      val cock:  Map[String,Song] = recmap
      cock}
    else {
      val placeinlist: Song = listofsong1(index)
      //this is for when the song alreadu exists
      if (recmap.contains(placeinlist.youtubeId)) {
        val youtubething: String = placeinlist.youtubeId
        val ratings: List[SongRating] = placeinlist.ratings
        val mapping: Song = recmap(placeinlist.youtubeId)
        val addtheratings: Song = mapping.addMultipleRatings(ratings)
        val createmap: Map[String, Song] = recmap + (youtubething -> addtheratings)
        helpersongListToMap(listofsong1, index + 1, createmap)
      }
      else {
        val youtubething: String = placeinlist.youtubeId
        val anothermap: Map[String, Song] = recmap + (youtubething -> placeinlist)
        helpersongListToMap(listofsong1, index + 1, anothermap)

      }
      //else the regular recursice
    }
  }


  def songListToMap(listsongs: List[Song]): Map[String, Song] = {
    helpersongListToMap(listsongs,0,Map())
  }

  def main(args: Array[String]): Unit = {
    val cock: List[Song] = readSongsFromFile("data/yourRatings.csv")
    println(songListToMap(cock))


  }
}

