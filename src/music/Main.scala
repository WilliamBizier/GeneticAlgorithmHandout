package music

import statistics._

object Main {

  // Uncomment these methods and call them from main as you implement the required functionality
  // Enjoy the music!

//  def topSongsByBayesianAverage(songs: List[Song], k: Int): Unit = {
//    val numberOfExtraRatings: Int = 2
//    val valueOfExtraRatings: Int = 3
//    val bayesianRatingFunction: Song => Double = (song: Song) => song.bayesianRating(numberOfExtraRatings, valueOfExtraRatings)
//    new Playlist(Statistics.topK(songs, bayesianRatingFunction, k)).openPlaylist()
//  }

//  def topSongsByScore(songs: List[Song], yourRatings: Map[String, Int], k: Int): Unit = {
//    val songCostFunction: Song => Double = Music.songCostFunction(yourRatings)
//    // compute the inverse of the cost so low cost songs rank higher
//    val inverseCost: Song => Double = (song: Song) => 1.0 / songCostFunction(song)
//    new Playlist(Statistics.topK(songs, inverseCost, k)).openPlaylist()
//  }


  def main(args: Array[String]): Unit = {

//    val songs: List[Song] = readSongsFromFile("data/song_ratings_2021.csv")
//    val yourRatings: Map[String, Int] = Music.readUserRatingsFromFile("data/yourRatings.csv")
//    val k: Int = 10

    // Plays the top 10 songs
//    topSongsByBayesianAverage(songs, k)

    // Plays the top 10 songs and takes your ratings into consideration
//    topSongsByScore(songs, yourRatings, k)

  }

}
