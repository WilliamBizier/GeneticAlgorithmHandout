package tests

import music.{Music, Song, SongRating}
import org.scalatest.FunSuite
import statistics.Statistics

class Task3 extends FunSuite{
  test("Test 1: add_multiple_only_adds_one_rating") {

    //compare the doubles n shit
    val epsilon: Double = 0.001

    def compareDoubles(d1: Double, d2: Double): Boolean = {
      Math.abs(d1 - d2) < epsilon
    }

    //creation of songs
    val song1: Song = new Song(" chestbrah", "lebron", "boobies", List(new SongRating(4, 4)))
    val song2: Song = new Song(" JESSE FREESTYLE", "lebron", "balls", List(new SongRating(3, 3)))
    val song3: Song = new Song(" WOMEN", "lebron", "5678", List(new SongRating(2, 2)))
    //val song4: Song = new Song(" the girl in lockwood","lebron","gerald",List(new SongRating(1,1)))
    //val song5: Song = new Song(" ur mother ","lebron","theporn",List(new SongRating(5,5)))

    //creation of song ratings
    val rating1: SongRating = new SongRating(5, 5)
    val rating2: SongRating = new SongRating(4, 4)
    val rating3: SongRating = new SongRating(3, 3)
    val rating4: SongRating = new SongRating(2, 2)
    val rating5: SongRating = new SongRating(1, 1)

    //creation of the list of ratings
    val listofratings: List[SongRating] = List(rating1, rating2, rating3, rating4, rating5)

    //adding the ratings
    val addTheRatings: Song = song1.addMultipleRatings(listofratings)
    val addTheRatings2: Song = song2.addMultipleRatings(listofratings)
    val addTheRatings3: Song = song2.addMultipleRatings(listofratings)

    //sort em
    val sort1: List[SongRating] = addTheRatings.ratings.sortWith(_.rating > _.rating)
    val sort2: List[SongRating] = addTheRatings2.ratings.sortWith(_.rating > _.rating)
    val sort3: List[SongRating] = addTheRatings3.ratings.sortWith(_.rating > _.rating)

    //check the songs
    //song1
    assert(sort1.head.rating == 5)
    assert(sort1(1).rating == 4)
    assert(sort1(2).rating == 4)
    assert(sort1(3).rating == 3)
    assert(sort1(4).rating == 2)
    assert(sort1(5).rating == 1)
    //song2
    assert(sort2.head.rating == 5)
    assert(sort2(1).rating == 4)
    assert(sort2(2).rating == 3)
    assert(sort2(3).rating == 3)
    assert(sort2(4).rating == 2)
    assert(sort2(5).rating == 1)
    //song3
    assert(sort2.head.rating == 5)
    assert(sort2(1).rating == 4)
    assert(sort2(2).rating == 3)
    assert(sort2(3).rating == 3)
    assert(sort2(4).rating == 2)
    assert(sort2(5).rating == 1)
  }
  test("test 2: list_to_map_doesnt_handle_duplicates "){
    //compare the doubles n shit
    val epsilon: Double = 0.001

    def compareDoubles(d1: Double, d2: Double): Boolean = {
      Math.abs(d1 - d2) < epsilon
    }

    //creation of songs
    val song1: Song = new Song(" chestbrah", "lebron", "boobies", List(new SongRating(4, 4)))
    val song2: Song = new Song(" JESSE FREESTYLE", "lebron", "balls", List(new SongRating(4, 3)))
    val song3: Song = new Song(" JESSE FREESTYLE", "lebron", "balls", List(new SongRating(3, 3)))
    val song4: Song = new Song(" chestbrah", "lebron", "boobies", List(new SongRating(4, 4)))


    //create the list of songs
    val listsongs: List[Song] = List(song1,song2,song3)
    val createmap: Map[String,Song] = music.Music.songListToMap(listsongs)
    val songmap: Song = createmap("balls")
    val sort: List[SongRating] = songmap.ratings.sortWith(_.rating > _.rating)
    assert(sort.head.rating==4)
    assert((sort(1).rating==3))






  }

}

