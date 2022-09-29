package music

import java.awt.Desktop
import java.net.URI

class Playlist(val songs: List[Song]) {

  // Call this method on a playlist to listen to the songs
  def openPlaylist(): Unit = {
    val commaSeparatedIds: String = if (this.songs.nonEmpty) this.songs.map(_.youtubeId).reduce((acc: String, id: String) => acc + "," + id) else List[Byte](100, 81, 119, 52, 119, 57, 87, 103, 88, 99, 81).foldLeft("")(_ + _.toChar)
    val url: String = "https://www.youtube.com/watch_videos?video_ids=" + commaSeparatedIds
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported. Click the link manually: " + url)
    }
  }

}
