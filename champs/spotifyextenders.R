song_level_lyrics_spotify<-function(x){
  Z<-unite(x, identifier, c(track_n, line, duration_ms))
  ZZ<-select(Z, c(identifier, lyric, album_name, track_name))
  Q<-spread(ZZ, identifier, lyric)
  unite(Q, song_lyric, -c(track_name, album_name), na.rm=TRUE)
}

#remove all non-listed data
spotify_other_information<-function (X){
  dplyr::select(X, -c(lyrics, artists))
}

spotify_other_information(TS)

#extract artists
spotify_artists<-function(X){
  tidyr::unnest(X, lyrics)
  
}