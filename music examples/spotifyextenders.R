#First, go ahead and log into the spotify API

#today we will use the Weird Al example
Al<-search_spotify("Weird Al")
ABBA<-search_spotify("ABBA")
Drake<-search_spotify("Drake")

Drake$tracks$items$popularity
Drake$tracks$items$name


#take an unnested dataframe 
song_level_lyrics_spotify<-function(x){
  Z<-unite(x, identifier, c(track_n, line, duration_ms))
  ZZ<-select(Z, c(identifier, lyric, album_name, track_name))
  Q<-spread(ZZ, identifier, lyric)
  unite(Q, song_lyric, -c(track_name, album_name), na.rm=TRUE)
}



#remove all non-listed data
spotify_other_information<-function (X){
  dplyr::select(X, c(track_name, tempo, key, liveness, speechiness, ) )
}

#extract artists
spotify_artists<-function(X){
  tidyr::unnest(X, artists)
}