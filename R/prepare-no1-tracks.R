library(tidyverse)
library(glue)
library(ggtext)


#' Download data from Kaggle:
#' https://www.kaggle.com/dhruvildave/billboard-the-hot-100-songs

charts <- read_csv(file.path("data", "charts.csv"))
glimpse(charts)

no1 <- charts |> 
  filter(rank == 1) |> 
  select(date, song, artist)

max(no1$date)

# Latest date in the Kaggle dataset is 2021-11-06
# "All Too Well" made no. 1 on 2021-11-27
# Adele - Easy On Me was no. 1 on 2021-11-13 and 2021-11-20
# Source: https://www.billboard.com/charts/hot-100/2021-11-27/
# https://www.billboard.com/music/chart-beat/taylor-swift-all-too-well-hot-100-debut-1235001340/

no1_ext <- no1 |> 
  add_row(date = as_date("2021-11-13"), song = "Easy On Me", artist = "Adele") |> 
  add_row(date = as_date("2021-11-20"), song = "Easy On Me", artist = "Adele") |> 
  add_row(date = as_date("2021-11-27"), song = "All Too Well (10 Minute Version) (Taylor's Version)", artist = "Taylor Swift") |> 
  arrange(desc(date))


discard_featuring_artist_name <- function(x) {
  regex <- "(?i)\\s(Feat(\\.|uring)?|x|&|With\\s|And The|And His|Starring|\\(.+).+"
  str_remove_all(x, regex)
}

cleanup_song_name <- function(x) {
  str_remove(x, "(\\s\\(|/).+")
}


no1_uniq <- no1_ext |> 
  # distinct(song, artist) |> 
  group_by(song, artist) |> 
  summarize(first_no1_date = min(date), .groups = "drop") |> 
  mutate(artist2 = discard_featuring_artist_name(artist),
         song2 = cleanup_song_name(song),
         artist_song = glue("{artist2} - {song2}")) |> 
  # manually clean up some song names
  mutate(
    artist_song = case_when(
      artist == "Barbra Streisand/Donna Summer" &
        song == "No More Tears (Enough Is Enough)" ~ "Barbra Streisand - No More Tears (Enough Is Enough)",
      artist == "Joey Dee & the Starliters" & 
        song == "Peppermint Twist - Part I" ~ 
        "Joey Dee & the Starliters - Peppermint Twist",
      TRUE ~ as.character(artist_song)  # glue returns an object of class glue which doesn't work with case_when
    )
  )


tracks <- read_rds(file.path("data", "tracks.rds"))

empty_tracks <- tracks |> 
  keep(is_empty) |> 
  names()

tracks_df <- bind_rows(tracks, .id = "artist_song") |> 
  inner_join(
    mutate(no1_uniq, 
           artist_song = glue("{no1_uniq$artist} - {no1_uniq$song}")), 
    by = "artist_song") |> 
  mutate(duration_s = duration_ms / 1000,
         decade = str_sub(first_no1_date, 1, 4) |> 
           as.numeric() %/% 10 * 10)

tracks_df <- tracks_df |> 
  select(artist_song, artist, song, first_no1_date, decade, duration_s, href, id)

write_rds(tracks_df, file.path("data", "no1-tracks-prepared.rds"))
write_csv(tracks_df, file.path("data", "no1-tracks-prepared.csv"))

