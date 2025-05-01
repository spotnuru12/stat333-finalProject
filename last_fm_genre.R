library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(progress)

# allowed genres; matches MSD Top-MAGD list from slideshow
LASTFM_API_KEY <- "c10f51598f6850c96dbe1e636e8042cf"
allowed_genres <- c(
  "Pop", "Rock","Electronic","Rap","Jazz","Latin","R&B","International",
  "Country","Reggae","Blues","Vocal","Folk","New Age"
) %>% tolower() %>% str_replace_all("/", " ")

# fetch first allowed Last.fm tag for a given track 
get_lastfm_genre <- function(artist, track) {
  url <- modify_url(
    "http://ws.audioscrobbler.com/2.0/",
    query = list(
      method  = "track.getTopTags",
      api_key = LASTFM_API_KEY,
      artist  = artist,
      track   = track,
      format  = "json"
    )
  )
  resp <- tryCatch({
    r <- GET(url); stop_for_status(r)
    content(r, as = "text", encoding = "UTF-8")
  }, error = function(e) NA_character_)
  if (is.na(resp)) return(NA_character_)
  
  j    <- fromJSON(resp, simplifyDataFrame = TRUE)
  tags <- j$toptags$tag
  if (is.null(tags) || nrow(as_tibble(tags))==0) return(NA_character_)
  
  tag_names <- tolower(tags$name) %>% str_replace_all("/", " ")
  matches   <- tag_names[tag_names %in% allowed_genres]
  if (length(matches)==0) return(NA_character_)
  matches[1]
}

# read in half-filled file; or empty 
df <- read_csv("df_filled_lastfm.csv")

# clean_title & search_artist cols 
if (!"clean_title" %in% names(df)) {
  df <- df %>%
    mutate(
      clean_title   = str_remove_all(Title, '"'),
      search_artist = map_chr(artist_list, ~ {
        a <- tryCatch(fromJSON(.x), error = function(e) primary_artist)
        paste(a, collapse = " & ")
      })
    )
}

# find which rows still need a tag
to_fill <- which(is.na(df$genre_final))

message("Need to fetch ", length(to_fill), " missing genres from Last.fm")

# progress bar \
pb <- progress_bar$new(
  total  = length(to_fill),
  format = " Fetching [:bar] :current/:total (:percent) ETA: :eta"
)

for (i in to_fill) {
  pb$tick()
  Sys.sleep(0.5)  # throttle
  df$genre_final[i] <- get_lastfm_genre(
    df$search_artist[i],
    df$clean_title[i]
  )
}

# check & save
df %>% count(has_genre = !is.na(genre_final)) %>% print()
write_csv(df, "billboard_top40_genre_lastfm_complete.csv")
