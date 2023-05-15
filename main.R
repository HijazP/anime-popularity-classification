# Eksplorasi
## Membaca dataset
data <- read.csv("./dataset/anime_cleaned.csv")

# Preprocessing
## Feature selection
df <- data.frame(data[,c(1:2,7:9,14:21,23,28:29)])

## Menghapus record yang tidak perlu
count_genre_hentai <- sum(df$genre == "Hentai")
count_genre_hentai
count_rating_nudity <- sum(df$rating == "R+ - Mild Nudity")
count_rating_nudity
count_rating_hentai <- sum(df$rating == "Rx - Hentai")
count_rating_hentai
total_deleted_records <- count_genre_hentai + count_rating_nudity + count_rating_hentai
df <- df[df$genre != "Hentai", ]
df <- df[df$rating != "R+ - Mild Nudity", ]
df <- df[df$rating != "Rx - Hentai", ]