# Eksplorasi
## Membaca dataset
data <- read.csv("./dataset/anime_cleaned.csv")

# Preprocessing
## Feature selection
df <- data.frame(data[,c(1:2,7:9,14:21,23,28:29)])
