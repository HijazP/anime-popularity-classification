# Fungsi One-Hot-Encoding
oneHotEncoding <- function(col, prefix, col_name) {
  unique_val <- unique(strsplit(col, ","))
  unique_val <- trimws(unlist(unique_val))
  unique_val <- unique(unique_val)
  unique_val_list <- as.list(unique_val)
  
  encoded_data <- data.frame(col)
  colnames(encoded_data) <- col_name
  for (val in unique_val) {
    encoded_data[[val]] <- 0
  }
  
  for (i in 1:nrow(encoded_data)) {
    for (val in unique_val) {
      encoded_data[i, val] <- ifelse(grepl(val, encoded_data[i, col_name]), 1, 0)
    }
  }
  
  encoded_data <- encoded_data[, -1]
  
  col_names = colnames(encoded_data)
  for (i in 1:length(col_names)) {
    col_names[i] <- paste0(prefix, col_names[i])
  }
  
  colnames(encoded_data) <- col_names
  encoded_data <- data.frame(encoded_data)
  return(encoded_data)
}


# Fungsi Filter Multi Value pada suatu Atribut
filterMultiVal <- function(col, col_name) {
  df_col <- data.frame(col)
  colnames(df_col) <- col_name
  
  df_col$total_elements <- sapply(strsplit(col, ","), length)
  max_row <- which.max(df_col$total_elements)
  total_rows_multi_val <- nrow(subset(df_col, total_elements >= 2))
  
  filtered_col <- sapply(strsplit(df_col[, 1], ","), `[`, 1)
  
  filtered_col <- data.frame(filtered_col)
  colnames(filtered_col) <- col_name
  
  filtered_col$total_elements <- sapply(strsplit(filtered_col[, 1], ","), length)
  
  max_row_filtered <- which.max(filtered_col$total_elements)
  
  return(list(df_col[max_row, ], total_rows_multi_val, filtered_col[max_row_filtered, ], filtered_col))
}

# Fungsi memeriksa missing value pada dataset
checkMissingValue <- function(df) {
  for (i in 1:ncol(df)) {
    message <- paste("atribut:", colnames(df[i]))
    print(message)
    message <- paste("missing value:", sum(is.na(df[, i])))
    print(message)
    message <- paste("empty value:", sum(df[, i] == ""))
    print(message)
    message <- paste("===================================")
    print(message)
  }
}


# Eksplorasi
## Membaca dataset
data_raw <- read.csv("./dataset/anime_cleaned.csv")

# Preprocessing
## Feature selection
df_raw <- data.frame(data_raw[,c(1:2,7:9,14:21,23,28:29)])

## Menghapus record yang tidak perlu
count_genre_hentai <- sum(df_raw$genre == "Hentai")
count_genre_hentai
count_rating_nudity <- sum(df_raw$rating == "R+ - Mild Nudity")
count_rating_nudity
count_rating_hentai <- sum(df_raw$rating == "Rx - Hentai")
count_rating_hentai
total_deleted_records <- count_genre_hentai + count_rating_nudity + count_rating_hentai
df_raw <- df_raw[df_raw$genre != "Hentai", ]
df_raw <- df_raw[df_raw$rating != "R+ - Mild Nudity", ]
df_raw <- df_raw[df_raw$rating != "Rx - Hentai", ]

# Praproses lanjutan dan eksplorasi menggunakan Weka
## Menghapus id dan tiitle
df_clean <- data.frame(df_raw[,c(3:16)])
## Menyimpan data bersih untuk praproses lanjutan
## dan eksplorasi menggunakan Weka
write.csv(df_clean, "./dataset/data_clean.csv")

# Membaca data yang sudah dibersihkan
data <- read.csv("./dataset/data_clean.csv")
df <- data.frame(data)
df <- df[, -1]

# Memeriksa missing value pada setiap atribut
checkMissingValue(df)

# Memeriksa missing value pada atribut rank
print(sum(is.na(df$rank))) # Ada 2 buah records yang bernilai NA
print(sum(df$rank == ""))
print(which(is.na(df$rank)))
df$rank[2316] <- 11453
df <- df[-1822, ]

# Memeriksa missing value pada atribut genre
print(sum(is.na(df$genre))) # Tidak ada genre yang bernilai NA
print(sum(df$genre == "")) # Ada 4 buah records dengan genre yang bersifat empty
print(which(df$genre == "")) # Mengecek indeks dari records yang genre nya empty
# -> daftar index: 2056, 2861, 4419, dan 5476
# cek judul -> df_raw$title[index]

# Mengisi missing value pada atribut genre
# Pengisian dilakukan manual dengan mengambil referensi di internet
df$genre[2056] <- "Drama, Fantasy" # Title : Genbanojou
df$genre[2861] <- "School, Seinen" # Title : Match Shoujo
df$genre[4419] <- "Supernatural" # Title : Kyoto Animation: Megane-hen
df$genre[5746] <- "Kids" # Title : Season's Greetings from Dwarf

# Memeriksa missing value pada atribut premiered
print(sum(is.na(df$premiered)))
print(sum(df$premiered == ""))
print(which(df$premiered == ""))
# Membuang atribut premiered
# Alasan: Terdapat 3027 records dengan empty value di atribut premiered
# sehingga lebih baik atribut nya tidak dipakai
df$premiered <- NULL

# Memeriksa missing value pada atribut studio
print(sum(is.na(df$studio)))
print(sum(df$studio == ""))

# Melakukan One-hot-encoding terhadap atribut genre
encoded_genre <- oneHotEncoding(df$genre, "Genre_", "genre")

# Melakukan filtering terhadap data pada atribut studio
# Skenario: terdapat multi-value pada records di atribut studio
studio_filter_output <- filterMultiVal(df$studio, "studio")

record_max_studio <- studio_filter_output[1]
sprintf("Record dengan studio penggarap terbanyak: ")
print(record_max_studio)
total_multi_val <- studio_filter_output[2]
message <- paste("Total records dengan studio penggarap lebih dari satu adalah:", total_multi_val, "buah")
print(message)
validation_record_max <- studio_filter_output[3]
# Ada 406 buah records yang digarap lebih dari 1 buah studio
# Persentase terhadap keseluruhan data: (406 / 5770) * 100 = 7.03 %
# Keputusan: Hanya mengambil 1 studio saja dari beberapa studio yang ada
sprintf("Records dengan studio penggarap terbanyak: ")
print(validation_record_max) # studio penggarap terbanyak adalah 1 (setelah difilter)
filtered_studio <- data.frame(studio_filter_output[4])
filtered_studio <- data.frame(studio=filtered_studio[, 1])

# Merging dataframe genre dan studio ke dataframe awal
df$studio <- filtered_studio$studio
df$genre <- NULL
df <- cbind(df, encoded_genre)

checkMissingValue(df)
