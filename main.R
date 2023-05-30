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


# 1.) Eksplorasi
## Membaca dataset
data_raw <- read.csv("./dataset/anime_cleaned.csv")

# 2.) Preprocessing
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

# 1 & 2 cont.) Praproses lanjutan dan eksplorasi menggunakan Weka
## Menghapus id dan tiitle
df_clean <- data.frame(df_raw[,c(3:16)])
## Menyimpan data bersih untuk praproses lanjutan
## dan eksplorasi menggunakan Weka
write.csv(df_clean, "./dataset/data_clean.csv")

## Membaca data yang sudah dibersihkan
data <- read.csv("./dataset/data_clean.csv")
df <- data.frame(data)
df <- df[, -1]

## Memeriksa missing value pada setiap atribut
checkMissingValue(df)

## Memeriksa missing value pada atribut rank
print(sum(is.na(df$rank))) # Ada 2 buah records yang bernilai NA
print(sum(df$rank == ""))
print(which(is.na(df$rank)))
df$rank[2316] <- 11453
df <- df[-1822, ]

## Memeriksa missing value pada atribut genre
print(sum(is.na(df$genre))) # Tidak ada genre yang bernilai NA
print(sum(df$genre == "")) # Ada 4 buah records dengan genre yang bersifat empty
print(which(df$genre == "")) # Mengecek indeks dari records yang genre nya empty
#### -> daftar index: 2056, 2861, 4419, dan 5476
#### cek judul -> df_raw$title[index]

## Mengisi missing value pada atribut genre
### Pengisian dilakukan manual dengan mengambil referensi di internet
df$genre[2056] <- "Drama, Fantasy" # Title : Genbanojou
df$genre[2861] <- "School, Seinen" # Title : Match Shoujo
df$genre[4419] <- "Supernatural" # Title : Kyoto Animation: Megane-hen
df$genre[5746] <- "Kids" # Title : Season's Greetings from Dwarf

## Memeriksa missing value pada atribut premiered
print(sum(is.na(df$premiered)))
print(sum(df$premiered == ""))
print(which(df$premiered == ""))
#### Membuang atribut premiered
#### Alasan: Terdapat 3027 records dengan empty value di atribut premiered
#### sehingga lebih baik atribut nya tidak dipakai
df$premiered <- NULL

## Memeriksa missing value pada atribut studio
print(sum(is.na(df$studio)))
print(sum(df$studio == ""))

## Melakukan One-hot-encoding terhadap atribut genre
encoded_genre <- oneHotEncoding(df$genre, "Genre_", "genre")

## Melakukan filtering terhadap data pada atribut studio
### Skenario: terdapat multi-value pada records di atribut studio
studio_filter_output <- filterMultiVal(df$studio, "studio")

record_max_studio <- studio_filter_output[1]
sprintf("Record dengan studio penggarap terbanyak: ")
print(record_max_studio)
total_multi_val <- studio_filter_output[2]
message <- paste("Total records dengan studio penggarap lebih dari satu adalah:", total_multi_val, "buah")
print(message)
validation_record_max <- studio_filter_output[3]
#### Ada 406 buah records yang digarap lebih dari 1 buah studio
#### Persentase terhadap keseluruhan data: (406 / 5770) * 100 = 7.03 %
#### Keputusan: Hanya mengambil 1 studio saja dari beberapa studio yang ada
sprintf("Records dengan studio penggarap terbanyak: ")
print(validation_record_max) # studio penggarap terbanyak adalah 1 (setelah difilter)
filtered_studio <- data.frame(studio_filter_output[4])
filtered_studio <- data.frame(studio=filtered_studio[, 1])

## Merging dataframe genre dan studio ke dataframe awal
df$studio <- filtered_studio$studio
df$genre <- NULL
df$X <- NULL
df <- cbind(df, encoded_genre)
write.csv(df, "./dataset/data_clean_final.csv")


# =============================================================================================================


# 3.) Data Splitting
## Membaca data
dataset <- read.csv('./dataset/data_clean_final.csv')
df <- data.frame(dataset)
df$X <- NULL

## Diskritisasi variabel target popularity dengan equal binding, n = 3 (populer, cukup populer, dan tidak populer)
### Tentukan jumlah kelas yang diinginkan
n_classes <- 3

#### Dapatkan nilai minimum dan maksimum dari atribut "popularity"
min_value <- min(df$popularity)
max_value <- max(df$popularity)

### Hitung lebar setiap bin
bin_width <- ceiling((max_value - min_value) / n_classes)

### Buat cut points dengan menggunakan seq()
cut_points <- seq(min_value, max_value, by = bin_width)

### Tambahkan nilai maksimum ke cut points
cut_points <- append(cut_points, max_value + 1)

### Lakukan diskritisasi
df$popularity_category <- cut(df$popularity, breaks = cut_points, labels = c("populer", "cukup_populer", "kurang_populer"))

### Cetak hasil
print(df)

### Isi manual record dengan popularity category NA
which(!complete.cases(df))
df$popularity[3290]
df$popularity_category[3290] <- "populer"
which(!complete.cases(df))

### Hapus atribut popularity versi numerik
df$popularity <- NULL

## Proses splitting dengan rasio 80:20
set.seed(27)  # Mengatur seed untuk reproduktibilitas

# Mendapatkan jumlah baris yang akan menjadi data latih
n_train <- floor(0.8 * nrow(df))

# Menghasilkan indeks acak untuk data latih
train_indices <- sample(1:nrow(df), n_train)

# Mendefinisikan data latih dan data uji berdasarkan indeks yang dihasilkan
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]


# =============================================================================================================


# 4.) Training Model (type: Decision Tree Model)
library(rpart)

## cek total unique value pada setiap atribut [memprediksi ukuran tree]
# unique_counts <- sapply(df, function(x) length(unique(x)))
# sorted_counts <- sort(unique_counts, decreasing = TRUE)
# print(sorted_counts)


# atribut yang bisa dipakai: type (chr) + source (chr) + episodes (int) + score (num) + favorites (int) + scored_by (num) + rank (num)
# atribut yang tidak bisa dipakai [menyebabkan hang]: duration [unique = 245], studio [duration = 368]
# alasan: kemungkinan karena kedua atribut adalah atribut kategorik dengan jumlah unique value yang banyak
#         sehingga tree yang terbentuk terlalu kompleks dan memerlukan komputasi yang besar
# kemungkinan solusi: diskritisasi atau prunning tree [perlu konfigurasi di fungsi rpart ??]
# atribut yang menyebabkan akurasi 100% = members
# alasan: kemungkinan karena rentang nilai nya yang berbeda jauh dengan atribut lain
# solusi: normalisasi atribut members
rpart_formula <- as.formula(
  paste("popularity_category ~ rating + type + source + episodes + score + favorites +",
  paste(colnames(df)[12:52],
  collapse = " + ")))
# rpart_formula <- as.formula(
#   paste("popularity_category ~ rating + type + source + episodes + score + favorites + scored_by + rank + ",
#   paste(colnames(df)[12:52],
#   collapse = " + ")))
# rpart_formula <- as.formula(paste("popularity_category ~ ", paste(colnames(df)[12:52], collapse = " + ")))
# rpart_formula <- as.formula(paste("popularity_category ~ members +", paste(colnames(df)[12:52], collapse = " + ")))
# rpart_formula <- as.formula(paste("popularity_category ~ members"))
# rpart_formula <- as.formula(paste("popularity_category ~ scored_by"))
# rpart_formula <- as.formula(paste("popularity_category ~ rank"))
model <- rpart(rpart_formula, data = train_data)

print(model)

predicted_labels <- predict(model, newdata = test_data, type="class")

test_labels <- as.factor(test_data$popularity_category)
predicted_labels <- factor(predicted_labels, levels = levels(test_labels))

## 5.) Evaluasi model
library(caret)

confusion_matrix <- confusionMatrix(predicted_labels, test_labels)
accuracy <- confusion_matrix$overall['Accuracy']

print(confusion_matrix)
print(paste("Akurasi model: ", accuracy))

# install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(model)


# ALTERNATIF seleksi fitur
# # Menggunakan metode Information Gain untuk seleksi atribut
# ctrl <- trainControl(method = "none")
# info_gain <- caret::varImp(train_data, scale = FALSE, 
#                            options = list(xval = 0, repeats = 1, verboseIter = FALSE),
#                            model = NULL, oblique = FALSE, use.train = TRUE,
#                            split = FALSE, ...)
# 
# # Menampilkan hasil seleksi atribut
# print(info_gain)

# ALTERNATIF untuk improvisasi akurasi
## a. Backward Chaining untuk Feature Selection
## b. K-Fold Cross Validation untuk Splitting Data

# Akurasi terlau tinggi -> berpotensi overfitting
# Analisis: Diskritisasi popularitas, tree terlalu kompleks, perbedaan rentang nilai data
# Solusi: coba variasi seed yang berbeda, prunning, normalisasi fitur

# Tree terlalu sederhana
# Solusi: normalisasi fitur