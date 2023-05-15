# Klasifikasi Popularitas Anime menggunakan Decision Tree
---
## Oleh Kelompok 8 Paralel 1 Mata Kuliah Data Mining 
<table>
    <tr>
        <th>Nama</th>
        <th>NIM</th>
    </tr>
    <tr>
        <td>Hanif Ali Ramdani</td>
        <td>G6401201004</td>
    </tr>
    <tr>
        <td>Daffa Fikri</td>
        <td>G6401201086</td>
    </tr>
    <tr>
        <td>Aysuka Ansari</td>
        <td>G6401201087</td>
    </tr>
</table>

---

Mengklasifikasikan popularitas dari suatu judul anime pada MyAnimeList dengan menggunakan atribut seperti rating, score, studio, genre, dan lain-lain.

## Tools
- R
- R studio
- Weka

## Dataset
Dataset yang diambil adalah dari Kaggle, dengan dataset [MyAnimeList Dataset](https://www.kaggle.com/datasets/azathoth42/myanimelist)
Dataset tersebut berisi 300k user, 14k anime metada, dan 80m rating dari MyAnimeList.net.
Dataset yang diambil adalah anime_cleande.csv, karena dataset tersebut lebih mudah terbaca di R daripada data raw AnimeList.csv.

## Eksplorasi
![kaggle](/img/kaggle.png)
Dataset tersebut diupdate 5 tahun terakhir oleh pengguna AZATHOTH, sehingga anime yang tercatat paling baru adalah antara 2018-2019.

- Membaca dataset
`data <- read.csv("./dataset/anime_cleaned.csv")`
![dataset](/img/r_read_data.png)

- Atribut

- Feature selection
`df <- data.frame(data[,c(1:2,7:9,14:21,23,28:29)])`
![df](/img/df.png)