# Preprocessing

data =read.csv("D:/IPB/Semester 6/Data Mining/Proyek djawir coy/data fix/anime_cleaned.csv")
df = data.frame(data[,c(1:2,7:9,14:21,23,28:29)])
#data train
dt = data.frame(data[,c(7:9,14:21,23,28:29)])
#clean data
dt = dt[dt$genre != "Hentai",]
dt = dt[dt$rating != "Rx - Hentai",]
dt = dt[dt$rating != "R+ - Mild Nudity",]
dt

#fixed data
write.csv(dt,"fxd.csv")

#cek Missing Value
library(mice)
#cek total missing value per atribut
print(sum(dt$type==""))
print(sum(dt$source==""))
print(sum(dt$episodes==""))
print(sum(dt$rating==""))
print(sum(dt$score==""))
print(sum(dt$score_by==""))
print(sum(dt$rank==""))
print(sum(dt$popularity==""))
print(sum(dt$members==""))
print(sum(dt$favorites==""))
print(sum(dt$premiered==""))
print(sum(dt$studio==""))
print(sum(dt$genre==""))

#Mengubah data kosong menjadi NA
dt$premiered[dt$premiered==""] =NA
dt$genre[dt$genre==""] =NA

#cek total missing value
md.pattern(dt)

#Visualisasi data numerik
summary(dt$episode)
summary(dt$score)
summary(dt$scored_by)
summary(dt$rank)
summary(dt$popularity)
summary(dt$members)
summary(dt$favorites)

#Visualisasi data nominal
barplot(table(dt$type),main= "Histogram Type", ylab="Frequency", col="maroon")
barplot(table(dt$source),main= "Histogram source", ylab="Frequency", col="maroon")
barplot(table(dt$duration),main= "Histogram duration", ylab="Frequency", col="maroon")
barplot(table(dt$rating),main= "Histogram rating", ylab="Frequency", col="maroon")
barplot(table(dt$premiered),main= "Histogram premiered", ylab="Frequency", col="maroon")
barplot(table(dt$studio),main= "Histogram studio", ylab="Frequency", col="maroon")
barplot(table(dt$genre),main= "Histogram genre", ylab="Frequency", col="maroon")
