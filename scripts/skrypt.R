#wlaczenie bibliotek 
library(tm) 
library (hunspell)
library (stringr)
library(lsa)
#punkt 6
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)
#punkt 8 
library(wordcloud)
#analiza ukrytej alokacji Dirichlet'a
library(topicmodels)
#algorytm rake
library(slowraker)

#zmiana katalogu roboczego 
workDir <- "G:\\R-Project06\\Projekty06"
setwd(workDir)

#definicja katalogow projektu

inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"
workspaceDir <- ".\\workspaces"
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspaceDir, showWarnings = FALSE)

#utworzenie korpusu dokumentow
corpusDir <- paste(
  inputDir, 
  "Literatura - oryginal",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)
View(corpus)

#wstepne przetwarzanie: usuniecie z podzia3u na akapity
pasteParagraphs <- content_transformer(function(x,char) paste(x, collapse = char))
corpus <- tm_map(corpus, pasteParagraphs, " ")

#wstepne przetwarzanie: stoplista
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
stoplistFile <- paste(
  inputDir, 
  "stopwords_pl.txt",
  sep = "\\"
)

stoplist <- readLines(stoplistFile, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

#wstepne przetwarzanie: usuniecie znakow psujacych macierze 
removeChar <- content_transformer(function(x,pattern) gsub(pattern, "", x))
corpus <- tm_map(corpus, removeChar, intToUtf8(8722))
corpus <- tm_map(corpus, removeChar, intToUtf8(190))
corpus <- tm_map(corpus, removeChar, "„")
corpus <- tm_map(corpus, removeChar, "”")
corpus <- tm_map(corpus, removeChar, "\"")
corpus <- tm_map(corpus, removeChar, "«")
corpus <- tm_map(corpus, removeChar, "—")
corpus <- tm_map(corpus, removeChar, "…")

#wstepne przetwarzanie: lemantyzacja
polish <- dictionary(lang="pl_PL")
lemmatize <- function(text) {
  simpleText <- str_trim(as.character(text))
  parsedText <- strsplit(simpleText, split = " ")
  newTextVec <- hunspell_stem(parsedText[[1]], dict = polish)
  for (i in 1:length(newTextVec)) {
    if (length(newTextVec[[i]]) == 0) newTextVec[i] <- parsedText[[1]][i]
    if (length(newTextVec[[i]]) > 1) newTextVec[i] <- newTextVec[[i]][1]
  }
  newText <- paste(newTextVec, collapse = " ")
  return(newText)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))
#wstepne przetwarzanie: usuniecie rozszerzen
cutExtensions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", replacement = "", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)
#eksport korpusu przetworzonego do plików tekstowych
preprocessedDir <- paste(
  outputDir,
  "\\",
  "Literatura - przetworzone",
  sep = ""
)
dir.create(preprocessedDir, showWarnings = FALSE)
writeCorpus(corpus, path = preprocessedDir)

inspect(corpus)

#analiza glownych skladowych - pca 

#Macierz czestosci - PUNKT 4          
#utworzenie korpusu dokmentow
corpusDir <- paste(
  inputDir, 
  "Literatura - przetworzone",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#usuniecie rozszezen z nazw plikow w korpusie
cutExtensions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", replacement = "", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)

#utworzenie macierzy czestosci
tdmTfAll <- TermDocumentMatrix(corpus)
dtmTfAll <- DocumentTermMatrix(corpus)
tdmBinAll <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightBin
  )
)
tdmTfidfAll <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(2,19)
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,14)
    )
  )
)
dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,19)
    )
  )
)
dtmTfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(2,14)
    )
  )
)

#konwersja macirzy rzadkich do macierzy klasycznch
tdmTfAllMatrix <- as.matrix(tdmTfAll)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBounds)
dtmTfBoundsMAtrix <- as.matrix(dtmTfBounds)

#eksport macirzy czestosci do pliku .csv
matrixFile <- paste(
  outputDir, 
  "tdmTfidf.csv",
  sep = "\\"
)
write.table(
  tdmTfidfAllMatrix,
  file = matrixFile,
  sep = ";",
  dec = ",",
  col.names = NA
)

matrixFile1 <- paste(
  outputDir, 
  "tdmTfBoundsMatrix(2,19).csv",
  sep = "\\"
)
write.table(
  tdmTfBoundsMatrix,
  file = matrixFile1,
  sep = ";",
  dec = ",",
  col.names = NA
)

matrixFile2 <- paste(
  outputDir, 
  "tdmTfidfBounds(2,14).csv",
  sep = "\\"
)
write.table(
  tdmTfidfBoundsMatrix,
  file = matrixFile2,
  sep = ";",
  dec = ",",
  col.names = NA
)

# Redukcja wymiarow - PUNKT 5
# PCA - analiza glownych skladowych

#analiza glownych skladowych
pca <- prcomp(dtmTfidfBounds)
x <- pca$x[,1]
y <- pca$x[,2]

#przygotowanie legendy
legend <- paste(
  paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""),
  rownames(dtmTfidfBoundsMatrix),
  sep = "<-"
)

#wykres dokumentow w przestrzeni dwuwymiarowej
plot(
  x,
  y,
  xlim = c(-0.30,0.25),
  ylim = c(-0.08, 0.1),
  xlab="Wspó³rzêdna syntetyczna 1", 
  ylab="Wspó³rzêdna syntetyczna 2",
  main="Analiza g³ównych sk³adowych", 
  col = "red"
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = "orange"
)
legend("bottom", legend, cex=.7, text.col = "blue")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "\\",
  "pca.png",
  sep = ""
)
png(file = plotFile)
plot(
  x,
  y,
  ylim = c(-0.25,0.25),
  xlab="Wspó³rzêdna syntetyczna 1", 
  ylab="Wspó³rzêdna syntetyczna 2",
  main="Analiza g³ównych sk³adowych", 
  col = "blue"
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = "blue"
)
legend("bottom", legend, cex=.65, text.col = "blue")
dev.off()

# L S A 
#analiza ukrytych wymiarow semantycznych (dekompozycja wg. wartosci osobliwych)
lsa <- lsa(tdmTfidfBoundsMatrix)
lsa$tk #odpowiednik macierzy U, wspó³rzêdne wyrazów
lsa$dk #odpowiednik macierzy V, wspó³rzêdne dokumentów
lsa$sk #odpowiednik macierzy D, znaczenie sk³adowych

#przygotowanie danych do wykresu
coordTerms <- lsa$tk%*%diag(lsa$sk)
coorDocs <- lsa$dk%*%diag(lsa$sk)

termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantTerms <- names(tail(sort(termsImportance),25))

#zale¿nie od preferencji wybrac mozna importantTerms jak i terms znalezione przez nas, gdy¿ jest ona bardziej wiarygodna
coordTerms <- coordTerms[importantTerms,]
legend <- paste(paste("d", 1:20, sep = ""), rownames(coorDocs), sep = "<-")
x1 <- coorDocs[,1]
y1 <- coorDocs[,2]
x2 <- coordTerms[,1]
y2 <- coordTerms[,2]

#wykres dokumentow i wybranych slow w przestrzeni dwuwymiatowej2222
options(scipen = 5)
plot(
  x1, 
  y1, 
  xlim = c(-0.2,0.05),
  #ylim = c(,),
  pch = 1, 
  col = "blue"
)
points(
  x2, 
  y2, 
  pch = 2, 
  col = "red"
)
text(
  x1, 
  y1, 
  paste("d", 1:20, sep = ""), 
  col = "blue",
  pos = 4
)
text(
  x2, 
  y2, 
  rownames(coordTerms), 
  col = "red",
  pos = 4
)

legend("topleft", legend, cex = 0.7, text.col = "blue")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir, 
  "lsaTerms.png",
  sep = "\\"
)
png(file = plotFile)
options(scipen = 5)
plot(
  x1, 
  y1, 
  xlim = c(-0.2,0.05),
  #ylim = c(,),
  pch = 1, 
  col = "blue"
)
points(
  x2, 
  y2, 
  pch = 2, 
  col = "red"
)
text(
  x1, 
  y1, 
  paste("d", 1:20, sep = ""), 
  col = "blue",
  pos = 4
)
text(
  x2, 
  y2, 
  rownames(coordTerms), 
  col = "red",
  pos = 4
)

legend("topleft", legend, cex = 0.7, text.col = "blue")
dev.off()

# PUNKT 6
#analiza skupien
##metoda hierarchiczna
#parametry metody:
# 1. macierz czestosci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odleglosci (euclidean, jaccard, cosine)
# 3. sposob wyznaczania odleglosci pomiedzy skupieniami (single, complete, ward.D2)

# najelpsze metody : cosine i ward.D2

par(mai = c(1,2,1,1))  #marginesy 
nDocuments = 20
#eksperyment 1
distMatrix1 <- dist(dtmTfAllMatrix, method = "euclidean") #te metode mozemy zmienic
hclust1 <- hclust(distMatrix1, method = "ward.D2") #te metode mozemy zmienic
plot(hclust1)
barplot(hclust1$height, names.arg = 18:1) # po tym okreslic ile nClusters 
nClusters1 = 5
clusters1 <- cutree(hclust1, k=nClusters1)
clustersMatrix1 <- matrix(0,nDocuments,nClusters1)
rownames(clustersMatrix1) <- names(clusters1)
for (i in 1:nDocuments) {
  clustersMatrix1[i, clusters1[i]] <- 1
}
corrplot(clustersMatrix1)
dendrogram1 <- as.dendrogram(hclust1)
coloredDendrogram1 <- color_branches(dendrogram1, h = 100)
plot(coloredDendrogram1)
#alternatywne wersje dendrogramow
par(mar=c(16,5,1,1), mgp=c(4,2.5,0))
coloredDendrogram1%>%set("labels_cex", 0.8)%>%plot()
par(mar=c(5,1,1,16), mgp=c(4,2.5,0))
coloredDendrogram1%>%set("labels_cex", 0.9)%>%plot(horiz = T)
par(mai = c(1,2,1,1))

#eksperyment 2
distMatrix2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(distMatrix2, method = "ward.D2")
plot(hclust2)
barplot(hclust2$height, names.arg = 18:1)
nClusters2 = 3
clusters2 <- cutree(hclust2, k=nClusters2)
clustersMatrix2 <- matrix(0,nDocuments,nClusters2)
rownames(clustersMatrix2) <- names(clusters2)
for (i in 1:nDocuments) {
  clustersMatrix2[i, clusters2[i]] <- 1
}
corrplot(clustersMatrix2)
dendrogram2 <- as.dendrogram(hclust2)
coloredDendrogram2 <- color_branches(dendrogram2, h = 1.2)
plot(coloredDendrogram2)
#alternatywne wersje dendrogramu2
par(mar=c(16,5,1,1), mgp=c(4,2.5,0))
coloredDendrogram2%>%set("labels_cex", 0.8)%>%plot()
par(mar=c(5,1,1,16), mgp=c(4,2.5,0))
coloredDendrogram2%>%set("labels_cex", 0.9)%>%plot(horiz = T)
par(mai = c(1,2,1,1))

#porownanie wynikow eksperymentow
Bk_plot(
  dendrogram1,
  dendrogram2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Index Fawlkes'a Mallows'a",
  ylab = "Index Fawlkes'a Mallows'a"
)

##metoda niehierarchiczna
#parametry metody:
# 1. macierz czesttosci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. zakladana liczba skupien

#eksperyment 3
nClusters3 <- 3
kmeans3 <- kmeans(dtmTfidfBounds, centers = nClusters3)
clustersMatrix3 <- matrix(0,nDocuments,nClusters3)
rownames(clustersMatrix3) <- names(kmeans3$cluster)
for (i in 1:nDocuments) {
  clustersMatrix3[i, kmeans3$cluster[i]] <- 1
}
corrplot(clustersMatrix3)

#eksperyment 4
nClusters4 <- 5
kmeans4 <- kmeans(dtmTfidfBounds, centers = nClusters4)
clustersMatrix4 <- matrix(0,nDocuments,nClusters4)
rownames(clustersMatrix4) <- names(kmeans4$cluster)
for (i in 1:nDocuments) {
  clustersMatrix4[i, kmeans4$cluster[i]] <- 1
}
corrplot(clustersMatrix4)

#podzia³ obiektów na skupienia przy zadanej liczbie klas - metoda hierarchiczna
#eksperyment 5
clusters5 <- cutree(hclust1, k = 4)
clustersMatrix5 <- matrix(0, 20, 4)
rownames(clustersMatrix5) <- names(clusters5)
for (i in 1:20){
  clustersMatrix5[i,clusters5[i]] <- 1
}
corrplot(clustersMatrix5)

#eksperyment 6
clusters6 <- cutree(hclust1, k = 3)
clustersMatrix6 <- matrix(0, 20, 3)
rownames(clustersMatrix6) <- names(clusters6)
for (i in 1:20){
  clustersMatrix6[i,clusters6[i]] <- 1
}
corrplot(clustersMatrix6)


#wspolczynnik zbieznosci podzialow przy zadanej liczbie skupien

#porownanie experymentu 1 i 3 
randEx1Ex3 <- randIndex(clusters1, kmeans3$cluster, F)
randEx1Ex3

#porownanie eksperymentu 2 i 3 
randEx2Ex3 <- randIndex(clusters2, kmeans3$cluster, F)
randEx2Ex3

#analiza ukrytej alokacji Dirichlet'a
nWords <- ncol(dtmTfAll)
# tyle ile mamy roznych dziedzin = 5 
nTopics <- 5
lda <- LDA(
  dtmTfAll,
  k = nTopics,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100, 
    iter = 3000
  )
)

#uzyta do oceny jakosci 
perplexity <- perplexity(lda, dtmTfAll)
results <- posterior(lda)

#prezentacja tematow
#prezentacja tematów
par(mai = c(1,2,1,1))
topic1 <- head(sort(results$terms[1,], decreasing = TRUE), 20)
barplot(
  rev(topic1), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 1 - Stanis³aw Lem / Tolkien",
  xlab = "Prawdopodobienstwo",
  col = "orange"
)
topic2 <- head(sort(results$terms[2,], decreasing = TRUE), 20)
barplot(
  rev(topic2), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 2 - WiedŸmin",
  xlab = "Prawdopodobienstwo",
  col = "turquoise"
)
topic3 <- head(sort(results$terms[3,], decreasing = TRUE), 20)
barplot(
  rev(topic3), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 3 - Sienkiewicz - Trylogia",
  xlab = "Prawdopodobienstwo",
  col = "violet"
)
topic4 <- head(sort(results$terms[4,], decreasing = TRUE), 20)
barplot(
  rev(topic4), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 4 - Tolkien / Stanis³aw Lem",
  xlab = "Prawdopodobienstwo",
  col = "lightskyblue"
)
topic5 <- head(sort(results$terms[5,], decreasing = TRUE), 20)
barplot(
  rev(topic5), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 5 - Antyk",
  xlab = "Prawdopodobienstwo",
  col = "darkseagreen"
)

#prezentacja dokumentów
document1 <- results$topics[1,]
barplot(
  rev(document1), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[1],
  xlab = "Prawdopodobienstwo",
  col = "darkseagreen"
)

document2 <- results$topics[2,]
barplot(
  rev(document2), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[2],
  xlab = "Prawdopodobienstwo",
  col = "darkseagreen"
)

document3 <- results$topics[3,]
barplot(
  rev(document3), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[3],
  xlab = "Prawdopodobienstwo",
  col = "darkseagreen"
)

document4 <- results$topics[4,]
barplot(
  rev(document4), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[4],
  xlab = "Prawdopodobienstwo",
  col = "darkseagreen"
)

document5 <- results$topics[5,]
barplot(
  rev(document5), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[5],
  xlab = "Prawdopodobienstwo",
  col = "lightskyblue"
)

document6 <- results$topics[6,]
barplot(
  rev(document6), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[6],
  xlab = "Prawdopodobienstwo",
  col = "orange"
)

document7 <- results$topics[7,]
barplot(
  rev(document7), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[7],
  xlab = "Prawdopodobienstwo",
  col = "lightskyblue"
)

document8 <- results$topics[8,]
barplot(
  rev(document8), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[8],
  xlab = "Prawdopodobienstwo",
  col = "orange"
)

document9 <- results$topics[9,]
barplot(
  rev(document9), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[9],
  xlab = "Prawdopodobienstwo",
  col = "violet"
)

document10 <- results$topics[10,]
barplot(
  rev(document10), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[10],
  xlab = "Prawdopodobienstwo",
  col = "violet"
)

document11 <- results$topics[11,]
barplot(
  rev(document11), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobienstwo",
  col = "violet"
)

document12 <- results$topics[12,]
barplot(
  rev(document12), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[12],
  xlab = "Prawdopodobienstwo",
  col = "violet"
)

document13 <- results$topics[13,]
barplot(
  rev(document13), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[13],
  xlab = "Prawdopodobienstwo",
  col = "lightskyblue"
)

document14 <- results$topics[14,]
barplot(
  rev(document14), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[14],
  xlab = "Prawdopodobienstwo",
  col = "lightskyblue"
)

document15 <- results$topics[15,]
barplot(
  rev(document15), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[15],
  xlab = "Prawdopodobienstwo",
  col = "orange"
)

document16 <- results$topics[16,]
barplot(
  rev(document16), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[16],
  xlab = "Prawdopodobienstwo",
  col = "lightskyblue"
)

document17 <- results$topics[17,]
barplot(
  rev(document17), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[17],
  xlab = "Prawdopodobienstwo",
  col = "turquoise"
)

document18 <- results$topics[18,]
barplot(
  rev(document18), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[18],
  xlab = "Prawdopodobienstwo",
  col = "orange"
)

document19 <- results$topics[19,]
barplot(
  rev(document19), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[19],
  xlab = "Prawdopodobienstwo",
  col = "turquoise"
)

document20 <- results$topics[20,]
barplot(
  rev(document20), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[20],
  xlab = "Prawdopodobienstwo",
  col = "turquoise"
)


#wagi s³ow 
##waga tf jako miara waznosci slow
keywordsTf1 <- head(sort(dtmTfAllMatrix[1,], decreasing = T))
keywordsTf1

keywordsTf2 <- head(sort(dtmTfAllMatrix[2,], decreasing = T))
keywordsTf2

keywordsTf3 <- head(sort(dtmTfAllMatrix[3,], decreasing = T))
keywordsTf3

keywordsTf4 <- head(sort(dtmTfAllMatrix[4,], decreasing = T))
keywordsTf4

keywordsTf5 <- head(sort(dtmTfAllMatrix[5,], decreasing = T))
keywordsTf5

keywordsTf6 <- head(sort(dtmTfAllMatrix[6,], decreasing = T))
keywordsTf6

keywordsTf7 <- head(sort(dtmTfAllMatrix[7,], decreasing = T))
keywordsTf7

keywordsTf8 <- head(sort(dtmTfAllMatrix[8,], decreasing = T))
keywordsTf8

keywordsTf9 <- head(sort(dtmTfAllMatrix[9,], decreasing = T))
keywordsTf9

keywordsTf10 <- head(sort(dtmTfAllMatrix[10,], decreasing = T))
keywordsTf10

keywordsTf11 <- head(sort(dtmTfAllMatrix[11,], decreasing = T))
keywordsTf11

keywordsTf12 <- head(sort(dtmTfAllMatrix[12,], decreasing = T))
keywordsTf12

keywordsTf13 <- head(sort(dtmTfAllMatrix[13,], decreasing = T))
keywordsTf13

keywordsTf14 <- head(sort(dtmTfAllMatrix[14,], decreasing = T))
keywordsTf14

keywordsTf15 <- head(sort(dtmTfAllMatrix[15,], decreasing = T))
keywordsTf15

keywordsTf16 <- head(sort(dtmTfAllMatrix[16,], decreasing = T))
keywordsTf16

keywordsTf17 <- head(sort(dtmTfAllMatrix[17,], decreasing = T))
keywordsTf17

keywordsTf18 <- head(sort(dtmTfAllMatrix[18,], decreasing = T))
keywordsTf18

keywordsTf19 <- head(sort(dtmTfAllMatrix[19,], decreasing = T))
keywordsTf19

keywordsTf20 <- head(sort(dtmTfAllMatrix[20,], decreasing = T))
keywordsTf20


##waga tfidf jako miara waznosci slow
keywordsTfidf1 <- head(sort(dtmTfidfBoundsMatrix[1,], decreasing = T))
keywordsTfidf1

keywordsTfidf2 <- head(sort(dtmTfidfBoundsMatrix[2,], decreasing = T))
keywordsTfidf2

keywordsTfidf3 <- head(sort(dtmTfidfBoundsMatrix[3,], decreasing = T))
keywordsTfidf3

keywordsTfidf4 <- head(sort(dtmTfidfBoundsMatrix[4,], decreasing = T))
keywordsTfidf4

keywordsTfidf5 <- head(sort(dtmTfidfBoundsMatrix[5,], decreasing = T))
keywordsTfidf5

keywordsTfidf6 <- head(sort(dtmTfidfBoundsMatrix[6,], decreasing = T))
keywordsTfidf6

keywordsTfidf7 <- head(sort(dtmTfidfBoundsMatrix[7,], decreasing = T))
keywordsTfidf7

keywordsTfidf8 <- head(sort(dtmTfidfBoundsMatrix[8,], decreasing = T))
keywordsTfidf8

keywordsTfidf9 <- head(sort(dtmTfidfBoundsMatrix[9,], decreasing = T))
keywordsTfidf9

keywordsTfidf10 <- head(sort(dtmTfidfBoundsMatrix[10,], decreasing = T))
keywordsTfidf10

keywordsTfidf11 <- head(sort(dtmTfidfBoundsMatrix[11,], decreasing = T))
keywordsTfidf11

keywordsTfidf12 <- head(sort(dtmTfidfBoundsMatrix[12,], decreasing = T))
keywordsTfidf12

keywordsTfidf13 <- head(sort(dtmTfidfBoundsMatrix[13,], decreasing = T))
keywordsTfidf13

keywordsTfidf14 <- head(sort(dtmTfidfBoundsMatrix[14,], decreasing = T))
keywordsTfidf14

keywordsTfidf15 <- head(sort(dtmTfidfBoundsMatrix[15,], decreasing = T))
keywordsTfidf15

keywordsTfidf16 <- head(sort(dtmTfidfBoundsMatrix[16,], decreasing = T))
keywordsTfidf16

keywordsTfidf17 <- head(sort(dtmTfidfBoundsMatrix[17,], decreasing = T))
keywordsTfidf17

keywordsTfidf18 <- head(sort(dtmTfidfBoundsMatrix[18,], decreasing = T))
keywordsTfidf18

keywordsTfidf19 <- head(sort(dtmTfidfBoundsMatrix[19,], decreasing = T))
keywordsTfidf19

keywordsTfidf20 <- head(sort(dtmTfidfBoundsMatrix[20,], decreasing = T))
keywordsTfidf20

##prawdopodobienstwo w LDA jako miara waznoscislow

termsImportance1 <- c(results$topics[1,]%*%results$terms)
names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1, decreasing = TRUE))
keywordsLda1

termsImportance2 <- c(results$topics[2,]%*%results$terms)
names(termsImportance2) <- colnames(results$terms)
keywordsLda2 <- head(sort(termsImportance2, decreasing = TRUE))
keywordsLda2

termsImportance3 <- c(results$topics[3,]%*%results$terms)
names(termsImportance3) <- colnames(results$terms)
keywordsLda3 <- head(sort(termsImportance3, decreasing = TRUE))
keywordsLda3

termsImportance4 <- c(results$topics[4,]%*%results$terms)
names(termsImportance4) <- colnames(results$terms)
keywordsLda4 <- head(sort(termsImportance4, decreasing = TRUE))
keywordsLda4

termsImportance5 <- c(results$topics[5,]%*%results$terms)
names(termsImportance5) <- colnames(results$terms)
keywordsLda5 <- head(sort(termsImportance5, decreasing = TRUE))
keywordsLda5

termsImportance6 <- c(results$topics[6,]%*%results$terms)
names(termsImportance6) <- colnames(results$terms)
keywordsLda6 <- head(sort(termsImportance6, decreasing = TRUE))
keywordsLda6

termsImportance7 <- c(results$topics[7,]%*%results$terms)
names(termsImportance7) <- colnames(results$terms)
keywordsLda7 <- head(sort(termsImportance7, decreasing = TRUE))
keywordsLda7


termsImportance8 <- c(results$topics[8,]%*%results$terms)
names(termsImportance8) <- colnames(results$terms)
keywordsLda8 <- head(sort(termsImportance8, decreasing = TRUE))
keywordsLda8

termsImportance9 <- c(results$topics[9,]%*%results$terms)
names(termsImportance9) <- colnames(results$terms)
keywordsLda9 <- head(sort(termsImportance9, decreasing = TRUE))
keywordsLda9

termsImportance10 <- c(results$topics[10,]%*%results$terms)
names(termsImportance10) <- colnames(results$terms)
keywordsLda10 <- head(sort(termsImportance10, decreasing = TRUE))
keywordsLda10

termsImportance11 <- c(results$topics[11,]%*%results$terms)
names(termsImportance11) <- colnames(results$terms)
keywordsLda11 <- head(sort(termsImportance11, decreasing = TRUE))
keywordsLda11

termsImportance12 <- c(results$topics[12,]%*%results$terms)
names(termsImportance12) <- colnames(results$terms)
keywordsLda12 <- head(sort(termsImportance12, decreasing = TRUE))
keywordsLda12

termsImportance13 <- c(results$topics[13,]%*%results$terms)
names(termsImportance13) <- colnames(results$terms)
keywordsLda13 <- head(sort(termsImportance13, decreasing = TRUE))
keywordsLda13

termsImportance14 <- c(results$topics[14,]%*%results$terms)
names(termsImportance14) <- colnames(results$terms)
keywordsLda14 <- head(sort(termsImportance14, decreasing = TRUE))
keywordsLda14

termsImportance15 <- c(results$topics[15,]%*%results$terms)
names(termsImportance15) <- colnames(results$terms)
keywordsLda15 <- head(sort(termsImportance15, decreasing = TRUE))
keywordsLda15

termsImportance16 <- c(results$topics[16,]%*%results$terms)
names(termsImportance16) <- colnames(results$terms)
keywordsLda16 <- head(sort(termsImportance16, decreasing = TRUE))
keywordsLda16

termsImportance17 <- c(results$topics[17,]%*%results$terms)
names(termsImportance17) <- colnames(results$terms)
keywordsLda17 <- head(sort(termsImportance17, decreasing = TRUE))
keywordsLda17

termsImportance18 <- c(results$topics[18,]%*%results$terms)
names(termsImportance18) <- colnames(results$terms)
keywordsLda18 <- head(sort(termsImportance18, decreasing = TRUE))
keywordsLda18

termsImportance19 <- c(results$topics[19,]%*%results$terms)
names(termsImportance19) <- colnames(results$terms)
keywordsLda19 <- head(sort(termsImportance19, decreasing = TRUE))
keywordsLda19

termsImportance20 <- c(results$topics[20,]%*%results$terms)
names(termsImportance20) <- colnames(results$terms)
keywordsLda20 <- head(sort(termsImportance20, decreasing = TRUE))
keywordsLda20

##chmury tagow
par(mai = c(0,0,0,0))
wordcloud(corpus[1], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[2], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[3], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[4], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[5], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[6], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[7], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[8], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[9], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[10], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[11], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[12], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[13], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[14], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[15], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[16], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[17], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[18], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[19], max.words = 200, colors = brewer.pal(8,"PuOr"))

par(mai = c(0,0,0,0))
wordcloud(corpus[20], max.words = 200, colors = brewer.pal(8,"PuOr"))


##algorytm RAKE
text1 <- as.character(corpus[1])
rake1 <- slowrake(txt = text1, stem = FALSE, stop_pos = NULL)
print(rake1[[1]])

text2 <- as.character(corpus[10])
rake2 <- slowrake(txt = text2, stem = FALSE, stop_pos = NULL)
print(rake2[[1]])

text3 <- as.character(corpus[20])
rake3 <- slowrake(txt = text3, stem = FALSE, stop_pos = NULL)
print(rake3[[1]])

