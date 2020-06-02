#włączenie bibliotek 
library(tm) 
library (hunspell)
library (stringr)

#zmiana katalogu roboczego 
workDir <- "C:\\ProjektR\\Projekty06"
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

#wstepne przetwarzanie: usuni�cie z podzia�u na akapity
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
corpus <- tm_map(corpus, removeChar, "�")
corpus <- tm_map(corpus, removeChar, "�")
corpus <- tm_map(corpus, removeChar, "\"")
corpus <- tm_map(corpus, removeChar, "�")
corpus <- tm_map(corpus, removeChar, "�")
corpus <- tm_map(corpus, removeChar, "�")

#wstepne przetwarzanie: lemantyzacja
polish <- dictionary(lang="pl")
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

#eksport korpusu przetworzonego do plik�w tekstowych
preprocessedDir <- paste(
  outputDir,
  "\\",
  "Literatura - przetworzone",
  sep = ""
)
dir.create(preprocessedDir, showWarnings = FALSE)
writeCorpus(corpus, path = preprocessedDir)





#Macierz czestosci - PUNKT 4          
#za�adowanie bibliotek

#tego nie powinno byc
library(tm)
library(stringr)


#utworzenie korpusu dokment�w
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

#usuni�cie rozszerze� z nazw plik�w w korpusie
cutExtensions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", replacement = "", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)

#utworzenie macierzy cz�stosci
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

#eksport macirzy cz�sto�ci do pliku .csv
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
  file = matrixFile1,
  sep = ";",
  dec = ",",
  col.names = NA
)



#Redukcja wymiarow  -    PUNKT 5
# P C A : 
#tutaj zaczyna si� analiza glownych skladowych  (PCA)

#analiza g��wnych sk�adowych
pca <- prcomp(dtmTfidfBounds)
x <- pca$x[,1]
y <- pca$x[,2]

#przygotowanie legendy
legend <- paste(
  paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""),
  rownames(dtmTfidfBoundsMatrix),
  sep = "<-"
)

#wykres dokument�w w przestrzeni dwuwymiarowej
plot(
  x,
  y,
  xlim = c(-0.30,0.25),
  xlab="Wsp�rz�dna syntetyczna 1", 
  ylab="Wsp�rz�dna syntetyczna 2",
  main="Analiza g��wnych sk�adowych", 
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
  xlab="Wsp�rz�dna syntetyczna 1", 
  ylab="Wsp�rz�dna syntetyczna 2",
  main="Analiza g��wnych sk�adowych", 
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
#za�adowanie bibliotek
library(lsa)


#analiza ukrytych wymiar�w semantycznych (dekompozycja wg. warto�ci osobliwych)
lsa <- lsa(tdmTfidfBoundsMatrix)
lsa$tk #odpowiednik macierzy U, wsp�rz�dne wyraz�w
lsa$dk #odpowiednik macierzy V, wsp�rz�dne dokument�w
lsa$sk #odpowiednik macierzy D, znaczenie sk�adowych

#przygotowanie danych do wykresu
coordTerms <- lsa$tk%*%diag(lsa$sk)
coorDocs <- lsa$dk%*%diag(lsa$sk)

termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
terms <- c("achilles", "troja", "itaka", "pryjam", "italia", "wiedzmin", "geralt", "ciri", "yennefer", "jaskier", "szlachta", "zagloba", "michal", "waszmosc", "wasc", "frodo", "mordor", "gandalf", "hobbit", "baggins", "telefon", "kosmicznej", "robot", "helikopter", "astronomii")
importantTerms <- names(tail(sort(termsImportance),25))

#zale�nie od preferencji wybrac mozna importantTerms jak i terms znalezione przez nas, gdy� jest ona bardziej wiarygodna
coordTerms <- coordTerms[importantTerms,]
legend <- paste(paste("d", 1:20, sep = ""), rownames(coorDocs), sep = "<-")
x1 <- coorDocs[,1]
y1 <- coorDocs[,2]
x2 <- coordTerms[,1]
y2 <- coordTerms[,2]

#wykres dokument�w i wybranych s��w w przestrzeni dwuwymiatowej
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
  "lsa.png",
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

#w��czenie bibliotek
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)

#analiza skupie�
##metoda hierarchiczna
#parametry metody:
# 1. macierz cz�sto�ci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odleg�o�ci (euclidean, jaccard, cosine)
# 3. spos�b wyznaczania odleg�o�ci pomiedzy skupieniami (single, complete, ward.D2)

# najelpsze metody : cosine i ward.D2

par(mai = c(1,2,1,1))  #marginesy 
nDocuments = 19
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
#alternatywne wersje dendrogram�w
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

#por�wnanie wynik�w eksperyment�w
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
# 1. macierz cz�sto�ci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. zak�adana liczba skupie�

#eksperyment 3
nClusters3 <- 3
kmeans3 <- kmeans(dtmTfidfBounds, centers = nClusters3)
clustersMatrix3 <- matrix(0,nDocuments,nClusters3)
rownames(clustersMatrix3) <- names(kmeans3$cluster)
for (i in 1:nDocuments) {
  clustersMatrix3[i, kmeans3$cluster[i]] <- 1
}
corrplot(clustersMatrix3)

#wsp�czynnik zbie�no�ci podzia��w przy zadanej liczbie skupie�
##dla 3 skupie�
randEx2Ex3 <- randIndex(clusters2, kmeans3$cluster, F)
randEx2Ex3
randEx2Pattern <- randIndex(clusters2, pattern, F)
randEx2Pattern
randEx3Pattern <- randIndex(kmeans3$cluster, pattern, F)
randEx3Pattern






#Punkt 7 (wklejony kod, nic nie zmieniane) LDA 

#analiza ukrytej alokacji Dirichlet'a

#w��czenie bibliotek
library(topicmodels)



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

#prezentacja temat�w
par(mai = c(1,2,1,1))
topic1 <- head(sort(results$terms[1,], decreasing = TRUE), 20)
barplot(
  rev(topic1), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 1",
  xlab = "Prawdopodobie�stwo 1",
  col = "blue"
)
topic2 <- head(sort(results$terms[2,], decreasing = TRUE), 20)
barplot(
  rev(topic2), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 2",
  xlab = "Prawdopodobie�stwo 2",
  col = "red"
)
topic3 <- head(sort(results$terms[3,], decreasing = TRUE), 20)
barplot(
  rev(topic3), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 3",
  xlab = "Prawdopodobie�stwo 3",
  col = "violet"
)
topic4 <- head(sort(results$terms[4,], decreasing = TRUE), 20)
barplot(
  rev(topic4), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 4",
  xlab = "Prawdopodobie�stwo 4",
  col = "lightskyblue"
)
topic5 <- head(sort(results$terms[5,], decreasing = TRUE), 20)
barplot(
  rev(topic5), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 5",
  xlab = "Prawdopodobie�stwo 5",
  col = "darkseagreen"
)

#prezentacja dokument�w
document1 <- results$topics[1,]
barplot(
  rev(document1), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[1],
  xlab = "Prawdopodobie�stwo",
  col = "darkseagreen"
)

document2 <- results$topics[2,]
barplot(
  rev(document2), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[2],
  xlab = "Prawdopodobie�stwo",
  col = "darkseagreen"
)

document3 <- results$topics[3,]
barplot(
  rev(document3), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[3],
  xlab = "Prawdopodobie�stwo",
  col = "darkseagreen"
)

document4 <- results$topics[4,]
barplot(
  rev(document4), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[4],
  xlab = "Prawdopodobie�stwo",
  col = "darkseagreen"
)

document5 <- results$topics[5,]
barplot(
  rev(document5), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[5],
  xlab = "Prawdopodobie�stwo",
  col = "lightskyblue"
)

document6 <- results$topics[6,]
barplot(
  rev(document6), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[6],
  xlab = "Prawdopodobie�stwo",
  col = "orange"
)

document7 <- results$topics[7,]
barplot(
  rev(document7), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[7],
  xlab = "Prawdopodobie�stwo",
  col = "lightskyblue"
)

document8 <- results$topics[8,]
barplot(
  rev(document8), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[8],
  xlab = "Prawdopodobie�stwo",
  col = "orange"
)

document9 <- results$topics[9,]
barplot(
  rev(document9), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[9],
  xlab = "Prawdopodobie�stwo",
  col = "violet"
)

document10 <- results$topics[10,]
barplot(
  rev(document10), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[10],
  xlab = "Prawdopodobie�stwo",
  col = "violet"
)

document11 <- results$topics[11,]
barplot(
  rev(document11), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobie�stwo",
  col = "violet"
)

document12 <- results$topics[12,]
barplot(
  rev(document12), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[12],
  xlab = "Prawdopodobie�stwo",
  col = "violet"
)

document13 <- results$topics[13,]
barplot(
  rev(document13), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[13],
  xlab = "Prawdopodobie�stwo",
  col = "lightskyblue"
)

document14 <- results$topics[14,]
barplot(
  rev(document14), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[14],
  xlab = "Prawdopodobie�stwo",
  col = "lightskyblue"
)

document15 <- results$topics[15,]
barplot(
  rev(document15), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[15],
  xlab = "Prawdopodobie�stwo",
  col = "orange"
)

document16 <- results$topics[16,]
barplot(
  rev(document16), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[16],
  xlab = "Prawdopodobie�stwo",
  col = "lightskyblue"
)

document17 <- results$topics[17,]
barplot(
  rev(document17), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[17],
  xlab = "Prawdopodobie�stwo",
  col = "turquoise"
)

document18 <- results$topics[18,]
barplot(
  rev(document18), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[18],
  xlab = "Prawdopodobie�stwo",
  col = "orange"
)

document19 <- results$topics[19,]
barplot(
  rev(document19), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[19],
  xlab = "Prawdopodobie�stwo",
  col = "turquoise"
)

document20 <- results$topics[20,]
barplot(
  rev(document20), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[20],
  xlab = "Prawdopodobie�stwo",
  col = "turquoise"
)







#P U N K T    8     (do ) 
#w��czenie bibliotek
library(wordcloud)



#dla pierwszego dokumentu
##waga tf jako miara wa�no�ci s��w
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


##waga tfidf jako miara wa�no�ci s��w
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

##prawdopodobie�stwo w LDA jako miara wa�no�ci s��w 
#(tu dopisze jak bedziemy mieli ogarniete LDA, bo nie bardzo moge to wytestowac teraz)
termsImportance1 <- c(results$topics[1,]%*%results$terms)
names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1, decreasing = T))
keywordsLda1

termsImportance1 <- c(results$topics[1,]%*%results$terms)
names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1, decreasing = T))
keywordsLda1


##chmury tag�w
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
