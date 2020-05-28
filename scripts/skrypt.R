#wÅ‚Ä…czenie bibliotek 
library(tm) 
library (hunspell)
library (stringr)

#zmiana katalogu roboczego 
workDir <- "C:\\Users\\Beniamin\\Desktop\\Git\\Projekty06"
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

#wstepne przetwarzanie: usuniêcie z podzia³u na akapity
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

#wstepne przetwarzanie: usuniecie em dash i 3/4
removeChar <- content_transformer(function(x,pattern) gsub(pattern, "", x))
corpus <- tm_map(corpus, removeChar, intToUtf8(8722))
corpus <- tm_map(corpus, removeChar, intToUtf8(190))
corpus <- tm_map(corpus, removeChar, "„")
corpus <- tm_map(corpus, removeChar, "”")
corpus <- tm_map(corpus, removeChar, "«")



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





#Macierz czestosci - PUNKT 4          
#za³adowanie bibliotek
library(tm)
library(stringr)


#utworzenie korpusu dokmentów
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

#usuniêcie rozszerzeñ z nazw plików w korpusie
cutExtensions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", replacement = "", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)

#utworzenie macierzy czêstosci
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
      global = c(2,16)
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(2,16)
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

#eksport macirzy czêstoœci do pliku .csv
matrixFile <- paste(
  outputDir, 
  "tdmTfidf(2,16).csv",
  sep = "\\"
)
write.table(
 tdmTfidfBoundsMatrix,
 file = matrixFile,
 sep = ";",
 dec = ",",
 col.names = NA
)





#Redukcja wymiarow  -    PUNKT 5
# P C A : 
#tutaj zaczyna siê analiza glownych skladowych  (PCA)

#analiza g³ównych sk³adowych
pca <- prcomp(dtmTfidfBounds)
x <- pca$x[,1]
y <- pca$x[,2]

#przygotowanie legendy
legend <- paste(
  paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""),
  rownames(dtmTfidfBoundsMatrix),
  sep = "<-"
)

#wykres dokumentów w przestrzeni dwuwymiarowej
plot(
  x,
  y,
  #xlim = c(-0.5,-0.2),
  #ylim = c(-0.2,0.1),
  xlab="Wspó³rzêdna syntetyczna 1", 
  ylab="Wspó³rzêdna syntetyczna 2",
  main="Analiza g³ównych sk³adowych", 
  col = "orange"
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = "orange"
)
legend("bottom", legend, cex=.5, text.col = "orange")

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
  xlab="Wspó³rzêdna syntetyczna 1", 
  ylab="Wspó³rzêdna syntetyczna 2",
  main="Analiza g³ównych sk³adowych", 
  col = "orange"
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = "orange"
)
legend("bottom", legend, cex=.65, text.col = "orange")
dev.off()







# L S A    to generuje diagram, ale 1. sa jakie bledy w kodzie 2. nie wiem czy ten diagram wartosciowy
#za³adowanie bibliotek
library(lsa)


#analiza ukrytych wymiarów semantycznych (dekompozycja wg. wartoœci osobliwych)
lsa <- lsa(tdmTfidfBoundsMatrix)
lsa$tk #odpowiednik macierzy U, wspó³rzêdne wyrazów
lsa$dk #odpowiednik macierzy V, wspó³rzêdne dokumentów
lsa$sk #odpowiednik macierzy D, znaczenie sk³adowych

#przygotowanie danych do wykresu
coordTerms <- lsa$tk%*%diag(lsa$sk)
coorDocs <- lsa$dk%*%diag(lsa$sk)
#Linijke nizej trzeba zmienic! albo nie. cos z tym termsImportance sie robi. 

# <- c("harry", "czarodziej", "dumbledore", "hermiona", "ron", "komnata", "powiedzieæ", "chcieæ", "dowiadywaæ", "albus", "syriusz", "lupin", "umbridge", "edmund", "kaspian", "³ucja", "czarownica", "piotr", "zuzanna", "aslana", "narnii", "baron", "dziecko", "wyspa", "bell", "edward", "wampir", "jacob")
termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantTerms <- names(tail(sort(termsImportance),25))
coordTerms <- coordTerms[terms,]
coordTerms <- coordTerms[importantTerms,]
legend <- paste(paste("d", 1:19, sep = ""), rownames(coorDocs), sep = "<-")
x1 <- coorDocs[,1]
y1 <- coorDocs[,2]
x2 <- coordTerms[,1]
y2 <- coordTerms[,2]

#wykres dokumentów i wybranych s³ów w przestrzeni dwuwymiatowej
options(scipen = 5)
plot(
  x1, 
  y1, 
  xlim = c(-0.2,0.05),
  #ylim = c(,),
  pch = 1, 
  col = "orange"
)
points(
  x2, 
  y2, 
  pch = 2, 
  col = "brown"
)
text(
  x1, 
  y1, 
  paste("d", 1:19, sep = ""), 
  col = "orange",
  pos = 4
)
text(
  x2, 
  y2, 
  rownames(coordTerms), 
  col = "brown",
  pos = 4
)
legend("bottomleft", legend, cex = 0.7, text.col = "orange")

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
  col = "orange"
)
points(
  x2, 
  y2, 
  pch = 2, 
  col = "brown"
)
text(
  x1, 
  y1, 
  paste("d", 1:19, sep = ""), 
  col = "orange",
  pos = 4
)
text(
  x2, 
  y2, 
  rownames(coordTerms), 
  col = "brown",
  pos = 4
)
legend("bottomleft", legend, cex = 0.5, text.col = "orange")
dev.off()


# PUNKT 6  (poki co wklejony kod, nic nie zmieniane)

#w³¹czenie bibliotek
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)

#analiza skupieñ
##metoda hierarchiczna
#parametry metody:
# 1. macierz czêstoœci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odleg³oœci (euclidean, jaccard, cosine)
# 3. sposób wyznaczania odleg³oœci pomiedzy skupieniami (single, complete, ward.D2)

par(mai = c(1,2,1,1))
nDocuments = 19
#eksperyment 1
distMatrix1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(distMatrix1, method = "ward.D2")
plot(hclust1)
barplot(hclust1$height, names.arg = 18:1)
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
#alternatywne wersje dendrogramów
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

#porównanie wyników eksperymentów
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
# 1. macierz czêstoœci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. zak³adana liczba skupieñ

#eksperyment 3
nClusters3 <- 3
kmeans3 <- kmeans(dtmTfidfBounds, centers = nClusters3)
clustersMatrix3 <- matrix(0,nDocuments,nClusters3)
rownames(clustersMatrix3) <- names(kmeans3$cluster)
for (i in 1:nDocuments) {
  clustersMatrix3[i, kmeans3$cluster[i]] <- 1
}
corrplot(clustersMatrix3)

#wspó³czynnik zbie¿noœci podzia³ów przy zadanej liczbie skupieñ
##dla 3 skupieñ
randEx2Ex3 <- randIndex(clusters2, kmeans3$cluster, F)
randEx2Ex3
randEx2Pattern <- randIndex(clusters2, pattern, F)
randEx2Pattern
randEx3Pattern <- randIndex(kmeans3$cluster, pattern, F)
randEx3Pattern






#Punkt 7 (wklejony kod, nic nie zmieniane)

#analiza ukrytej alokacji Dirichlet'a

#w³¹czenie bibliotek
library(topicmodels)



#analiza ukrytej alokacji Dirichlet'a
nWords <- ncol(dtmTfAll)
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
perplexity <- perplexity(lda, dtmTfAll)
results <- posterior(lda)

#prezentacja tematów
par(mai = c(1,2,1,1))
topic1 <- head(sort(results$terms[1,], decreasing = TRUE), 20)
barplot(
  rev(topic1), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 1",
  xlab = "Prawdopodobieñstwo",
  col = "orange"
)
topic2 <- head(sort(results$terms[2,], decreasing = TRUE), 20)
barplot(
  rev(topic2), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 2",
  xlab = "Prawdopodobieñstwo",
  col = "turquoise"
)
topic3 <- head(sort(results$terms[3,], decreasing = TRUE), 20)
barplot(
  rev(topic3), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 3",
  xlab = "Prawdopodobieñstwo",
  col = "violet"
)
topic4 <- head(sort(results$terms[4,], decreasing = TRUE), 20)
barplot(
  rev(topic4), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 4",
  xlab = "Prawdopodobieñstwo",
  col = "lightskyblue"
)
topic5 <- head(sort(results$terms[5,], decreasing = TRUE), 20)
barplot(
  rev(topic5), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 5",
  xlab = "Prawdopodobieñstwo",
  col = "darkseagreen"
)

#prezentacja dokumentów
document4 <- results$topics[4,]
barplot(
  rev(document4), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[4],
  xlab = "Prawdopodobieñstwo",
  col = "orange"
)

document11 <- results$topics[11,]
barplot(
  rev(document11), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobieñstwo",
  col = "turquoise"
)

document17 <- results$topics[17,]
barplot(
  rev(document17), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[17],
  xlab = "Prawdopodobieñstwo",
  col = "violet"
)

#udzia³ tematów w s³owach
#tutaj na pewno do zmiany
words1<- c("czarodziej", "czarownica", "wampir")
round(results$terms[,words1],2)

words2<- c("harry", "³ucja", "bell")
round(results$terms[,words2],2)





#P U N K T    8      (wklejony kod tylko)
#w³¹czenie bibliotek
library(wordcloud)



#dla pierwszego dokumentu
##waga tf jako miara wa¿noœci s³ów
keywordsTf1 <- head(sort(dtmTfAllMatrix[1,], decreasing = T))
keywordsTf1

##waga tfidf jako miara wa¿noœci s³ów
keywordsTfidf1 <- head(sort(dtmTfidfBoundsMatrix[1,], decreasing = T))
keywordsTfidf1

##prawdopodobieñstwo w LDA jako miara wa¿noœci s³ów
termsImportance1 <- c(results$topics[1,]%*%results$terms)
names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1, decreasing = T))
keywordsLda1

##chmury tagów
par(mai = c(0,0,0,0))
wordcloud(corpus[1], max.words = 200, colors = brewer.pal(8,"PuOr"))
