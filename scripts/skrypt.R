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

#w³¹czenie bibliotek
library(tm)

#utworzenie korpusu dokumentów
corpusDir <- paste(
  inputDir,
  "\\",
  "Literatura - streszczenia - przetworzone",
  sep = ""
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

#usuniêcie rozszerzeñ z nazw dokumentów
cutExtensions <- function(document) {
  meta(document, "id") <- gsub(pattern = "\\.txt$", "", meta(document, "id"))
  return(document)
}

corpus <- tm_map(corpus, cutExtensions)


#utworzenie macierzy czêstoœci
tdmTfAll <- TermDocumentMatrix(corpus)
dtmTfAll <- DocumentTermMatrix(corpus)
tdmTfidfAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightTfIdf
  )
)
tdmBinAll <- TermDocumentMatrix(
  corpus,
  control = list(
    weighting = weightBin
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

#konwersja macierzy ¿adkich do macierzy klasycznych
tdmTfAllMatrix <- as.matrix(tdmTfAll)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBounds)

#eksport macirzy do pliku .csv
matrixFile <- paste(
  outputDir,
  "\\",
  "tdmTfidfBounds(2,16).csv",
  sep = ""
)
write.table(tdmTfidfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)






#Redukcja wymiarow  -    PUNKT 5

#w³¹czenie bibliotek
library(lsa)



#analiza ukrytych wymiarów semantycznych (dekompozycja wg wartoœci osobliwych)
lsa <- lsa(tdmTfidfBoundsMatrix)
lsa$tk #odpowiednik macierzy U, wspó³rzêdne wyrazów
lsa$dk #odpowiednik macierzy V, wspó³rzêdne dokumentów
lsa$sk #odpowiednik macierzy D, znaczenie sk³adowych

#przygotowanie wspó³rzêdnych do wykresu
coordDocs <- lsa$dk%*%diag(lsa$sk)
coordTerms <- lsa$tk%*%diag(lsa$sk)
words <- c("harry", "czarodziej", "dumbledore", "hermiona", "ron", "komnata", "powiedzieæ", "chcieæ", "dowiadywaæ", "albus", "syriusz", "lupin", "umbridge", "edmund", "kaspian", "³ucja", "czarownica", "piotr", "zuzanna", "aslana", "narnii", "baron", "dziecko", "wyspa", "bell", "edward", "wampir", "jacob")
termsImportance <- diag(coordTerms%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantWords <- names(tail(sort(termsImportance), 25))
coordWords <- coordTerms[importantWords,]
x1 <- coordDocs[,1]
y1 <- coordDocs[,2]
x2 <- coordWords[,1]
y2 <- coordWords[,2]

#przygotowanie legendy
legend <- paste(
  paste("d", 1:length(rownames(coordDocs)),sep = ""),
  rownames(coordDocs),
  sep = "<-"
)

#wykres dokumentów w przestrzeni dwuwymiarowej
plot(
  x1,
  y1,
  #xlim = c(-0.02,-0.01),
  #ylim = c(-0.05,0.05),
  xlab="Wspó³rzêdna syntetyczna 1", 
  ylab="Wspó³rzêdna syntetyczna 2",
  main="Analiza ukrytych wymiarów sematycznych", 
  col = "orange"
)
text(
  x1, 
  y1, 
  labels = paste("d", 1:length(rownames(coordDocs)),sep = ""), 
  pos = 4,
  col = "orange"
)
points(
  x2,
  y2,
  pch = 2,
  col = "brown"
)
text(
  x2, 
  y2, 
  labels = rownames(coordWords), 
  pos = 4,
  col = "brown"
)
legend("bottomleft", legend, cex=.6, text.col = "orange")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "\\",
  "lsa.png",
  sep = ""
)
png(file = plotFile)
plot(
  x1,
  y1,
  #xlim = c(-0.02,-0.01),
  #ylim = c(-0.05,0.05),
  xlab="Wspó³rzêdna syntetyczna 1", 
  ylab="Wspó³rzêdna syntetyczna 2",
  main="Analiza ukrytych wymiarów sematycznych", 
  col = "orange"
)
text(
  x1, 
  y1, 
  labels = paste("d", 1:length(rownames(coordDocs)),sep = ""), 
  pos = 4,
  col = "orange"
)
points(
  x2,
  y2,
  pch = 2,
  col = "brown"
)
text(
  x2, 
  y2, 
  labels = rownames(coordWords), 
  pos = 4,
  col = "brown"
)
legend("bottomleft", legend, cex=.5, text.col = "orange")
dev.off()






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






#Punkt 7 (kod w pliku oddzielnym)

#analiza ukrytej alokacji Dirichlet'a
