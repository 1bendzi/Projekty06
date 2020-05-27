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

#wstepne przetwarzanie: zapisanie przetworzonych plikow do katalogu
preprocessedDir <- paste(
  outputDir, 
  "Literatura - przetworzone",
  sep = "\\"
)
dir.create(preprocessedDir)
writeCorpus(corpus, path = preprocessedDir)

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
#matrixFile <- paste(
#  outputDir,
#  "\\",
#  "tdmTfidfBounds.csv",
#  sep = ""
#)
#write.table(tdmTfidfBoundsMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)
