#włączenie bibliotek 
library(tm) 
library (hunspell)
library (stringr)

#zmiana katalogu roboczego 
workDir <- "C:\\Users\\Beniamin\\Desktop\\Projekty06 Projekt\\Projekty06"
setwd(workDir)


#definicja katalog?w projektu
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"
workspaceDir <- ".\\workspaces"

#utworzenie katalogu wyj?ciowego
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspaceDir, showWarnings = FALSE)