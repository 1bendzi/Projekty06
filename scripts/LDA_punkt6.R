#Punkt 6 

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
  xlab = "Prawdopodobieństwo",
  col = "orange"
)
topic2 <- head(sort(results$terms[2,], decreasing = TRUE), 20)
barplot(
  rev(topic2), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 2",
  xlab = "Prawdopodobieństwo",
  col = "turquoise"
)
topic3 <- head(sort(results$terms[3,], decreasing = TRUE), 20)
barplot(
  rev(topic3), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 3",
  xlab = "Prawdopodobieństwo",
  col = "violet"
)
topic4 <- head(sort(results$terms[4,], decreasing = TRUE), 20)
barplot(
  rev(topic4), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 4",
  xlab = "Prawdopodobieństwo",
  col = "lightskyblue"
)
topic5 <- head(sort(results$terms[5,], decreasing = TRUE), 20)
barplot(
  rev(topic5), 
  horiz = TRUE,
  las = 1, 
  main = "Temat 5",
  xlab = "Prawdopodobieństwo",
  col = "darkseagreen"
)

#prezentacja dokumentów
document4 <- results$topics[4,]
barplot(
  rev(document4), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[4],
  xlab = "Prawdopodobieństwo",
  col = "orange"
)

document11 <- results$topics[11,]
barplot(
  rev(document11), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobieństwo",
  col = "turquoise"
)

document17 <- results$topics[17,]
barplot(
  rev(document17), 
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[17],
  xlab = "Prawdopodobieństwo",
  col = "violet"
)

#udział tematów w słowach
words1<- c("czarodziej", "czarownica", "wampir") #tu powstawiamy wlasne
round(results$terms[,words1],2)

words2<- c("harry", "łucja", "bell") #tu tez 
round(results$terms[,words2],2)