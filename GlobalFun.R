filterByFreq <- function(docTermMatrix, prob) {
  freqs <- (slam::col_sums(docTermMatrix)) 
  qt <- quantile(freqs, probs = c(prob))
  idxFreqs <- freqs > qt
}

###
#word cloud and other analysis
#http://www.rdatamining.com/examples/text-mining
#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

getWordCloud <- function(df.terms, title){
  df.terms <- df.terms[,filterByFreq(df.terms, .75)]
  
  require(wordcloud)  
  df.terms.freqs <- sort(colSums(df.terms), decreasing=TRUE)
  #head(df.terms.freqs)
  set.seed(1234)
  dark2 <- brewer.pal(6, "Dark2")  
  wordcloud(names(df.terms.freqs), main = title, df.terms.freqs, max.words = 100, rot.per=0.2, colors=dark2)
}

getWordHist <- function(df.terms, title){
  df.terms.freqs <- colSums(df.terms)
  dt.terms <- data.frame(word=names(df.terms.freqs), freq=df.terms.freqs) 
  dt.terms <- dt.terms[dt.terms$freq>50,]
  dt.terms <- dt.terms[order(dt.terms$freq, decreasing = T),]
  dt.terms$word <- factor(dt.terms$word, as.character(dt.terms$word))
  require(ggplot2)
  p <- ggplot(data = dt.terms[1:50,], aes(word, freq))
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
  p <- p + ggtitle(title)
  p
}

######################
#clustering
#http://michael.hahsler.net/SMU/CSE7337/install/tm.R

## do document clustering
plotTreeCluster <- function(df.terms){
  df.terms.freqs <- colSums(df.terms)
  df.terms <- df.terms[,names(df.terms.freqs[df.terms.freqs>3])]
  m <- as.matrix(df.terms)
  rownames(m) <- 1:nrow(m)
  
  ### don't forget to normalize the vectors so Euclidean makes sense
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  m_norm <- norm_eucl(m)
  
  ## hierarchical clustering
  library(proxy)
  library(cluster)
  
  ### this is going to take 4-ever (O(n^2))
  distances <- dist(t(m_norm), method="cosine") ##warning: t() is the transpose function, we look for clustering words not documents
  hc <- hclust(d=distances, method="ward.D2")   
  plot(hc, hang=-1)
  
  groups <- cutree(hc, k=5)   # "k=" defines the number of clusters you are using
  rect.hclust(hc, k=5, border="red") # draw dendogram with red borders around the 5 clusters   
  
  #cl <- cutree(hc, 30)
  #table(cl)
}
###############################