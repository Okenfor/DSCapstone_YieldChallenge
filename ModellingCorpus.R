library(jsonlite)
library(tm)
#install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm.lexicon.GeneralInquirer)
#install.packages("tm.plugin.sentiment", repos="http://R-Forge.R-project.org")
library(tm.plugin.sentiment)
library(tm)

library(stringi)

#data_business <- stream_in(file("yelp_academic_dataset_business.json"))
#data_checkin <- stream_in(file("yelp_academic_dataset_checkin.json"))
data_review <- stream_in(file("yelp_academic_dataset_review.json"))
#data_tip <- stream_in(file("yelp_academic_dataset_tip.json"))
#data_user <- stream_in(file("yelp_academic_dataset_user.json"))

flat_data_review <- flatten(data_review)
colnames(flat_data_review) <- gsub("[^A-Za-z]", "", colnames(flat_data_review))
s <- summary(flat_data_review)
dput(s, file = "summary_data_review.txt")

nrows <- nrow(data_review)
#sampling text
#sDataReview <- data_review[sample(nrows, replace = F, size = 0.1 * nrows),]
sampleSize <- 0.05

#1-star
idx1 <- data_review$stars == 1
nrows1 <- sum(idx1)
data_review1 <- data_review[idx1,]
set.seed(1234)
sDataReview1 <- data_review1[sample(nrows1, replace = F, size = sampleSize * nrows1),]

#2-star
idx2 <- data_review$stars == 2
nrows2 <- sum(idx2)
data_review2 <- data_review[idx2,]
set.seed(1234)
sDataReview2 <- data_review2[sample(nrows2, replace = F, size = sampleSize * nrows2),]

#3-star
idx3 <- data_review$stars == 3
nrows3 <- sum(idx3)
data_review3 <- data_review[idx3,]
sDataReview3 <- data_review3[sample(nrows3, replace = F, size = sampleSize * nrows3),]

#4-star
idx4 <- data_review$stars == 4
nrows4 <- sum(idx4)
data_review4 <- data_review[idx4,]
set.seed(1234)
sDataReview4 <- data_review4[sample(nrows4, replace = F, size = sampleSize * nrows4),]

#5-star
idx5 <- data_review$stars == 5
nrows5 <- sum(idx5)
data_review5 <- data_review[idx5,]
set.seed(1234)
sDataReview5 <- data_review5[sample(nrows5, replace = F, size = sampleSize * nrows5),]

#Freeing memory
rm(data_review)
rm(data_review1)
rm(data_review2)
rm(data_review3)
rm(data_review4)
rm(data_review5)
rm(nrows1)
rm(nrows2)
rm(nrows3)
rm(nrows4)
rm(nrows5)
gc()

sDataReview <- rbind(sDataReview1,sDataReview2,sDataReview3,sDataReview4,sDataReview5)
rm(sDataReview1)
rm(sDataReview2)
rm(sDataReview3)
rm(sDataReview4)
rm(sDataReview5)
gc()

#indexes for stars on sample data
idx1 <- sDataReview$stars == 1
idx2 <- sDataReview$stars == 2
idx3 <- sDataReview$stars == 3
idx4 <- sDataReview$stars == 4
idx5 <- sDataReview$stars == 5

sDataReview <- flatten(sDataReview)
colnames(sDataReview) <- gsub("[^A-Za-z]", "", colnames(sDataReview))
sDataReview$id <- paste("doc", 1:nrow(sDataReview), sep = "")

m <- list(id = "id", content = "text")
myReader <- readTabular(mapping = m)
corpus <- Corpus(DataframeSource(sDataReview), readerControl = list(reader = myReader))

# Manually keep ID information from http://stackoverflow.com/a/14852502/1036500
for (i in 1:length(myReader)) {
  attr(corpus[[i]], "id") <- sDataReview$id[i]
}

#corpus_bkp <- corpus
#corpus <- corpus_bkp

### preprocessing functions #####
skipWords <- function(x) removeWords(x, stopwords("english"))
# removing URLs
urlPat<-function(x) gsub("(ftp|http)s?:(\\/)+[\\d\\w.]*\\b", " ", x, perl = T)
# removing Emails
emailRgx <- "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
emlPat<-function(x) gsub(emailRgx, " ", x)
# removing Twitter tags
tags<-function(x) gsub("(RT )|via", " ", x)
#replace curly brace with single quote 
singleQuote <- function(x) gsub("\u2019|`", "'", x, perl = T)
#replace non printable, except ' - and space with empty string
nonprint <- function(x) gsub("[^\\p{L}\\s'-]", "", x, perl = T)
#remove non English words
nonEng <- function(x) gsub("[^A-Za-z']", " ", x)
#remove duplicated letters in words
dupLetters <- function(x) gsub("(\\w)\\1+", "\\1", x, perl = T)
#remove duplicated consecutive words
dupWords <- function(x) gsub("(\\w+)\\s+\\1", "\\1", x, perl = T)
# stemming
library(SnowballC)

funcs <- list(tolower, urlPat, emlPat, tags, singleQuote, nonprint, nonEng, dupLetters, dupWords, removePunctuation, removeNumbers, stripWhitespace, stemDocument, skipWords)
rcorpus <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
rcorpus <- tm_map(rcorpus, PlainTextDocument)

#writeCorpus(rcorpus)
##loading corpus
##txt <- system.file("texts", "txt", package = "tm")
##(ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),
##                   +                  readerControl = list(language = "lat")))

#term matrix
library(slam)
dtm <- DocumentTermMatrix(rcorpus, control = list(wordLengths = c(3,10), minDocFreq=50))
#inspect(dtm[1:10,10:20])
#Min frequency term
#min((dtm[,1])[,1])
#Max frequency term
#max((dtm[,1])[,1])
#Shows terms with a frequency of at least 10
#length(findFreqTerms(dtm, lowfreq =  1000))
#dim(dtm)
#findAssocs(dtm, 'good', 0.30)

#################
#sentiment scores
pos.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Positiv"))
neg.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Negativ")) 
pleasur.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Pleasur"))
pain.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Pain")) 
feel.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Feel")) 
arousal.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Arousal")) 
emot.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("EMOT")) 
virtue.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Virtue")) 
vice.score <- tm_term_score(dtm, terms_in_General_Inquirer_categories("Vice"))

df.scores <- data.frame(positive = pos.score, negative = neg.score,
                       pleasure = pleasur.score, pain = pain.score,
                       feel = feel.score, arousal = arousal.score,
                       emotion = emot.score, virtue = virtue.score,
                       vice = vice.score)
df.scores$id <- paste("doc", 1:nrow(df.scores), sep = "")
#################
#sentiment terms
filterByFreq <- function(docTermMatrix, prob) {
  require(slam)
  freqs <- (slam::col_sums(docTermMatrix)) 
  qt <- quantile(freqs, probs = c(prob))
  idxFreqs <- freqs > qt
}

pos.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Positiv")]
neg.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Negativ")] 
pleasur.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Pleasur")]
pain.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Pain")] 
feel.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Feel")] 
arousal.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Arousal")] 
emot.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("EMOT")] 
virtue.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Virtue")] 
vice.terms <- dtm[, dtm$dimnames$Terms %in% terms_in_General_Inquirer_categories("Vice")]

write.csv(as.data.frame(inspect(pos.terms)), file = "pos_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(neg.terms)), file = "neg_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(pleasur.terms)), file = "pleasur_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(pain.terms)), file = "pain_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(feel.terms)), file = "feel_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(arousal.terms)), file = "arousal_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(emot.terms)), file = "emot_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(virtue.terms)), file = "virtue_terms.csv", row.names = FALSE)
write.csv(as.data.frame(inspect(vice.terms)), file = "vice_terms.csv", row.names = FALSE)

all.sentiment <- c(terms_in_General_Inquirer_categories("Positiv"), 
                   terms_in_General_Inquirer_categories("Negativ"),
                   terms_in_General_Inquirer_categories("Pleasur"),
                   terms_in_General_Inquirer_categories("Pain"),
                   terms_in_General_Inquirer_categories("Feel"),
                   terms_in_General_Inquirer_categories("Arousal"),
                   terms_in_General_Inquirer_categories("EMOT"),
                   terms_in_General_Inquirer_categories("Virtue"),
                   terms_in_General_Inquirer_categories("Arousal"))
all.sentiment <- unique(all.sentiment)
df.terms <- dtm[, dtm$dimnames$Terms %in% all.sentiment]
df.terms <- df.terms[,filterByFreq(df.terms, .85)]

data <- merge.data.frame(sDataReview[,-c(1,2,4:7)], df.terms, by.x = "id")
data <- merge.data.frame(data, df.scores, by.x = "id", by.y ="id")

data$stars <- as.factor(make.names((data$stars)))
rm(M)
gc()

rm(rcorpus)
rm(pos.terms)
rm(neg.terms)
rm(emot.terms)
rm(feel.terms)
rm(arousal.terms)
rm(all.terms)
rm(df.terms)
rm(pleasur.terms)
rm(pain.terms)
rm(vice.terms)
rm(virtue.terms)
rm(pos.score)
rm(neg.score)
rm(emot.score)
rm(feel.score)
rm(arousal.score)
rm(all.score)
rm(df.score)
rm(pleasur.score)
rm(pain.score)
rm(vice.score)
rm(virtue.score)
rm(dtm)
gc()

write.csv(data, file = "reviews_processed.csv", row.names = FALSE)
