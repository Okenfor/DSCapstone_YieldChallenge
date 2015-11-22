FinalPresentation
========================================================
author: paperezq
date: 2015-11-22

Sentiment Analysis over Yelp Business Reviews
========================================================

This is part of the Data Science capstone and the dataset used comes from the [Yelp Dataset Challenge](http://www.yelp.com/dataset_challenge). 

My question of interest for this project is how well can a tool guess a review's rating from its text alone?. Basically, the idea is measure the how well a classifier performs based-on some positive and negative words from text reviews. Nevertheless, words of other kind of lexicons were used. We will see that the models built used more than positive and negative words.

- Bullet 1
- Bullet 2
- Bullet 3

Data is explored
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Preprocessing
========================================================

Some functions are defined to get data cleaned from characters that are not in the english alphabet, i.e. just letters. Functions that catch english [stop words](https://en.wikipedia.org/wiki/Stop_words), urls, emails, twitter tags, duplicated quotes, non-ascii characters, non english character, duplicated letters and duplicated words. At the end, a list of this functions is created and an additional tm function is append in order to [stem words](https://en.wikipedia.org/wiki/Stemming).


```r
#list of functions to preprocess the corpus
funcs <- list(tolower, urlPat, emlPat, tags, singleQuote, nonprint, nonEng, dupLetters, dupWords, removePunctuation, removeNumbers, stripWhitespace, stemDocument, skipWords)
#cleansing process is done
rcorpus <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
#fix a bug of tm library
rcorpus <- tm_map(rcorpus, PlainTextDocument)
```


Most frequent sentiments
========================================================

-positive and negative
-pleasure and pain
-virtues and vices
-emotions and feelings
-arousals

classifiers
========================================================

-naive bayes
-penalized discriminant analysis
-random forest
-linear discriminant analysis with bagging
