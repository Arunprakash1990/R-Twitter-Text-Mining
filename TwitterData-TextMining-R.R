require(twitteR)
require(RCurl)
consumer_key <- 'Cq1ZFajR1dpgCJeGIfEhjXkaB'
consumer_secret <- 'oSFI9CfZeYyRNIwYvZnPEAzN9u8Gjnb5MLZ2fELTuYZB0Vvx9E'
access_token <- '111953449-ImnXqfCMF7mOKamLCIpksHbAkqnn2JYwtWexebOY'
access_secret <- 'YYs0D95zfJ3P6qeiZ5IEb7iYEcjZYBUKWBKw6RzZH71rX'
setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)


tweets = searchTwitter("#Oscars", n=1000)
head(tweets)
library("tm")
mylist <- sapply(tweets, function(x) x$getText())
mycorpus <- Corpus(VectorSource(mylist))
mycorpus <- tm_map(mycorpus, content_transformer(tolower), lazy = TRUE)
mycorpus <- tm_map(mycorpus, removeNumbers, lazy = TRUE)
mycorpus <- tm_map(mycorpus, removePunctuation, lazy = TRUE)
mycorpus <- tm_map(mycorpus,
                   function(x)removeWords(x,stopwords()), lazy = TRUE)
?getTransformations
mycorpus <- tm_map(mycorpus, PlainTextDocument, lazy = TRUE)
library("wordcloud")
library("RColorBrewer")
?RColorBrewer
col <- brewer.pal(5,"Dark2") 
wordcloud(mycorpus, min.freq=3, rot.per=0.5, scale=c(4,1),
          random.color=T, max.word=45, random.order=F, colors=col)
mytdm <- TermDocumentMatrix(mycorpus)
findFreqTerms(mytdm, lowfreq=55) # experiment with the lowfreq
tdm <-removeSparseTerms(mytdm, sparse=0.93) # experimet with
sparse
tdmscale <- scale(tdm)
dist <- dist(tdmscale, method = "canberra")
fit <- hclust(dist)
plot(fit)
# we need to change the margins and delete some titles
par(mai=c(1,1.2,1,0.5))
plot(fit, xlab="", sub="", col.main="salmon")
cutree(fit, k=7)
rect.hclust(fit, k=7, border="salmon")
- import positive and negative words
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
library("stringr")
library("plyr")
score.sentiment = function(sentences, pos.words, neg.words,
                           .progress='none')
{
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr
                   package)
  word.list = str_split(sentence, "\\s+")
  words = unlist(word.list)
  # compare words to the dictionaries of positive & negative terms
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  # get the position of the matched term or NA
  # we just want a TRUE/FALSE
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  # final score
  score = sum(pos.matches) - sum(neg.matches)
  return(score)
                 }, pos.words, neg.words, .progress=.progress )

# data frame with scores for each sentence
scores.df = data.frame(text=sentences, score=scores)
return(scores.df)
}
# tweets for companies - may not get the full 900
bayertweets = searchTwitter("#bayer", n=900, lang="en",
                            cainfo="cacert.pem")
pfizertweets = searchTwitter("#pfizer", n=900, lang="en",
                             cainfo="cacert.pem")
rochetweets = searchTwitter("#roche", n=900, lang="en",
                            cainfo="cacert.pem")
novartistweets = searchTwitter("#novartis", n=900, lang="en",
                               cainfo="cacert.pem")
# get text
bayer_txt = sapply(bayertweets, function(x) x$getText())
pfizer_txt = sapply(pfizertweets, function(x) x$getText())
roche_txt = sapply(rochetweets, function(x) x$getText())
novartis_txt = sapply(novartistweets, function(x) x$getText())
# how many tweets
nd = c(length(bayer_txt), length(pfizer_txt), length(roche_txt),
       length(novartis_txt))
# join texts
company = c(bayer_txt, pfizer_txt, roche_txt, novartis_txt) 
# apply function score.sentiment
scores = score.sentiment(company, pos, neg, .progress='text')
# add variables to data frame
scores$company = factor(rep(c("bayer", "pfizer", "roche", "novartis"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)
# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)
# global score
global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)
par(bty="l")
boxplot(score~company, data=scores, col=c("red", "grey"))
library("lattice")
histogram(data=scores, ~score|company, main="Sentiment Analysis of 4
Companies", col=c("red", "grey"),
          xlab="", sub="Sentiment Score")

