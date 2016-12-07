## Code for examples from Week 9, topic models

setwd("~/Dropbox/Slavka/Teaching/AQM/Lectures/week9")

require(quanteda)

##
## general examples topic models
##
data(SOTUCorpus, package = "quantedaData")
presDfm <- dfm(subset(SOTUCorpus, Date > as.Date("1900-01-01")), stem = TRUE)
presDfm <- trim(presDfm, minCount = 5, minDoc = 3)

require(topicmodels)
presLDA <- LDA(presDfm, k = 30)
# which terms contribute most to each topic
terms(presLDA, k = 20)
# which is the dominant topic for each document
topics(presLDA, 3)

# now without stopwords
presDfm2 <- dfm(subset(SOTUCorpus, Date > as.Date("1900-01-01")), 
                stem = TRUE, ignoredFeatures = c(stopwords("english"), "will"))
presDfm2 <- trim(presDfm2, minCount = 5, minDoc = 3)

t1 <- Sys.time()
presLDA2 <- LDA(presDfm2, k = 30)
t2 <- Sys.time()
t2 - t1  # about 7 minutes on MacBook Pro


terms(presLDA2, k = 20)
topics(presLDA2, 3)
(postTopics <- data.frame(posterior(presLDA)$topics))

#save.image(file = "topicmodels.RData")

##
## Topic models on NYT articles
##
require(jsonlite)
require(topicmodels)
nyt <- fromJSON("https://raw.githubusercontent.com/smikhaylov/PUBLG088/master/week9/nyt_ac.json")
nytCorpus <- corpus(nyt$body$body_text, docvars = cbind(title = nyt$body$title, nyt$meta))
nytDfm <- dfm(nytCorpus, stem = TRUE, ignoredFeatures = stopwords("english"))
nytDfm <- trim(nytDfm, minCount = 20)
# fit topic model with 4 topics
nytLDA4 <- LDA(nytDfm, k=4)
terms(nytLDA4, k=20)
prop.table(table(topics(nytLDA4)))
# fit lda model with 8 topics
nytLDA8 <- LDA(nytDfm, k=8)
terms(nytLDA8, k=20)
prop.table(table(topics(nytLDA8)))
nytCTM8 <- CTM(nytDfm, k=8)
terms(nytCTM8, k=20)


##
## with visualization
##
# load movies and create a dfm
data(movies, package = "quantedaData")

# prepare the texts
moviesDfm <- dfm(movies, ignoredFeatures = stopwords("SMART"), stem = FALSE)
moviesDfm <- trim(moviesDfm, minCount = 5)
moviesDfm

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# convert to lda format
moviesDfmlda <- convert(moviesDfm, to = "lda")
# fit the model
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = moviesDfmlda$documents, K = K, 
                                   vocab = moviesDfmlda$vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 17 minutes on MacBook Pro


 #save(fit, file = "./fit.RData")
 # load("./fit.RData")
 
 
library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(phi = t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x))), 
                   theta = t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x))), 
                   doc.length = ntoken(moviesDfm), 
                   vocab = features(moviesDfm), 
                   term.frequency = colSums(moviesDfm))
serVis(json, out.dir = "./visColl", open.browser = TRUE)

