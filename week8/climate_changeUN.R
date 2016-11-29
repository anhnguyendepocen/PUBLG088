rm(list = ls(all = TRUE))

library(text2vec)

setwd("~/Dropbox/Slavka/Research/UN General Debate/Analysis/input")

files <-list.files()

data <- 0


for (f in files) {
  
  tempData = scan( f, what="character", quiet = TRUE)
  
  data <- c(data,tempData)    
  
} 


# Create iterator over tokens
tokens <- space_tokenizer(data)

# Create vocabulary. Terms will be unigrams (simple words).
it <-  itoken(tokens, progressbar = FALSE)


vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 10)



# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
                               # don't vectorize input
                               grow_dtm = FALSE, 
                               # use window of 5 for context words
                               skip_grams_window = 5L)
tcm <- create_tcm(it, vectorizer)


glove <- GlobalVectors$new(word_vectors_size = 300, vocabulary = vocab, x_max = 10)
glove$fit(tcm, n_iter = 20)


glove <-  GlobalVectors$new(word_vectors_size = 300, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit()` call !
fit(tcm, glove, n_iter = 20)


word_vectors <- glove$get_word_vectors()

problem <- word_vectors["climate", , drop = FALSE] + 
  word_vectors["change", , drop = FALSE] - 
  word_vectors["health", , drop = FALSE]

cos_sim = sim2(x = word_vectors, y = problem, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)





