rm(list = ls())

library(text2vec)

text8_file = "~/text8"

if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = "~/")
}

wiki = readLines(text8_file, n = 1, warn = FALSE)

# Create iterator over tokens
tokens <- space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab, term_count_min = 5L)


# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
                               # don't vectorize input
                               grow_dtm = FALSE, 
                               # use window of 5 for context words
                               skip_grams_window = 5L)
tcm <- create_tcm(it, vectorizer)


glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glove$fit(tcm, n_iter = 20)


glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit()` call !
fit(tcm, glove, n_iter = 20)

word_vectors2 <- glove$get_word_vectors()

paris <- word_vectors["london", , drop = FALSE] - 
  word_vectors["britain", , drop = FALSE] + 
  word_vectors["france", , drop = FALSE]

cos_sim = sim2(x = word_vectors, y = paris, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)
#london     paris      york   founded      city 
#0.8858521 0.7756275 0.7343578 0.6803436 0.6791411 
