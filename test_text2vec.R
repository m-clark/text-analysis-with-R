load('data/shakes_words_df_4text2vec.RData')

library(tidyverse); library(text2vec)
shakes_words_as_list = shakes_words %>%
  anti_join(tidytext::stop_words) %>%
  split(.$id) %>%
  map(function(x) x %>% pull(word))
# tokens <- word_tokenizer(shakes_words_as_list)  # you've essentially done this
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(shakes_words_as_list, progressbar = FALSE)
vocab <- create_vocabulary(it) %>%
  prune_vocabulary(term_count_min = 5L)
# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 3)

RcppParallel::setThreadOptions(numThreads = 6)
glove = GloVe$new(word_vectors_size = 10, vocabulary = vocab, x_max = 10)
word_vectors_main = glove$fit_transform(tcm, n_iter = 100)
# word_vectors <- glove$get_word_vectors() # deprecated
word_vectors_context <- glove$components
word_vectors = word_vectors_main + t(word_vectors_context)



test <- word_vectors["romeo", , drop = FALSE] +
  word_vectors["juliet", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = test, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

test <- word_vectors["romeo", , drop = FALSE] -
  word_vectors["juliet", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = test, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

test <- word_vectors["romeo", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = test, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

test <- word_vectors["hamlet", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = test, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

test <- word_vectors["juliet", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = test, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)


