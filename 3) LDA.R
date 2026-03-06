# Set the working directory 
setwd("~/Dropbox/Mac/Desktop/Quantitative Text Analysis/Assignment/Code")

#Clear Workspace and memory
rm(list = ls())
gc()

# Install packages and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quanteda, seededlda, topicmodels, remotes)

if (!requireNamespace("ldatuning", quietly = TRUE)) {
  message("Package not found...", 
          remotes::install_github("nikita-moor/ldatuning"))
}
library(ldatuning)

obj <- readRDS("./CBB_corpus_and_tokens_clean.rds")
names(obj)
corp <- obj$corpus

# Extract each object
corp <- obj$corpus
toks_all <- obj$tokens_clean
toks_nouns <- obj$toks_nouns

##### Create DFM with dfm() command #####
# dfm_main <- obj$dfm_main
# 
##### Build a dfm from your tokens #####
# # remove very rare features (tune these)
# dfm_tm <- dfm_trim(dfm_main,
#                    min_termfreq = 5,      # keep terms appearing >=5 times overall
#                    min_docfreq  = 2)      # keep terms appearing in >=2 docs
# 
# 
# dfm_tm
# topfeatures(dfm_tm, 20)

dfm_nouns <- dfm(toks_nouns)
dfm_tm <- dfm_trim(dfm_nouns, min_termfreq = 5, min_docfreq = 2)
custom_stop <- c("bank", "barbado", "year", "government")
dfm_tm <- dfm_remove(dfm_tm, custom_stop)
topfeatures(dfm_tm, 20)

##### Optimal number of topics #####
set.seed(12345)

n_docs <- ndoc(dfm_tm)
train_prop <- 0.8

# RandomLy sample document indices for training set
train_idx <- sample(n_docs, size = floor(train_prop * n_docs))
# Training and held-out sets
dfm_train <- dfm_tm[train_idx, ]
dfm_test <- dfm_tm[-train_idx, ]

K_candidates <- c(2,3,4,5, 6, 7, 8, 9, 10)
# Prepare a results table
perp_results <- data.frame(
  k = K_candidates,
  perplexity = NA_real_
)
# Loop over K values, fit LDA, compute held-out perplexity
for (i in seq_along(K_candidates)) {
  k_val <- K_candidates[i]
  cat ("Fitting LDA with", k_val, "topics...\n")
  # Fit LDA on training set onLy
  lda_k<- textmodel_lda(
    x = dfm_train,
    k = k_val,
    max_iter = 500)
  # Compute perplexity on held-out speech documents
  # NOTE: explicitly use seededlda: :perplexity to avoid conflict with topicmodels: :perplexity
  perp_results$perplexity[i] <- seededlda::perplexity(lda_k, newdata = dfm_test)
}
# Inspect results
perp_results
# PerpLexity curve
#Note: Looking for smallest Kafter which additional topics yield only marginal improvements in perplexity
# In other words: the point on curve where curve stops bending sharply
# If there is more than one 'elbow' then one can choose any of these,
# depending on whether one wants broader or specific themes
plot(perplexity ~ k,
     data = perp_results,
     type = "b",
     xlab = "Number of topics (K)",
     ylab = "Held-out perplexity",
     main = "Held-out perplexity") ##6 or 7 ?



# Additional K diagnostics using Ldatuning
#convert dfm_train (quanteda) to a topicmodeLs-compatible DTM
dtm_train_tm <- convert(dfm_train, to = "topicmodels")
K_candidates <-c(3,4,5,6,7,8)
# Run Ldatuning over your candidate K values
ldatune_results <- FindTopicsNumber(
  dtm = dtm_train_tm,
  topics = K_candidates,
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  # Gibbs sampling LDA
  method = "Gibbs",
  # seed is for reproducibility;
  # iter affects run time but should idealLy be 300-500!
  control = list(iter = 500, seed = 12345),
  verbose = TRUE
)
ldatune_results

FindTopicsNumber_plot(ldatune_results)

k_topics <- 6

lda_unsup <- textmodel_lda(
  x = dfm_tm,
  k = k_topics,
  max_iter = 300
)

phi_mat <- as.matrix(lda_unsup$phi)
phi_mat
top_n <- 15

top_terms_by_topic <- list()

for (k in 1:nrow(phi_mat)) {
  topic_probs <- phi_mat[k, ]
  topic_probs_sorted <- sort(topic_probs, decreasing = TRUE)
  top_terms_by_topic[[k]] <- names(topic_probs_sorted)[1:top_n]
  
}

top_terms_by_topic

topic_names <- c("External Sector & Growth",
                 "Central Bank Governance",
                 "Financial Sector Oversight", 
                 "Banking & Financial Intermediation", "Exchange Rate & Commodity Economy",
                 "Domestic Activity & Investment"
)

# -----------------------------
# Plot
# -----------------------------

#Get doc-topic proportions (theta)
theta <- as.matrix(lda_unsup$theta)   # docs x topics

# Get year from docvars (preferred)
yr <- docvars(dfm_tm, "year")

# Fallback: if year is missing, derive from docnames like "1933-03-12"
if (is.null(yr)) {
  yr <- as.integer(substr(docnames(dfm_tm), 1, 4))
} else {
  yr <- as.integer(as.character(yr))
}

stopifnot(!anyNA(yr))

# Aggregate topic share by year (mean across docs in that year)
theta_df <- data.frame(year = yr, theta, check.names = FALSE)
topic_share_by_year <- aggregate(theta_df[, -1], by = list(year = theta_df$year), FUN = mean)

# Apply your topic names
colnames(topic_share_by_year)[-1] <- topic_names

# CHECK TOP TERMS BY TOPIC (diagnostics)

# Print your existing top_terms_by_topic with topic names
for (k in seq_along(top_terms_by_topic)) {
  cat("\n", "Topic", k, "—", topic_names[k], "\n", sep = " ")
  cat(paste(top_terms_by_topic[[k]], collapse = ", "), "\n")
}

# Top terms WITH phi weights (most important diagnostic)
top_n_check <- 10
top_terms_phi <- lapply(seq_len(nrow(phi_mat)), function(k) {
  probs <- sort(phi_mat[k, ], decreasing = TRUE)[1:top_n_check]
  data.frame(
    topic = k,
    topic_name = topic_names[k],
    term = names(probs),
    phi = round(as.numeric(probs), 4),
    row.names = NULL
  )
})

# Print each topic as a small table
for (k in seq_along(top_terms_phi)) {
  cat("\n", "=== Topic", k, ":", topic_names[k], "===\n")
  print(top_terms_phi[[k]])
}

# Distinctiveness check (filters out globally common terms like "government")
#    Higher = more topic-specific.
phi_norm <- sweep(phi_mat, 2, colSums(phi_mat), "/")

top_distinct_terms <- lapply(seq_len(nrow(phi_norm)), function(k) {
  probs <- sort(phi_norm[k, ], decreasing = TRUE)[1:top_n_check]
  data.frame(
    topic = k,
    topic_name = topic_names[k],
    term = names(probs),
    distinct_score = round(as.numeric(probs), 4),
    row.names = NULL
  )
})

for (k in seq_along(top_distinct_terms)) {
  cat("\n", "*** Distinctive terms — Topic", k, ":", topic_names[k], "***\n")
  print(top_distinct_terms[[k]])
}

# Optional: one combined table (easy to export)
top_terms_table <- do.call(rbind, top_terms_phi)
top_terms_table_distinct <- do.call(rbind, top_distinct_terms)

# Ensure each year's shares sum to 1 (numerical safety)
Y <- as.matrix(topic_share_by_year[, -1, drop = FALSE])
Y <- Y / rowSums(Y)

years <- topic_share_by_year$year

# Stacked area plot in base R
Y_cum <- t(apply(Y, 1, cumsum))

# Layout adjustments
# allow drawing outside plot + larger bottom margin
par(mar = c(7, 4, 3, 2), xpd = NA)

plot(
  years, rep(1, length(years)),
  type = "n",
  xlab = "Year",
  ylab = "Mean topic share",
  main = "Topic prevalence over time (stacked)",
  ylim = c(0, 1)
)

cum <- rep(0, length(years))

for (k in 1:nrow(mat)) {
  upper <- cum + mat[k, ]
  
  polygon(
    c(years, rev(years)),
    c(upper, rev(cum)),
    col = cols[k],
    border = NA
  )
  
  cum <- upper
}

# legend below plot
legend(
  x = par("usr")[1], y = -0.25,
  legend = short_labels,
  fill = cols,
  ncol = 2,
  cex = 0.8,
  bty = "n",
  xjust = 0
)