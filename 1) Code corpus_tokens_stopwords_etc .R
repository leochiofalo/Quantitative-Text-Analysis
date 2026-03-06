# Set the working directory 
setwd("~/Dropbox/Mac/Desktop/Quantitative Text Analysis/Assignment/Code")

#Clear Workspace and memory
rm(list = ls())
gc()

##### Load data set #####
# Install packages and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quanteda, udpipe, stopwords)

#### Load CBB corp ####
dat <- readRDS("./CBB_corpus.rds")
names(dat)

corp <- corpus(
  x    = dat
)

# Number of documents
ndoc(corp) #52

# View first few rows of document variables (titles, dates, etc.)
print(head(docvars(corp), 5), row.names = FALSE)

# Display first 500 characters of the first document's text
cat("\nFirst document snippet:\n")
cat(substr(as.character(corp)[1], 1, 500), "...\n")

# Display summary of first document (length, tokens, etc.)
summary(corp[1]) 
# Corpus consisting of 1 document, showing 1 document:
# Text        Types Tokens Sentences year      title
# Report1973  2908  18535       734  1973 Report1973

# docvars
docvars(corp)
print(names(docvars(corp))) #"year" "title"

# Add corpus-level metadata
meta(corp)

meta(corp, "title")        <- "Central Bank of Barbados Corpus"
meta(corp, "author")       <- "Central Bank of Barbados"
meta(corp, "compiler")     <- "Eric Strobl & Leo Chiofalo"
meta(corp, "years")        <- "1971–2020"
meta(corp, "description")  <- "Corpus of Central Bank of Barbados` annual reports, cleaned and tokenized for text analysis."
meta(corp, "source")       <- "Central Bank of Barbados"
meta(corp, "created_on")   <- Sys.Date()
meta(corp, "version")      <- "1.0"

meta(corp)

# Collapsing Data
# # Create sentence-level corpus: each sentence becomes a document
# corp_sentences <- corpus_reshape(corp, to = "sentences")
# 
# # Report number of sentence-documents
# cat("Number of sentences:", ndoc(corp_sentences), "\n") #Number of sentences: 34435 
# 
# # Create paragraph-level corpus: each paragraph becomes a document
# #corp_paragraphs <- corpus_reshape(corp, to = "paragraphs")
# 
# # Report number of paragraph-documents
# #cat("Number of paragraphs:", ndoc(corp_paragraphs), "\n") #same as documents
# 
# rm("corp_paragraphs", "corp_sentences", "dat")

# tokens() command
# Tokenize the original corpus with default settings (no cleaning yet - just the tokens)
toks <- tokens(corp)
# Show selection of tokens for a few documents
print(toks)

# Show number of tokens by document
ntoken(toks)

# Show number of tokens for entire corpus
sum(ntoken(toks))

# Show unique number of tokens by document & by corpus
ntype(toks)
length(types(toks)) # 27086

#### Start cleaning tokens ####
# Convert tokens to lowercase
toks_clean <- tokens_tolower(toks)
print(toks_clean)

#Remove punctuation, separators, and split hyphens
toks_clean <- tokens(
  toks_clean,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_separators = TRUE,
  split_hyphens = TRUE
)

print(toks_clean)

#Remove single letter words (but this will depend on context)
toks_clean <- tokens_remove(toks_clean, pattern = "^[a-z]$", valuetype = "regex")

# Remove very common web/tech artifacts (optional)
toks_clean <- tokens_remove(
  toks_clean,
  pattern = c("http*", "www*", ".com", ".org", ".pdf"),
  valuetype = "glob"
)
print(toks_clean)

# Define a list of number words (written-out numerals) to remove
# Recall we already removed numerical numbers
number_words <- c(
  "zero","one","two","three","four","five","six","seven","eight","nine","ten",
  "eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen",
  "eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy",
  "eighty","ninety","hundred","thousand","million","billion"
)

# Remove those number words from toks_clean
toks_clean <- tokens_remove(toks_clean, pattern = number_words)

topfeatures(dfm(toks_clean), 50)

# ----  Remove stopwords ----
"us"   %in% stopwords("en", source = "snowball")
sw <- stopwords("en", source = "snowball")
sw
sw_keep <- c("not","no","nor","never","without","cannot",
             "above", "below", "up", "down", "over", "under")
sw2 <- setdiff(sw, sw_keep)

toks_nostop <- tokens_remove(toks_clean, pattern = sw2)

topfeatures(dfm(toks_nostop), 50)

# Count tokens before vs after stopword removal
cat("Tokens before cleaning:", sum(ntoken(toks)),  "\n") #926123
cat("Tokens before stopwords removal:", sum(ntoken(toks_clean)),  "\n") #741773
cat("Tokens after stopwords removal:",  sum(ntoken(toks_nostop)), "\n") #459486

boiler <- c("report", "table", "figure", "appendix", "chapter", "section", "page"
            )

toks_nostop <- tokens_remove(toks_nostop, pattern = boiler, valuetype = "fixed")

cat("Tokens after sectionwords removal:",  sum(ntoken(toks_nostop)), "\n") #457273

##### Lemmatize using UDPipe model #####
# --- UDPipe model ---
model_path <- "~/Dropbox/Mac/Desktop/Quantitative Text Analysis/english-ewt-ud-2.5-191206.udpipe"
ud_model <- udpipe_load_model(model_path)

# Convert tokens -> one text string per doc (preserve order + docnames)
all_ids <- docnames(toks_nostop)
txts <- sapply(toks_nostop, paste, collapse = " ", USE.NAMES = TRUE)
txts <- txts[all_ids]
stopifnot(identical(names(txts), all_ids))

zeit <- system.time({
  anno <- udpipe_annotate(ud_model, x = txts, doc_id = names(txts))
})
print(zeit)
# user  system elapsed 
# 313.196   0.766 314.204 

anno_df <- as.data.frame(anno)

# Use lemma (fallback to token)
lem <- ifelse(is.na(anno_df$lemma) | anno_df$lemma == "", anno_df$token, anno_df$lemma)

# Split back to docs in original order
lem_list <- split(lem, anno_df$doc_id)

# FORCE full doc set + same order as all_ids (critical)
lem_list <- lapply(all_ids, function(id) if (id %in% names(lem_list)) lem_list[[id]] else character(0))
names(lem_list) <- all_ids

texts_lem <- vapply(lem_list, paste, collapse = " ", FUN.VALUE = character(1))
toks_lem <- tokens(texts_lem)

toks_lem <- tokens(
  toks_lem,
  remove_punct   = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  split_hyphens  = TRUE
)

# remove leftover apostrophe artifacts explicitly
toks_lem <- tokens_remove(toks_lem, pattern = c("'", "’"), valuetype = "fixed")

# remove single-letter again (after lemmatization)
toks_lem <- tokens_remove(toks_lem, pattern = "^[a-z]$", valuetype = "regex")
docnames(toks_lem) <- all_ids
docvars(toks_lem) <- docvars(corp)


# Nouns only #
nouns_only <- subset(anno_df, upos %in% c("NOUN","PROPN"))
nouns_only$lemma2 <- ifelse(is.na(nouns_only$lemma) | nouns_only$lemma=="", nouns_only$token, nouns_only$lemma)
nouns_list <- split(nouns_only$lemma2, nouns_only$doc_id)


# Force all docs to exist
nouns_list <- lapply(all_ids, function(id) if (id %in% names(nouns_list)) nouns_list[[id]] else character(0))
names(nouns_list) <- all_ids

texts_nouns <- vapply(nouns_list, paste, collapse = " ", FUN.VALUE = character(1))
toks_nouns <- tokens(texts_nouns)
docnames(toks_nouns) <- all_ids
docvars(toks_nouns) <- docvars(corp)

domain_boiler <- c("bank","central","barbados","annual","report")
toks_topic <- tokens_remove(toks_lem, domain_boiler, valuetype="fixed")
dfm_topic <- dfm(toks_topic)

docvars(dfm_topic) <- docvars(corp)

#
saveRDS(
  list(
    corpus       = corp,
    tokens_clean = toks_lem,
    toks_nouns   = toks_nouns,
    dfm_topic    = dfm_topic
  ),
  file = "./CBB_corpus_and_tokens_clean.rds"
)
