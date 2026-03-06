# Set the working directory 
setwd("~/Dropbox/Mac/Desktop/Quantitative Text Analysis/Assignment/Code")

#Clear Workspace and memory
rm(list = ls())
gc()
set.seed(1234)

# Install packages and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quanteda, stm, readxl, dplyr)

obj <- readRDS("./CBB_corpus_and_tokens_clean.rds")
names(obj)
corp <- obj$corpus
toks <- obj$toks_nouns

ttl <- docvars(corp, "title")

docvars(toks) <- docvars(corp)
docvars(toks)

dfm <- dfm(toks)
nfeat(dfm)

dfm <- dfm_trim(dfm, min_termfreq = 5, min_docfreq = 2)
domain_stop <- c("bank", "year", "rate", "barbado", "barbad", "mr")  # add as you inspect
dfm <- dfm_remove(dfm, pattern = domain_stop)
time_stop <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
dfm <- dfm_remove(dfm, pattern = time_stop)
junk_stop <- c("insight.bb", "pp")  # add as they appear
dfm <- dfm_remove(dfm, pattern = junk_stop)


#prepare stm input
stm_input <- convert(dfm, to = "stm")

docs <- stm_input$documents
vocab <- stm_input$vocab
meta <- stm_input$meta

#library(readxl)
macro <- read_excel("~/Dropbox/Mac/Desktop/Quantitative Text Analysis/Assignment/Barbados Statistics.xlsx")


macro <- macro |>
  dplyr::filter(Year >=  min(meta$year, na.rm = TRUE), Year <= max(meta$year, na.rm = TRUE)) |>
   dplyr::transmute(
     year = as.integer(Year),
     infl_12m = `PRIOR 12 MONTH INFLATION RATE %`,
     unemp = `Unemployment rate (Percent)`,        # or Yearly Unemployment Rate
     debt_cg = `Central Government Debt (Percent of GDP)`,
     wage_g = `Wage growth (%)`*100,
     gdp_g = `GDP per capita Growth, current prices (PPPP)`*100
   )

summary(macro)

# ================================================================
# Plot 5 macro series over time (base R)
# ================================================================

# Keep only needed columns, drop missing years, sort
macro_plot <- macro |>
  dplyr::select(year, infl_12m, unemp, debt_cg, wage_g, gdp_g) |>
  dplyr::filter(!is.na(year)) |>
  dplyr::arrange(year)

# 1) Inflation
plot(macro_plot$year, macro_plot$infl_12m, type = "l",
     xlab = "Year", ylab = "Inflation (prior 12m, %)",
     main = "Inflation")

# 2) Unemployment
plot(macro_plot$year, macro_plot$unemp, type = "l",
     xlab = "Year", ylab = "Unemployment rate (%)",
     main = "Unemployment")

# 3) Debt (% GDP)
plot(macro_plot$year, macro_plot$debt_cg, type = "l",
     xlab = "Year", ylab = "Central government debt (% GDP)",
     main = "Debt (% GDP)")

# 4) Wage growth
plot(macro_plot$year, macro_plot$wage_g, type = "l",
     xlab = "Year", ylab = "Wage growth (%)",
     main = "Wage growth")

# 5) GDP per capita growth
plot(macro_plot$year, macro_plot$gdp_g, type = "l",
     xlab = "Year", ylab = "GDP per capita growth (%)",
     main = "GDP per capita growth")

# ================================================================
# Export 5 macro series in one vertical PDF
# ================================================================

pdf("macro_series_barbados.pdf", width = 7, height = 14)

par(mfrow = c(5,1), mar = c(3,4,2,1))   # 5 rows, 1 column

# 1) Inflation
plot(macro_plot$year, macro_plot$infl_12m, type = "l",
     xlab = "Year", ylab = "Inflation (%)",
     main = "Inflation")

# 2) Unemployment
plot(macro_plot$year, macro_plot$unemp, type = "l",
     xlab = "Year", ylab = "Unemployment (%)",
     main = "Unemployment")

# 3) Debt (% GDP)
plot(macro_plot$year, macro_plot$debt_cg, type = "l",
     xlab = "Year", ylab = "Debt (% GDP)",
     main = "Central Government Debt")

# 4) Wage growth
plot(macro_plot$year, macro_plot$wage_g, type = "l",
     xlab = "Year", ylab = "Wage growth (%)",
     main = "Wage Growth")

# 5) GDP per capita growth
plot(macro_plot$year, macro_plot$gdp_g, type = "l",
     xlab = "Year", ylab = "GDP per capita growth (%)",
     main = "GDP per Capita Growth")

dev.off()

# ---- Event / regime dummies ----
# meta <- meta |>
#   mutate(
#     imf_nego = as.integer(year >= 1991 & year <= 1993),  # adjust window if needed
#     gfc      = as.integer(year >= 2008 & year <= 2009),  # optional: 2007-2010
#     covid    = as.integer(year >= 2020 & year <= 2021)                  
#   )


covars_used <- c("year")

keep <- complete.cases(meta[, covars_used])
sum(!keep)  # how many documents will be dropped: 0

docs_k <- docs[keep]
meta_k <- meta[keep, , drop = FALSE]
prep <- prepDocuments(documents = docs_k, vocab = vocab, meta = meta_k)

docs_k <- prep$documents
vocab_k <- prep$vocab
meta_k  <- prep$meta
# ================================================================
# Choose number of topics K for an STM
# ================================================================

# Define candidate values of K (number of topics) to test
# Smaller K = broader topics; larger K = more granular topics
 k_list <- c(2,3,4,5, 6, 7, 8, 9, 10)
# 
# # Run STM’s K search procedure
# # This fits multiple STM models (one per K) and computes diagnostics:
# #   - held-out likelihood
# #   - residuals
# #   - semantic coherence
# #   - lower bound

k_res <- searchK(
  documents  = docs_k,
  vocab      = vocab_k,
  data       = meta_k,
  K          = k_list,
  prevalence = ~ s(year),
  verbose    = FALSE
)

plot(k_res) #4-6 best, choose 5

K_topics <- 5

stm_fit <- stm(
  documents = docs_k,
  vocab = vocab_k,
  K = K_topics,
  prevalence = ~ s(year),
  data = meta_k,
  init.type = "Spectral",
  max.em.its = 100,
  verbose = FALSE
)

stm_topics <- labelTopics(stm_fit, n = 10)
print(stm_topics)

# ================================================================
# STM: label topics + estimate effects + plot topic prevalence by year
# ================================================================

# ---- Topic labels ----
topic_labels <- c(
  "External sector & commodity economy (trade/BoP, sugar, trade finance)",
  "Currency integrity, governance & financial supervision",
  "Trade, taxation & macro stabilisation",
  "Modernisation & operational resilience (digital/cyber/pandemic)",
  "Liberalisation/integration & departmental macro reporting"
)

# sanity check
stopifnot(length(topic_labels) == stm_fit$settings$dim$K)

# ---- Estimate effects (smooth time + macro covariates + event dummies) ----
eff <- estimateEffect(
  1:K_topics ~ s(year),
  stmobj = stm_fit,
  metadata = meta_k,
  uncertainty = "Global"
)

print(summary(eff))

plot(eff, "year", method = "continuous",
     topics = 1:K_topics,
     model = stm_fit,
     printlegend = TRUE,
     xlab = "Year",
     ylab = "Expected topic prevalence")

# ---- Mean topic share by year (simple lines) ----
theta <- stm_fit$theta  # document-topic proportions (docs x K)

topic_by_year <- aggregate(theta, by = list(year = meta_k$year), FUN = mean)
topic_by_year <- topic_by_year[order(topic_by_year$year), ]

op <- par(no.readonly = TRUE)              # save current graphics settings
on.exit(par(op), add = TRUE)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

for (k in 1:K_topics) {
  plot(
    topic_by_year$year, topic_by_year[[k + 1]],
    type = "b",
    xlab = "Year", ylab = "Mean topic share",
    main = paste0("Topic ", k, ": ", topic_labels[k])
  )
}
par(mfrow = c(1, 1))
par(op) 

# ---- Stacked area plot (RStudio-friendly) ----

years <- topic_by_year$year
mat <- t(as.matrix(topic_by_year[, -1]))   # rows = topics, cols = years

# numerical safety
mat <- sweep(mat, 2, colSums(mat), "/")

# Okabe–Ito palette
cols <- c("#E69F00", "#CC79A7","#56B4E9", "#009E73", "#F0E442")[1:K_topics]

short_labels <- paste0(
  "T", 1:K_topics, " ",
  gsub("^Topic [0-9]+:\\s*", "", topic_labels)
)

# normal margins
# allow drawing outside plot + larger bottom margin
par(mar = c(6, 4, 3, 2), xpd = NA)

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
  x = par("usr")[1], y = -0.3,
  legend = short_labels,
  fill = cols,
  ncol = 1,
  cex = 0.75,
  bty = "n",
  xjust = 0
)
