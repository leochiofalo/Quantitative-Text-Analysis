setwd("~/Dropbox/Mac/Desktop/Quantitative Text Analysis/Assignment/Code")
rm(list = ls()); gc()

if (!require("pacman")) install.packages("pacman")
pacman::p_load(quanteda, ggplot2, tidyr, dplyr, remotes)

# install ONCE (comment out after first successful install)
# remotes::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.dictionaries)

obj <- readRDS("./CBB_corpus_and_tokens_clean.rds")
corp <- obj$corpus
toks <- obj$tokens_clean

docvars(toks) <- docvars(corp)

# -------------------------------
# Target dictionaries
# -------------------------------
targets_prices       <- c("price*", "inflation*", "deflation*", "cost*", "rent*")
targets_wages        <- c("wage*", "salary*", "pay*", "earn*")
targets_fiscal       <- c("tax*", "revenue*", "budget*", "expenditure*")
targets_debt         <- c("debt*", "bond*", "loan*", "credit*", "interest*")
targets_employment   <- c("employ*", "job*", "workforce")
targets_unemployment <- c("unemploy*")

# -------------------------------
# Context windows around targets
# -------------------------------
toks_prices <- tokens_select(toks, targets_prices, selection = "keep", window = 10, valuetype = "glob")
toks_wages  <- tokens_select(toks, targets_wages, selection = "keep", window = 10, valuetype = "glob")
toks_fiscal <- tokens_select(toks, targets_fiscal, selection = "keep", window = 10, valuetype = "glob")
toks_debt   <- tokens_select(toks, targets_debt, selection = "keep", window = 10, valuetype = "glob")
toks_emp    <- tokens_select(toks, targets_employment, selection = "keep", window = 10, valuetype = "glob")
toks_unemp  <- tokens_select(toks, targets_unemployment, selection = "keep", window = 10, valuetype = "glob")

# -------------------------------
# Positive / negative dictionary
# -------------------------------
dict_posneg <- data_dictionary_LSD2015[1:2]

toks_prices_lsd <- tokens_lookup(toks_prices, dictionary = dict_posneg)
toks_wages_lsd  <- tokens_lookup(toks_wages, dictionary = dict_posneg)
toks_fiscal_lsd <- tokens_lookup(toks_fiscal, dictionary = dict_posneg)
toks_debt_lsd   <- tokens_lookup(toks_debt, dictionary = dict_posneg)
toks_emp_lsd    <- tokens_lookup(toks_emp, dictionary = dict_posneg)
toks_unemp_lsd  <- tokens_lookup(toks_unemp, dictionary = dict_posneg)

# -------------------------------
# Group by year
# -------------------------------
dfmat_prices_lsd <- dfm_group(dfm(toks_prices_lsd), groups = docvars(toks_prices_lsd, "year"))
dfmat_wages_lsd  <- dfm_group(dfm(toks_wages_lsd), groups = docvars(toks_wages_lsd, "year"))
dfmat_fiscal_lsd <- dfm_group(dfm(toks_fiscal_lsd), groups = docvars(toks_fiscal_lsd, "year"))
dfmat_debt_lsd   <- dfm_group(dfm(toks_debt_lsd), groups = docvars(toks_debt_lsd, "year"))
dfmat_emp_lsd    <- dfm_group(dfm(toks_emp_lsd), groups = docvars(toks_emp_lsd, "year"))
dfmat_unemp_lsd  <- dfm_group(dfm(toks_unemp_lsd), groups = docvars(toks_unemp_lsd, "year"))

# -------------------------------
# Helper: counts -> long
# -------------------------------
dfm_to_long <- function(dfm_obj, window_name) {
  d <- convert(dfm_obj, to = "data.frame")
  names(d)[1] <- "year"
  
  pivot_longer(d, cols = -year, names_to = "sentiment", values_to = "count") |>
    mutate(
      year = as.integer(as.character(year)),
      window = window_name
    )
}

df_long <- bind_rows(
  dfm_to_long(dfmat_prices_lsd, "Prices"),
  dfm_to_long(dfmat_wages_lsd, "Wages"),
  dfm_to_long(dfmat_fiscal_lsd, "Fiscal"),
  dfm_to_long(dfmat_debt_lsd, "Debt"),
  dfm_to_long(dfmat_emp_lsd, "Employment"),
  dfm_to_long(dfmat_unemp_lsd, "Unemployment")
) |>
  arrange(window, year)

# -------------------------------
# Plot raw positive/negative counts
# -------------------------------
ggplot(df_long, aes(x = year, y = count, linetype = sentiment)) +
  geom_line() +
  facet_wrap(~ window, scales = "free_y", ncol = 1) +
  labs(
    x = "Year",
    y = "Count (±10-token windows)",
    linetype = "Sentiment",
    title = "Positive vs Negative terms around economic targets"
  ) +
  theme_minimal()

# -------------------------------
# Net sentiment from counts
# -------------------------------
df_net <- df_long |>
  pivot_wider(names_from = sentiment, values_from = count, values_fill = 0) |>
  mutate(net = positive - negative) |>
  arrange(window, year)

ggplot(df_net, aes(x = year, y = net)) +
  geom_line() +
  facet_wrap(~ window, scales = "free_y", ncol = 1) +
  labs(
    x = "Year",
    y = "Net sentiment (positive - negative)",
    title = "Net sentiment in context windows"
  ) +
  theme_minimal()
