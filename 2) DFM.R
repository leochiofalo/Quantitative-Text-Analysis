# Set the working directory 
setwd("~/Dropbox/Mac/Desktop/Quantitative Text Analysis/Assignment/Code")

#Clear Workspace and memory
rm(list = ls())
gc()

##### Load data set #####
# Install packages and load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(quanteda, quanteda.textstats, remotes, quanteda.textplots, 
          wordcloud,ggplot2, RColorBrewer, dplyr, stringr) 

remotes::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.dictionaries)

#Load in previously saved data
obj <- readRDS("./CBB_corpus_and_tokens_clean.rds")
names(obj) #"corpus" "tokens_clean" "toks_nouns" "dfm_topic"   

# Extract each object
corp <- obj$corpus
toks_all <- obj$tokens_clean
toks_nouns <- obj$toks_nouns


##### Create DFM with dfm() command #####
dfmat <- dfm(toks_all)
dfm_nouns <- dfm(toks_nouns)
dfm_topic <- obj$dfm_topic

print(dfmat)
dim(dfmat) # 52 13335
dim(dfm_nouns) #52 8383
dim(dfm_topic) #  52 13330

# how many elements are zero:
sparsity(dfmat) #0.8485752
sparsity(dfm_nouns) #0.8641436
sparsity(dfm_topic) #0.8488632

# Summary commands for dfm:
head(rowSums(dfmat), 10)
head(colSums(dfmat), 10)

topfeatures(dfmat, 30)
topfeatures(dfm_nouns, 30)
topfeatures(dfm_topic, 30)


##### Annual proportion of dictionary-related tokens (CBB corpus) #####
##### Plots: (A) all tokens, (B) nouns-only (optional) ###################

# attach year to dfm(s) (dfm inherits docvars from tokens, but make it explicit)
docvars(dfmat) <- docvars(corp)
docvars(dfmat, "year") <- as.character(docvars(dfmat, "year"))

# If you want a nouns-only version too
if (exists("toks_nouns")) {
  docvars(toks_nouns) <- docvars(corp)
  docvars(toks_nouns, "year") <- as.character(docvars(toks_nouns, "year"))
  dfm_nouns <- dfm(toks_nouns)
}

# ---------------------------
# Build CBB topic dictionary
# ---------------------------
dict_cbb <- dictionary(list(
  inflation_prices = c("inflation*", "price*", "cpi", "cost*", "deflation*", "index", 
                       "basket"),
  growth_economy   = c("growth", "gdp", "output", "activity", "recession*", "recover*", 
                       "slowdown*", "expansion*"),
  labour_market    = c("employ*", "unemploy*", "job*", "wage*", "salary*", "labour*", 
                       "workforce"),
  fx_reserves      = c("exchange*", "fx", "reserve*", "peg*", 
                       "parity", "devalu*", "convertib*", "reserve*"), 
  external_sector  = c("import*", "export*", "account", "account", 
                       "bop", "balance", "external", 
                       "foreign"),
  fiscal_debt      = c("fiscal*", "budget*", "revenue*", "expend*", "spend*", "tax*", "vat",
                       "deficit*", "surplus", "debt*", "bond*", "sovereign", "restructur*", "imf", "program*"),
  financial_system = c("credit*", "loan*", "lending", "borrow*", "interest*", 
                       "mortgage*", "deposit*", "liquid*", "stabil*", "risk*", 
                       "stress", "shock*", "solvenc*", "capital_adequacy", "npl*", 
                       "nonperform*", "arrears"),
  regulation_supervision = c("regulat*", "supervis*", "prudential",
                             "licen*", "inspect*", "compliance","basel", "aml", "cft","crime", "sanction*"),
  
  tourism_energy   = c("touris*", "visitor*", "arrival*", "hotel*", "cruise", "travel", "receipt*",
                     "oil", "petrol*", "fuel", "energy", "electric*", "gas", "commodit*" ),
  shocks           = c("covid*", "pandemic*", "lockdown*", "vaccine*", "health", 
                       "hurricane*", "storm*", "flood*", "disaster*", "climate",
                       "resilien*", "adapt*", "insurance")
))

dict_countries <- dictionary(list(
  
  united_states = c("united states", "united states of america", "usa", "u.s.", "u.s.a.", "america"),
  united_kingdom = c("united kingdom", "uk", "u.k.", "britain", "great britain", "england"),
  canada = c("canada"),
  mexico = c("mexico"),
  
  argentina = c("argentina"),
  bolivia = c("bolivia"),
  brazil = c("brazil"),
  chile = c("chile"),
  colombia = c("colombia"),
  costa_rica = c("costa rica"),
  cuba = c("cuba"),
  dominican_republic = c("dominican republic"),
  ecuador = c("ecuador"),
  el_salvador = c("el salvador"),
  guatemala = c("guatemala"),
  haiti = c("haiti"),
  honduras = c("honduras"),
  jamaica = c("jamaica"),
  nicaragua = c("nicaragua"),
  panama = c("panama"),
  paraguay = c("paraguay"),
  peru = c("peru"),
  uruguay = c("uruguay"),
  venezuela = c("venezuela"),
  
  barbados = c("barbados"),
  trinidad_tobago = c("trinidad", "trinidad and tobago"),
  bahamas = c("bahamas"),
  guyana = c("guyana"),
  suriname = c("suriname"),
  
  germany = c("germany"),
  france = c("france"),
  italy = c("italy"),
  spain = c("spain"),
  portugal = c("portugal"),
  netherlands = c("netherlands", "holland"),
  belgium = c("belgium"),
  switzerland = c("switzerland"),
  austria = c("austria"),
  sweden = c("sweden"),
  norway = c("norway"),
  denmark = c("denmark"),
  finland = c("finland"),
  ireland = c("ireland"),
  poland = c("poland"),
  greece = c("greece"),
  turkey = c("turkey"),
  
  russia = c("russia", "russian federation"),
  ukraine = c("ukraine"),
  
  china = c("china", "people's republic of china", "prc"),
  japan = c("japan"),
  south_korea = c("south korea", "korea"),
  north_korea = c("north korea"),
  india = c("india"),
  pakistan = c("pakistan"),
  bangladesh = c("bangladesh"),
  sri_lanka = c("sri lanka"),
  indonesia = c("indonesia"),
  malaysia = c("malaysia"),
  singapore = c("singapore"),
  thailand = c("thailand"),
  vietnam = c("vietnam"),
  philippines = c("philippines"),
  
  saudi_arabia = c("saudi arabia"),
  uae = c("united arab emirates", "uae"),
  israel = c("israel"),
  iran = c("iran"),
  iraq = c("iraq"),
  qatar = c("qatar"),
  kuwait = c("kuwait"),
  oman = c("oman"),
  
  australia = c("australia"),
  new_zealand = c("new zealand"),
  
  south_africa = c("south africa"),
  nigeria = c("nigeria"),
  kenya = c("kenya"),
  egypt = c("egypt"),
  morocco = c("morocco"),
  ghana = c("ghana"),
  ethiopia = c("ethiopia"),
  tanzania = c("tanzania")
  
))
# ---------------------------
# Helper: annual shares
# ---------------------------
annual_shares <- function(dfm_x, dict, year_var = "year") {
  
  # lookup counts per doc
  dfm_lookup_x <- dfm_lookup(dfm_x, dictionary = dict)
  
  # per-doc totals
  total_tokens <- ntoken(dfm_x)
  
  # docvar year
  y <- as.character(docvars(dfm_x, year_var))
  
  # matrix of category counts (docs x categories)
  X <- as.matrix(dfm_lookup_x)
  
  # aggregate by year
  annual_counts <- aggregate(X, by = list(year = y), FUN = sum, na.rm = TRUE)
  annual_totals <- aggregate(total_tokens, by = list(year = y), FUN = sum, na.rm = TRUE)
  names(annual_totals)[2] <- "all_tokens"
  
  annual <- merge(annual_counts, annual_totals, by = "year", all = TRUE)
  annual <- annual[order(as.integer(annual$year)), ]
  
  # convert counts -> proportions
  cats <- setdiff(names(annual), c("year", "all_tokens"))
  for (cc in cats) annual[[paste0("prop_", cc)]] <- annual[[cc]] / annual$all_tokens
  
  annual
}

# ---------------------------
# Thematic groups
# ---------------------------
topic_groups <- list(
  macro_environment = c("inflation_prices", "growth_economy", "shocks", "labour_market"),
  external_sector   = c("fx_reserves", "external_sector", "tourism_energy"),
  financial_stability = c("financial_system", "regulation_supervision", "fiscal_debt")
)

# ---------------------------
# Plot function for ONE group
# ---------------------------
plot_group_annual_props <- function(annual,
                                    topics,
                                    group_title = "Annual share (group)",
                                    legend_pos = "topright",
                                    ylim = NULL) {
  
  years <- as.integer(annual$year)
  prop_cols <- paste0("prop_", topics)
  
  missing_cols <- prop_cols[!prop_cols %in% names(annual)]
  if (length(missing_cols) > 0) {
    stop("Missing series: ",
         paste(gsub("^prop_", "", missing_cols), collapse = ", "))
  }
  
  cols <- RColorBrewer::brewer.pal(max(3, min(length(prop_cols), 8)), "Dark2")[seq_along(prop_cols)]
  
  # if ylim not supplied, compute locally
  if (is.null(ylim)) {
    ylim <- range(annual[, prop_cols, drop = FALSE], na.rm = TRUE)
  }
  
  plot(years, annual[[prop_cols[1]]],
       type = "l",
       lwd = 2,
       col = cols[1],
       xlab = "Year",
       ylab = "Proportion of tokens",
       main = group_title,
       ylim = ylim)
  
  if (length(prop_cols) > 1) {
    for (i in 2:length(prop_cols)) {
      lines(years, annual[[prop_cols[i]]], col = cols[i], lwd = 2)
    }
  }
  
  legend(legend_pos,
         legend = topics,
         col = cols,
         lwd = 2,
         bty = "n",
         cex = 0.9)
}

# ---------------------------
# Wrapper: plot ALL groups (optionally in one 1x3 panel)
# ---------------------------
plot_all_groups <- function(annual, groups, main_prefix = "CBB: annual share") {
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  
  # find ALL series used across groups
  all_topics <- unique(unlist(groups))
  prop_cols <- paste0("prop_", all_topics)
  
  # global y-axis range
  global_ylim <- range(annual[, prop_cols, drop = FALSE], na.rm = TRUE)
  
  par(mfrow = c(1,3), mar = c(4,4,3,1))
  
  plot_group_annual_props(
    annual,
    topics = groups$macro_environment,
    group_title = paste0(main_prefix, " — Macroeconomic environment"),
    ylim = global_ylim
  )
  
  plot_group_annual_props(
    annual,
    topics = groups$external_sector,
    group_title = paste0(main_prefix, " — External sector"),
    ylim = global_ylim
  )
  
  plot_group_annual_props(
    annual,
    topics = groups$financial_stability,
    group_title = paste0(main_prefix, " — Financial stability"),
    ylim = global_ylim
  )
}

##### FINAL SECTION: build annual shares + SAVE 3 SEPARATE THEMATIC FIGURES #####

# --- ALL TOKENS: compute annual shares ---
annual_all <- annual_shares(dfmat, dict_cbb, year_var = "year")

# --- common y-axis across all three figures (all tokens) ---
all_topics <- unique(unlist(topic_groups))
prop_cols  <- paste0("prop_", all_topics)
global_ylim_all <- range(annual_all[, prop_cols, drop = FALSE], na.rm = TRUE)

# --- save Macroeconomic environment ---
pdf("CBB_macro_environment_all_tokens.pdf", width = 6, height = 4)
plot_group_annual_props(
  annual_all,
  topics = topic_groups$macro_environment,
  group_title = "CBB (all tokens): Macroeconomic environment",
  ylim = global_ylim_all
)
dev.off()

# --- save External sector ---
pdf("CBB_external_sector_all_tokens.pdf", width = 6, height = 4)
plot_group_annual_props(
  annual_all,
  topics = topic_groups$external_sector,
  group_title = "CBB (all tokens): External sector",
  ylim = global_ylim_all
)
dev.off()

# --- save Financial stability ---
pdf("CBB_financial_stability_all_tokens.pdf", width = 6, height = 4)
plot_group_annual_props(
  annual_all,
  topics = topic_groups$financial_stability,
  group_title = "CBB (all tokens): Financial stability",
  ylim = global_ylim_all
)
dev.off()


##### NOUNS-ONLY VERSION (same 3 figures) #####
if (exists("dfm_nouns")) {
  
  # ensure docvars are attached
  docvars(dfm_nouns) <- docvars(corp)
  docvars(dfm_nouns, "year") <- as.character(docvars(dfm_nouns, "year"))
  
  # compute annual shares (nouns)
  annual_nouns <- annual_shares(dfm_nouns, dict_cbb, year_var = "year")
  
  # common y-axis across all three figures (nouns)
  global_ylim_nouns <- range(annual_nouns[, prop_cols, drop = FALSE], na.rm = TRUE)
  
  # Macroeconomic environment (nouns)
  pdf("CBB_macro_environment_nouns_only.pdf", width = 6, height = 4)
  plot_group_annual_props(
    annual_nouns,
    topics = topic_groups$macro_environment,
    group_title = "CBB (nouns only): Macroeconomic environment",
    ylim = global_ylim_nouns
  )
  dev.off()
  
  # External sector (nouns)
  pdf("CBB_external_sector_nouns_only.pdf", width = 6, height = 4)
  plot_group_annual_props(
    annual_nouns,
    topics = topic_groups$external_sector,
    group_title = "CBB (nouns only): External sector",
    ylim = global_ylim_nouns
  )
  dev.off()
  
  # Financial stability (nouns)
  pdf("CBB_financial_stability_nouns_only.pdf", width = 6, height = 4)
  plot_group_annual_props(
    annual_nouns,
    topics = topic_groups$financial_stability,
    group_title = "CBB (nouns only): Financial stability",
    ylim = global_ylim_nouns
  )
  dev.off()
}

##### COUNTRIES: annual proportions (drop never-mentioned countries) #####
# docvars
docvars(toks_all) <- docvars(corp)
docvars(toks_all, "year") <- as.character(docvars(toks_all, "year"))

# 1–2 grams for multiword countries
toks_country <- tokens_ngrams(toks_all, n = 1:2)
dfm_country  <- dfm(toks_country)
docvars(dfm_country) <- docvars(corp)
docvars(dfm_country, "year") <- as.character(docvars(dfm_country, "year"))

# make dictionary ngram-compatible (spaces -> "_")
dict_countries_ngram <- dictionary(
  lapply(dict_countries, function(v) gsub(" ", "_", v, fixed = TRUE))
)

# lookup: docs x countries
dfm_countries <- dfm_lookup(dfm_country, dictionary = dict_countries_ngram)

X <- as.matrix(dfm_countries)  # rows = docs, cols = countries

top5_per_doc <- apply(X, 1, function(row) {
  if (all(row == 0)) return(character(0))           # no countries in that doc
  o <- order(row, decreasing = TRUE)
  o <- o[row[o] > 0]                                 # keep positive counts only
  colnames(X)[head(o, 5)]
})

countries_top5_ever <- sort(unique(unlist(top5_per_doc)))
countries_top5_ever
length(countries_top5_ever)

data.frame(doc_id = rownames(X), top5 = sapply(top5_per_doc, paste, collapse = ", "))

annual_countries <- annual_shares(dfm_country, dict_countries_ngram, year_var = "year")

prop_cols <- paste0("prop_", countries_top5_ever)

# keep only needed columns
annual_top5 <- annual_countries[, c("year", prop_cols, "all_tokens"), drop = FALSE]

plot_countries <- function(annual, keys_used,
                           title = "Countries ever in a document-level Top 5",
                           legend_cex = 0.7) {
  
  years <- as.integer(annual$year)
  prop_cols <- paste0("prop_", keys_used)
  
  ylim <- range(annual[, prop_cols, drop = FALSE], na.rm = TRUE)
  cols <- rep_len(RColorBrewer::brewer.pal(8, "Dark2"), length(prop_cols))
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mar = c(4, 4, 3, 8), xpd = NA)
  
  plot(years, annual[[prop_cols[1]]], type = "l", lwd = 2, col = cols[1],
       xlab = "Year", ylab = "Proportion of tokens", main = title, ylim = ylim)
  
  if (length(prop_cols) > 1) {
    for (i in 2:length(prop_cols)) {
      lines(years, annual[[prop_cols[i]]], col = cols[i], lwd = 2)
    }
  }
  
  legend("topright", inset = c(-0.22, 0),
         legend = keys_used, col = cols, lwd = 2, bty = "n", cex = legend_cex)
}

plot_countries(annual_top5, countries_top5_ever)

data.frame(doc_id = rownames(X), top5 = sapply(top5_per_doc, paste, collapse = ", "))

pdf("CBB_countries_ever_top5.pdf", width = 9, height = 5)
plot_countries(annual_top5, countries_top5_ever)
dev.off()
