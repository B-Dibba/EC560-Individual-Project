
# EC560 Individual Project — BLP Airline Demand Check point 2

log_file <- paste0("output/log_cp2_",
                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
sink(log_file, split = TRUE)
cat("Run started:", format(Sys.time()), "\n")
cat("R version:", R.version$version.string, "\n\n")

# ── 2. Working Directory 
setwd("C:/Users/dibba/OneDrive/Desktop/University of Oregon/2nd Year/Theory of IO/EC560-Individual-Project")

# install.packages("fixest")  # run once if needed
library(tidyverse)
library(fixest)

proc_path <- "data/processed/"
out_path  <- "output/"

# ── 2. Load Data 
cat("=== Loading blp_airline_data.csv ===\n")
blp <- read_csv(paste0(proc_path, "blp_airline_data.csv"), show_col_types = FALSE)

cat("Rows:", nrow(blp), "\n")
cat("Markets:", n_distinct(blp$market_id), "\n")
cat("Carriers:", n_distinct(blp$carrier),
    "—", paste(sort(unique(blp$carrier)), collapse = ", "), "\n\n")

# Quick variable check
cat("Variables present:\n")
print(names(blp))
cat("\n")

#Fixes: calibrate the market size to allow more flexibility in the
#   outside option share.

#Fix: scale market size by route distance — shorter routes have stronger
#   ground-travel competition (higher M → larger s_0)..
#
# M=5  → s_0 = 0.80  (routes < 500 mi: car/train is a real alternative)
# M=3  → s_0 = 0.67  (medium-haul baseline, same as CP1)
# M=2  → s_0 = 0.50  (long-haul > 1500 mi: flying near-captive market)

cat("=== Flexible Outside Option (addressing Prof comment) ===\n")

blp <- blp %>%
  mutate(
    mkt_multiplier    = case_when(
      distance < 500  ~ 5L,
      distance < 1500 ~ 3L,
      TRUE            ~ 2L
    ),
    market_size_flex   = total_pax_actual * mkt_multiplier,
    share_flex         = actual_pax / market_size_flex,
    outside_share_flex = 1 - total_pax_actual / market_size_flex,
    y_logit_flex       = log(share_flex) - log(outside_share_flex)
  )

# Sanity checks
cat("Any outside_share_flex <= 0?",
    sum(blp$outside_share_flex <= 0), "(should be 0)\n")
cat("Any share_flex >= 1?",
    sum(blp$share_flex >= 1), "(should be 0)\n\n")

cat("Multiplier distribution (by route obs):\n")
blp %>%
  count(mkt_multiplier) %>%
  mutate(outside_share = round(1 - 1/mkt_multiplier, 3),
         pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\nOutside share summary (flexible):\n")
print(summary(blp$outside_share_flex))
cat("Compared to CP1 constant value:", unique(round(blp$outside_share, 4)), "\n\n")

# ── 4. Price Construction Summary
# "How are you constructing price? What are the implications
#   of the underlying assumptions since consumers face different time-varying fares?"
# 
#   price_jm = Σ_i (MktFare_i × Passengers_i) / Σ_i Passengers_i
#   where i indexes individual DB1B records for carrier j in market m, Q1 2025.
# This is a PASSENGER-WEIGHTED AVERAGE across all fares in the quarter.
# Implication: we average over advance-purchase discounts and walk-up fares.
# This is standard in DB1B studies (Berry, Carnall & Spiller 2006).

cat("=== Price Construction Check ===\n")
cat("Price = passenger-weighted avg MktFare per carrier-route, Q1 2025.\n")
cat("Implication: averages over advance-purchase vs walk-up fare dispersion.\n")
cat("Within-market price variation (CV = sd/mean by market):\n")

within_mkt_price <- blp %>%
  group_by(market_id) %>%
  summarise(cv = sd(price) / mean(price), n = n(), .groups = "drop") %>%
  filter(n > 1)

cat("Mean CV of price across markets:", round(mean(within_mkt_price$cv), 4), "\n")
cat("Median CV:", round(median(within_mkt_price$cv), 4), "\n\n")

# ── 5.Within-Market Product Characteristic Variation ───
#  "Are there going to be any product characteristics besides
#   price that vary within a market?"
# 
# frac_nonstop varies by carrier within a market (some carriers
#   fly nonstop, others connect). Carrier fixed effects absorb brand-level
#   quality differences (network scope, loyalty programs, etc.).

cat("=== Within-Market Characteristic Variation ===\n")

within_mkt_ns <- blp %>%
  group_by(market_id) %>%
  summarise(sd_nonstop = sd(frac_nonstop), n = n(), .groups = "drop") %>%
  filter(n > 1)

cat("Markets with zero variation in frac_nonstop:",
    sum(within_mkt_ns$sd_nonstop == 0, na.rm = TRUE), "/",
    nrow(within_mkt_ns), "\n")
cat("Mean SD of frac_nonstop within market:",
    round(mean(within_mkt_ns$sd_nonstop, na.rm = TRUE), 4), "\n\n")
cat("Note: carrier dummies also provide within-market variation.\n\n")

# ── QUASI FIRST STAGE
# Assignment: "Include a quasi first stage showing that instruments are
#   relevant for prices conditional on product characteristics and
#   appropriate fixed effects."
#
# Dependent variable: log(price)
# Exogenous controls: log_dist, frac_nonstop
# Fixed effects: carrier (brand dummies absorb carrier-level quality)
# Excluded IVs: iv_blp_rival_logdist, iv_hausman, iv_sw_rival
# SE: clustered by market_id
#
# S F-statistic on excluded instruments:
#

cat("=== QUASI FIRST STAGE ===\n")
cat("Dependent variable: log(price)\n")
cat("Controls: log_dist, frac_nonstop\n")
cat("FE: carrier\n")
cat("SE: clustered by market_id\n\n")

# controls only (no IVs) ---
fs_base <- feols(log_price ~ log_dist + frac_nonstop | carrier,
                 data = blp,
                 cluster = ~market_id)

# --- Spec 2: add IV1 (BLP rival log-distance) ---
fs_iv1 <- feols(log_price ~ log_dist + frac_nonstop + iv_blp_rival_logdist | carrier,
                data = blp,
                cluster = ~market_id)

# --- Spec 3: add IV2 (Hausman) ---
fs_iv2 <- feols(log_price ~ log_dist + frac_nonstop + iv_hausman | carrier,
                data = blp,
                cluster = ~market_id)

# --- Spec 4: add IV3 (Southwest dummy) ---
fs_iv3 <- feols(log_price ~ log_dist + frac_nonstop + iv_sw_rival | carrier,
                data = blp,
                cluster = ~market_id)

# --- Spec 5: all three IVs + carrier FE (PRIMARY SPEC) ---
fs_all <- feols(log_price ~ log_dist + frac_nonstop +
                  iv_blp_rival_logdist + iv_hausman + iv_sw_rival | carrier,
                data = blp,
                cluster = ~market_id)

# --- Spec 6: carrier + market FE (IV1 absorbed; uses IV2 + IV3) ---
fs_mktfe <- feols(log_price ~ iv_hausman + iv_sw_rival | carrier + market_id,
                  data = blp,
                  cluster = ~market_id)

cat("--- Spec 5: All IVs + Carrier FE ---\n")
print(summary(fs_all))

cat("\n--- Spec 6: All FEs (IV2 + IV3 only; IV1 absorbed by market FE) ---\n")
print(summary(fs_mktfe))

# --- F-tests on excluded instruments ---
cat("\n--- Wald F-tests on excluded instruments (Spec 5) ---\n")

cat("\nIV1 (rival log-distance):\n")
print(wald(fs_all, "iv_blp_rival_logdist"))

cat("\nIV2 (Hausman cost shift):\n")
print(wald(fs_all, "iv_hausman"))

cat("\nIV3 (Southwest presence):\n")
print(wald(fs_all, "iv_sw_rival"))

cat("\nJoint Wald F — all three IVs:\n")
print(wald(fs_all, c("iv_blp_rival_logdist", "iv_hausman", "iv_sw_rival")))

cat("\nJoint Wald F — IV2 + IV3 only (Spec 6, with market FE):\n")
print(wald(fs_mktfe, c("iv_hausman", "iv_sw_rival")))

# --- Print first-stage table to file ---
cat("\n--- First Stage Table (saved to output/) ---\n")
etable(fs_base, fs_iv1, fs_iv2, fs_iv3, fs_all, fs_mktfe,
       headers = c("(1) Base", "(2) +IV1", "(3) +IV2",
                   "(4) +IV3", "(5) All IVs", "(6) Mkt FE"),
       title   = "Quasi First Stage: Dep. Var. = log(Price)",
       notes   = "Cluster-robust SE by market_id. Carrier FE in all specs. Spec (6) adds market FE; IV1 absorbed.",
       file    = paste0(out_path, "table1_first_stage.txt"),
       replace = TRUE)
cat("Saved: output/table1_first_stage.txt\n\n")

# ── 7. BERRY LOGIT DEMAND (2SLS) 
# Assignment: "Include a first-pass at estimating demand"
# (Note: assignment specifies pyBLP but we implement the equivalent Berry
#  (1994) pure logit via 2SLS in R, which is the same underlying model.)
#
# Model:  log(s_jm) - log(s_0m) = β1*log_dist + β2*frac_nonstop
#                                 + carrier FE + α*price + ξ_jm
#
# α < 0: demand slopes down in price.
# price is instrumented with IV1, IV2, IV3 (endogenous because carriers
# observe ξ_jm before setting fares → OLS biased toward zero).
#
# Using y_logit_flex (flexible outside share) as the dependent variable.
# fixest syntax: feols(y ~ exog | FE | endog ~ IVs)

cat("=== BERRY LOGIT DEMAND ESTIMATION (2SLS) ===\n\n")

# --- OLS baseline (biased — shown for comparison only) ---
ols_demand <- feols(y_logit_flex ~ price + log_dist + frac_nonstop | carrier,
                    data    = blp,
                    cluster = ~market_id)

# --- 2SLS: carrier FE + all 3 IVs ---
iv_demand <- feols(y_logit_flex ~ log_dist + frac_nonstop | carrier |
                     price ~ iv_blp_rival_logdist + iv_hausman + iv_sw_rival,
                   data    = blp,
                   cluster = ~market_id)

# --- 2SLS: carrier + market FE (IV2 + IV3 only; more demanding spec) ---
iv_demand_mktfe <- feols(y_logit_flex ~ 1 | carrier + market_id |
                           price ~ iv_hausman + iv_sw_rival,
                         data    = blp,
                         cluster = ~market_id)

cat("--- OLS Demand (endogeneity bias expected) ---\n")
print(summary(ols_demand))

cat("\n--- 2SLS Demand (carrier FE, all 3 IVs) --- PRIMARY SPEC ---\n")
print(summary(iv_demand))

cat("\n--- 2SLS Demand (carrier + market FE, IV2+IV3) ---\n")
print(summary(iv_demand_mktfe))

# --- First-stage F from 2SLS ---
cat("\nFirst-stage F-statistic from 2SLS (carrier FE spec):\n")
print(fitstat(iv_demand, "ivf"))

cat("\nFirst-stage F-statistic from 2SLS (market FE spec):\n")
print(fitstat(iv_demand_mktfe, "ivf"))

# --- Save demand table ---
etable(ols_demand, iv_demand, iv_demand_mktfe,
       headers = c("(1) OLS (biased)", "(2) 2SLS carrier FE",
                   "(3) 2SLS carrier+mkt FE"),
       title   = "Berry Logit Demand: Dep. Var. = log(s_j) - log(s_0)",
       notes   = paste("Cluster-robust SE by market_id.",
                       "Flexible outside share (distance-based multiplier).",
                       "Spec (3) uses IV2+IV3 only (IV1 absorbed by market FE)."),
       file    = paste0(out_path, "table2_demand.txt"),
       replace = TRUE)
cat("Saved: output/table2_demand.txt\n\n")

# ── 8. IMPLIED ELASTICITIES 
# Berry (1994) pure logit own-price elasticity:
#   ε_jj = α * p_j * (1 - s_j)
# where α is the price coefficient from the IV demand model.
# α < 0  →  ε_jj < 0 (downward-sloping demand, as required)
#
# Literature benchmarks for US domestic air travel:
#   Berry, Carnall & Spiller (2006): ε_jj ≈ -2.5 to -3.5
#   Ciliberto & Tamer (2009):        ε_jj ≈ -2.0 to -4.0

cat("=== IMPLIED ELASTICITIES ===\n\n")

# Extract price coefficient from primary 2SLS spec
# In fixest IV models, the instrumented coefficient is labeled "fit_price"
alpha_iv  <- coef(iv_demand)[["fit_price"]]
alpha_ols <- coef(ols_demand)[["price"]]

cat("Price coefficient — OLS (endogeneity bias):  ", round(alpha_ols, 6), "\n")
cat("Price coefficient — 2SLS (consistent):       ", round(alpha_iv,  6), "\n\n")

if (alpha_iv < 0) {
  cat("Sign check: PASS — demand slopes down in price.\n\n")
} else {
  cat("Sign check: FAIL — check specification.\n\n")
}

# Compute own-price elasticity for each observation
blp <- blp %>%
  mutate(
    elas_ols = alpha_ols * price * (1 - share_flex),
    elas_iv  = alpha_iv  * price * (1 - share_flex)
  )

cat("OLS own-price elasticity (biased):\n")
print(round(quantile(blp$elas_ols,
                     probs = c(0.10, 0.25, 0.50, 0.75, 0.90),
                     na.rm = TRUE), 4))

cat("\n2SLS own-price elasticity (consistent):\n")
print(round(quantile(blp$elas_iv,
                     probs = c(0.10, 0.25, 0.50, 0.75, 0.90),
                     na.rm = TRUE), 4))

cat("\nMean own-price elasticity (2SLS):",
    round(mean(blp$elas_iv, na.rm = TRUE), 4), "\n\n")

# Carrier-level average elasticity
cat("--- Carrier-level average elasticities (2SLS) ---\n")
carrier_elas <- blp %>%
  group_by(carrier) %>%
  summarise(
    n_routes  = n(),
    avg_price = round(mean(price), 2),
    avg_share = round(mean(share_flex), 5),
    mean_elas = round(mean(elas_iv, na.rm = TRUE), 4),
    .groups   = "drop"
  ) %>%
  arrange(mean_elas)
print(carrier_elas)

cat("\nLiterature benchmarks:\n")
cat("  Berry, Carnall & Spiller (2006): ε_jj ≈ -2.5 to -3.5\n")
cat("  Ciliberto & Tamer (2009):        ε_jj ≈ -2.0 to -4.0\n\n")

mean_e <- mean(blp$elas_iv, na.rm = TRUE)
if (mean_e < -1 & mean_e > -6) {
  cat("Assessment: REASONABLE — mean elasticity =",
      round(mean_e, 3), "falls within typical range.\n\n")
} else {
  cat("Assessment: OUTSIDE typical range — mean =",
      round(mean_e, 3), "— revisit specification.\n\n")
}

cat("IIA caveat: Pure logit cross-elasticities are proportional to\n")
cat("market share regardless of product similarity. This likely\n")
cat("understates substitution between close competitors (e.g., B6 vs NK).\n")
cat("Random coefficients (final project) will address this.\n\n")

# ── 9. MERGER ANALYSIS SETUP: B6 × NK 
# Assignment: "Detail how you will construct the counterfactual.
#   Motivate why you chose a particular merger."

# Chosen merger: JetBlue (B6) attempted acquisition of Spirit (NK).
# DOJ blocked January 2024; both still operating in Q1 2025 data.
# Motivation: documented price overlap, well-studied by DOJ economists,
# both B6 and NK are present in our carrier whitelist.

cat("=== MERGER OVERVIEW: JetBlue (B6) + Spirit (NK) ===\n\n")

b6_mkts  <- blp %>% filter(carrier == "B6") %>% pull(market_id) %>% unique()
nk_mkts  <- blp %>% filter(carrier == "NK") %>% pull(market_id) %>% unique()
overlap  <- intersect(b6_mkts, nk_mkts)

cat("B6 markets:", length(b6_mkts), "\n")
cat("NK markets:", length(nk_mkts), "\n")
cat("Overlapping markets (both carriers present):", length(overlap), "\n")
cat(sprintf("  = %.1f%% of B6 routes\n", length(overlap)/length(b6_mkts)*100))
cat(sprintf("  = %.1f%% of NK routes\n\n", length(overlap)/length(nk_mkts)*100))

overlap_summary <- blp %>%
  filter(market_id %in% overlap, carrier %in% c("B6", "NK")) %>%
  group_by(carrier) %>%
  summarise(
    avg_price = round(mean(price), 2),
    avg_share = round(mean(share_flex), 5),
    n_routes  = n(),
    .groups   = "drop"
  )
cat("Average fares in overlap markets:\n")
print(overlap_summary)

cat("\nNote: Full merger simulation (Bertrand FOC + ownership matrix update)\n")
cat("will be in the final project submission.\n\n")

# Close log
cat("Run completed:", format(Sys.time()), "\n")
sink()
