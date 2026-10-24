
# build_data.R
# EC560 Individual Project — BLP Airline Demand Data Construction
# Data: BTS Origin & Destination Survey (DB1B), Q1 2025

# Run Logging 
log_file <- paste0("output/log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
sink(log_file, split = TRUE)   # saves console output to log file
cat("Run started:", format(Sys.time()), "\n")
cat("R version:", R.version$version.string, "\n\n")

# ── 2. Working Directory 
setwd("C:/Users/dibba/OneDrive/Desktop/University of Oregon/2nd Year/Theory of IO/EC560-Individual-Project")

# ── 3. Packages 
# install.packages(c("tidyverse", "data.table"))
library(tidyverse)
library(data.table)

# ── 4. File Paths 
raw_path  <- "data/raw/"
proc_path <- "data/processed/"

# ── 5. Load Raw Data 
cat("═══ Loading Raw Data ═══\n")
market <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BMarket_2025_1.csv"))
coupon <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BCoupon_2025_1.csv"))
ticket <- fread(paste0(raw_path, "Origin_and_Destination_Survey_DB1BTicket_2025_1.csv"))
cat("All three files loaded successfully.\n\n")

# ── 6. Inspect Raw Data 
cat("═══ Raw Data Inspection ═══\n")

# Print ALL column names first — verify these match what the code uses below
cat("\nMarket columns:\n");  print(names(market))
cat("\nCoupon columns:\n");  print(names(coupon))
cat("\nTicket columns:\n");  print(names(ticket))

# Structure overview
cat("\nMarket:\n");  glimpse(market)
cat("\nCoupon:\n");  glimpse(coupon)
cat("\nTicket:\n");  glimpse(ticket)

# Dimensions
cat("\nDimensions — Market:", dim(market), "\n")
cat("Dimensions — Coupon:", dim(coupon), "\n")
cat("Dimensions — Ticket:", dim(ticket), "\n")

# Missing values
cat("\nMissing values — Market:\n");  print(colSums(is.na(market)))
cat("\nMissing values — Coupon:\n");  print(colSums(is.na(coupon)))
cat("\nMissing values — Ticket:\n");  print(colSums(is.na(ticket)))

# ---- 7. Data Cleaning & Sample Restrictions --------------------------------

cat("=== SECTION 7: Data Cleaning ===\n")

# Check GeoType distribution before filtering
cat("MktGeoType distribution:\n")
print(table(market$MktGeoType))
cat("\nOriginCountry distribution (top 5):\n")
print(sort(table(market$OriginCountry), decreasing = TRUE)[1:5])
cat("\nMktCoupons distribution:\n")
print(table(market$MktCoupons))

market_clean <- market %>%
  filter(
    OriginCountry == "US",   # Domestic origin only
    DestCountry   == "US",   # Domestic destination only
    BulkFare      == 0,      # Exclude bulk/charter fares (not individually priced)
    MktCoupons    <= 2,      # Nonstop (1) or single-connection (2) itineraries only
    MktFare       >= 25,     # Floor: remove suspiciously cheap fares / data errors
    MktFare       <= 2500,   # Ceiling: remove outlier ultra-premium / data errors
    Passengers    >= 1       # Must carry at least one passenger
  ) %>%
  select(-V42)               # Drop phantom empty column

cat("\nRows BEFORE cleaning:", format(nrow(market),       big.mark = ","), "\n")
cat("Rows AFTER cleaning: ", format(nrow(market_clean),  big.mark = ","), "\n")
cat("Rows removed:        ", format(nrow(market) - nrow(market_clean), big.mark = ","), "\n\n")


# SECTION 8: Market & Product Definition

cat("=== SECTION 8: Market & Product Definition ===\n\n")

# --- 8a. TkCarrier vs. OpCarrier ---
# MOTIVATION: TkCarrier is the brand printed on the consumer's ticket — the
# carrier they consciously chose. OpCarrier is often a regional subcontractor
# (e.g., SkyWest operating as "Delta Connection") invisible to the consumer
# at the point of purchase. BLP's differentiated-product Bertrand model
# requires products to be the brands consumers perceive and choose between.
# DB1B README: TkCarrier = "99" for interline itineraries (multiple ticketing
# carriers) — "99" is a BTS placeholder, NOT a consumer-facing brand.

cat("--- TkCarrier distribution (top 20) ---\n")
print(sort(table(market_clean$TkCarrier), decreasing = TRUE)[1:20])
cat("\n--- OpCarrier distribution (top 20) ---\n")
print(sort(table(market_clean$OpCarrier), decreasing = TRUE)[1:20])

# Aggregate to product level: TkCarrier × directional city-market pair
product_df <- market_clean %>%
  group_by(
    origin_mkt_id = OriginCityMarketID,
    dest_mkt_id   = DestCityMarketID,
    carrier       = TkCarrier            # <-- TkCarrier: consumer-facing brand
  ) %>%
  summarise(
    passengers   = sum(Passengers,                    na.rm = TRUE),
    price        = weighted.mean(MktFare, Passengers, na.rm = TRUE),
    distance     = mean(NonStopMiles,                 na.rm = TRUE),
    frac_nonstop = mean(MktCoupons == 1,              na.rm = TRUE),
    .groups      = "drop"
  )

cat("\n--- After TkCarrier aggregation ---\n")
cat("Products (carrier × route):", format(nrow(product_df),                           big.mark=","), "\n")
cat("Unique markets:            ", format(n_distinct(product_df$origin_mkt_id,
                                                     product_df$dest_mkt_id),          big.mark=","), "\n")
cat("Unique carriers:           ", n_distinct(product_df$carrier), "\n")
cat("Carriers present:          ", paste(sort(unique(product_df$carrier)), collapse=", "), "\n\n")

# --- 8b. Carrier Whitelist ---
#
# THREE CATEGORIES EXCLUDED:
#
# (1) "99" — BTS interline placeholder (not a brand)
#     When a consumer uses two separate carriers (e.g., AA SEA→ORD, then
#     UA ORD→LAX on different tickets), DB1B assigns TkCarrier = "99".
#     The consumer chose TWO carriers; treating "99" as a product misdefines
#     the choice set entirely.
#
# (2) Foreign carriers: AC, AM, AY, BA, CX, ET, FJ, QF, QR, RJ, SQ, TP, VC
#     DB1B samples the domestic SEGMENT of international itineraries. A
#     passenger flying Paris→JFK→LAX on a BA-issued BA/AA codeshare ticket
#     generates a domestic JFK→LAX record with TkCarrier = BA. That consumer
#     was NOT choosing in the US domestic market — they bought an international
#     ticket. Including BA in JFK→LAX inflates competition incorrectly.
#     Our OriginCountry/DestCountry == "US" filter removes international
#     routes but NOT foreign-carrier-coded domestic segments of intl trips.
#
# (3) Fringe / unidentified carriers (3M, 9K, 9X, KG, LF, PR, MX)
#     Carriers with negligible market presence lack independent pricing
#     power. Their demand is absorbed into the outside good (Berry 1994).
#     MX (Mexicana, relaunched 2024 as Mexican state carrier): despite the
#     US/US country filter, MX appears via transborder codeshare accounting.
#     No confirmed independent US domestic scheduled service → excluded.

us_domestic_carriers <- c(
  "AA",  # American Airlines       — Big 3 legacy network carrier
  "DL",  # Delta Air Lines         — Big 3 legacy network carrier
  "UA",  # United Airlines         — Big 3 legacy network carrier
  "WN",  # Southwest Airlines      — National LCC (point-to-point model)
  "AS",  # Alaska Airlines         — National network/LCC
  "B6",  # JetBlue Airways         — National LCC
  "F9",  # Frontier Airlines       — National ULCC
  "G4",  # Allegiant Air           — National ULCC (leisure-focused)
  "HA",  # Hawaiian Airlines       — Hawaii/Pacific specialist (indep. brand Q1 2025)
  "NK",  # Spirit Airlines         — National ULCC (operating Q1 2025 under Ch. 11)
  "SY",  # Sun Country Airlines    — Hybrid LCC/charter
  "XP"   # Avelo Airlines          — Regional LCC (founded 2021)
)

all_carriers_in_data <- sort(unique(product_df$carrier))
carriers_kept        <- intersect(all_carriers_in_data, us_domestic_carriers)
carriers_excluded    <- setdiff(all_carriers_in_data,   us_domestic_carriers)

cat("--- 8b. Carrier Whitelist ---\n")
cat("KEPT     (n=", length(carriers_kept),     "):",
    paste(sort(carriers_kept),     collapse=", "), "\n")
cat("EXCLUDED (n=", length(carriers_excluded), "):",
    paste(sort(carriers_excluded), collapse=", "), "\n\n")

excl_share <- product_df %>%
  summarise(
    excl_pax = sum(passengers[carrier %in% carriers_excluded]),
    tot_pax  = sum(passengers)
  ) %>% mutate(pct = round(excl_pax / tot_pax * 100, 2))
cat("Excluded carriers account for", excl_share$pct,
    "% of sampled passengers\n\n")

product_df <- product_df %>% filter(carrier %in% us_domestic_carriers)

cat("After whitelist:\n")
cat("  Products:", format(nrow(product_df), big.mark=","), "\n")
cat("  Markets: ", format(n_distinct(product_df$origin_mkt_id,
                                     product_df$dest_mkt_id), big.mark=","), "\n")
cat("  Carriers:", n_distinct(product_df$carrier), "\n\n")



# SECTION 9: Market Size & Share Construction

cat("=== SECTION 9: Market Size & Share Construction ===\n\n")

# MOTIVATION (Lecture 4, Berry 1994):
# BLP requires outside good share s_0m = 1 - Σ_j s_jm for each market m.
# u_{i0} = 0 + ε_{i0}: outside option normalized so e^{ū_0} = 1.
# "Need to assume market size for the denominator." (Lecture 4 slide 22)
#
# DB1B = 10% sample → expand by 10x for actual passenger counts.
# Market size = actual_pax × MULTIPLIER (Peters 2006, Ciliberto & Tamer 2009).
# Multiplier = 3 → inside goods capture 1/3 of potential market,
# outside share s_0 = 2/3 — consistent with low route-level penetration rates.
#
# Note: under a constant multiplier, s_0 is identical across markets (= 2/3).
# log(s_0) is therefore absorbed by the intercept or market fixed effects in
# the IV regression. Lecture 5 (slide 25) confirms: "logit with market fixed
# effects is equivalent to the HW (CES) log-quantity regression.

SAMPLE_EXPANSION  <- 10   # DB1B = 10% systematic sample
MARKET_MULTIPLIER <-  3   # outside good scaling

# Step 1: Market-level totals
market_totals <- product_df %>%
  group_by(origin_mkt_id, dest_mkt_id) %>%
  summarise(
    total_pax_sampled = sum(passengers),
    n_products        = n(),
    .groups = "drop"
  ) %>%
  mutate(
    total_pax_actual = total_pax_sampled * SAMPLE_EXPANSION,
    market_size      = total_pax_actual  * MARKET_MULTIPLIER
  )

cat("Market size summary (actual pax × multiplier):\n")
print(summary(market_totals$market_size))
cat("Single-product markets:", sum(market_totals$n_products == 1),
    "(no within-market price variation for IV — will drop in thin-market filter)\n\n")

# Step 2: Merge market totals and compute all share variables in one step
# FIX vs. earlier version: total_pax_actual is now joined BEFORE being used
product_df <- product_df %>%
  left_join(
    market_totals %>% select(origin_mkt_id, dest_mkt_id,
                             total_pax_actual, market_size, n_products),
    by = c("origin_mkt_id", "dest_mkt_id")
  ) %>%
  mutate(
    actual_pax    = passengers    * SAMPLE_EXPANSION,
    share         = actual_pax    / market_size,               # s_jm
    outside_share = 1 - (total_pax_actual / market_size)       # s_0m = 1 - 1/MULTIPLIER
  )

cat("Share summary (s_jm):\n")
print(summary(product_df$share))
cat("\nOutside share (s_0m) — constant under simple multiplier:\n")
cat("  s_0 =", unique(round(product_df$outside_share, 6)), "\n")
cat("  (= 1 - 1/", MARKET_MULTIPLIER, "= ", 1 - 1/MARKET_MULTIPLIER, ")\n\n")

# Sanity check: outside_share > 0 everywhere
neg_outside <- sum(product_df$outside_share <= 0)
cat("Products with outside_share <= 0:", neg_outside,
    ifelse(neg_outside == 0, "✓ ALL POSITIVE\n\n", "⚠ INCREASE MULTIPLIER\n\n"))

# Step 3: Berry (1994) logit LHS — the dependent variable for IV regression
# Lecture 4: "ln s_j - ln s_0 = δ_j = x_j β - α p_j"
# This is what we regress on price and characteristics to recover α and β.
product_df <- product_df %>%
  mutate(
    log_share   = log(share),
    log_outside = log(outside_share),
    y_logit     = log_share - log_outside   # = log(s_j) - log(2/3) under multiplier=3
  )

cat("Logit LHS (log s_j - log s_0) summary:\n")
print(summary(product_df$y_logit))
cat("\nFor pyBLP: use 'share' column directly (pyBLP inverts shares internally).\n")
cat("For diagnostic IV regression: use 'y_logit' as dependent variable.\n\n")


# SECTION 10: Minimum Market Size Filter

cat("=== SECTION 10: Minimum Market Size Filter ===\n\n")

# MOTIVATION (Lecture 3, Consumer slides):
# "Market demand function is an average over each consumer's demand function."
# Thin markets (< 90 sampled passengers ≈ < 900 actual travelers/quarter)
# produce unreliable passenger-weighted average fares — the "price" variable
# central to demand estimation — and extremely noisy shares.
# Threshold follows Peters (2006) for DB1B data.
# Single-carrier markets also provide no within-market price variation for IVs.

MIN_PAX <- 90

before_thin <- nrow(product_df)
markets_before <- n_distinct(product_df$origin_mkt_id, product_df$dest_mkt_id)

product_df <- product_df %>%
  group_by(origin_mkt_id, dest_mkt_id) %>%
  filter(sum(passengers) >= MIN_PAX, n() > 1) %>%
  ungroup()

after_thin   <- nrow(product_df)
markets_after <- n_distinct(product_df$origin_mkt_id, product_df$dest_mkt_id)

cat("Min sampled passengers per market:", MIN_PAX, "(≈", MIN_PAX*10, "actual)\n")
cat("Monopoly markets also dropped (n() == 1): YES\n") 
cat("Products BEFORE thin-market drop:", format(before_thin,    big.mark=","), "\n")
cat("Products AFTER  thin-market drop:", format(after_thin,     big.mark=","), "\n")
cat("Products dropped:                 ", format(before_thin - after_thin, big.mark=","), "\n")
cat("Markets BEFORE:", format(markets_before, big.mark=","), "\n")
cat("Markets AFTER: ", format(markets_after,  big.mark=","), "\n\n")


# SECTION 11: Instrumental Variables

cat("=== SECTION 11: Instrumental Variables ===\n\n")

# MOTIVATION (Lecture 3 & 4):
# Lecture 3: "We still have an omitted variable bias problem → IV regression"
# "Price might be higher because of unobserved quality (higher demand) →
#  Omitted variable bias. Coefficient on price captures the effect of price
#  AND unobserved quality."
#
# Lecture 4: "Later on: ξ_j is a product demand shock (shared by all consumers)"
# Carriers observe ξ_j (unobserved quality/brand appeal) before setting fares.
# OLS on price would conflate higher fares with higher demand → upward bias
# in the price coefficient (biased toward zero in magnitude; demand appears
# less elastic than it truly is).
#
# THREE INSTRUMENTS:
#
# IV1 — BLP "rival characteristics" (Berry, Levinsohn & Pakes 1995):
#   Sum of rivals' log distance on the same route.
#   Relevance:  fuel cost scales with distance → rivals' costs shift rivals'
#               prices → feed into carrier j's Bertrand best-response.
#               (Lecture 2: "Best response rises by 1/(2b) for each $1 increase
#               in rival's price")
#   Exclusion:  physical geography (miles) is determined by city locations,
#               not by demand shocks for a specific carrier on that route.
#
# IV2 — Hausman cost shift (Hausman, Leonard & Zona 1994; Nevo 2001):
#   Carrier j's average fare across all OTHER routes (same carrier, other mkts).
#   Relevance:  system-wide cost shocks (jet fuel, labor contracts) raise fares
#               on ALL routes uniformly.
#   Exclusion:  route-specific ξ_j is uncorrelated with demand shocks
#               on other routes (no common demand component assumption).
#
# IV3 — Southwest presence dummy (Goolsbee & Syverson 2008):
#   = 1 if WN operates on market m AND carrier j ≠ WN.
#   Relevance:  the well-documented "Southwest Effect" depresses rivals' prices
#               by 17-26% (consistent with Bertrand best-responses from Lecture 2).
#   Exclusion:  WN's entry driven by gate availability and fleet constraints,
#               not by route-specific demand shocks for legacy carriers.

# --- IV1: BLP rival log-distance ---
rival_iv <- product_df %>%
  group_by(origin_mkt_id, dest_mkt_id) %>%
  mutate(
    sum_log_dist_all   = sum(log(distance + 1)),    # Σ_k log(dist_k + 1))
    iv_blp_rival_logdist = sum_log_dist_all - log(distance + 1)  # subtract own
  ) %>%
  ungroup() %>%
  select(origin_mkt_id, dest_mkt_id, carrier, iv_blp_rival_logdist)

product_df <- product_df %>%
  left_join(rival_iv, by = c("origin_mkt_id", "dest_mkt_id", "carrier"))

cat("IV1 (BLP rival log-distance) summary:\n")
print(summary(product_df$iv_blp_rival_logdist))

# --- IV2: Hausman cost IV ---
hausman_iv <- product_df %>%
  group_by(carrier) %>%
  mutate(
    iv_hausman = (sum(price) - price) / pmax(n() - 1, 1)
    # average fare of carrier j on all routes EXCEPT the current one
  ) %>%
  ungroup() %>%
  select(origin_mkt_id, dest_mkt_id, carrier, iv_hausman)

product_df <- product_df %>%
  left_join(hausman_iv, by = c("origin_mkt_id", "dest_mkt_id", "carrier"))

cat("\nIV2 (Hausman carrier cost IV) summary:\n")
print(summary(product_df$iv_hausman))

# --- IV3: Southwest presence dummy ---
sw_markets <- product_df %>%
  filter(carrier == "WN") %>%
  distinct(origin_mkt_id, dest_mkt_id) %>%
  mutate(wn_present = 1L)

product_df <- product_df %>%
  left_join(sw_markets, by = c("origin_mkt_id", "dest_mkt_id")) %>%
  mutate(
    wn_present  = replace_na(wn_present, 0L),
    iv_sw_rival = as.integer(wn_present == 1L & carrier != "WN")
  ) %>%
  select(-wn_present)

cat("\nIV3 (Southwest rival dummy) distribution:\n")
print(table(product_df$iv_sw_rival))
cat("  (", round(mean(product_df$iv_sw_rival)*100, 1),
    "% of non-WN carrier-routes face Southwest competition)\n\n")


# SECTION 12: Final Dataset Assembly & Save

cat("=== SECTION 12: Final Dataset Assembly ===\n\n")

blp_data <- product_df %>%
  mutate(
    market_id  = paste0(origin_mkt_id, "_", dest_mkt_id),
    log_price  = log(price),
    log_dist   = log(distance + 1)
  ) %>%
  select(
    # Identifiers
    market_id, origin_mkt_id, dest_mkt_id, carrier,
    # Demand LHS (both forms: pyBLP uses 'share'; IV regression uses 'y_logit')
    share, outside_share, y_logit,
    # Price (endogenous RHS)
    price, log_price,
    # Product characteristics X_j (Lecture 4: utility = β X_j - α p_j)
    distance, log_dist, frac_nonstop,
    # Quantities
    passengers, actual_pax, market_size, total_pax_actual,
    # Instrumental variables
    iv_blp_rival_logdist, iv_hausman, iv_sw_rival,
    # Diagnostics
    n_products
  )

cat("Final BLP dataset dimensions:", dim(blp_data), "\n")
cat("Variables:", paste(names(blp_data), collapse = ", "), "\n\n")

cat("Summary of key variables:\n")
print(summary(blp_data %>% select(share, price, distance, frac_nonstop,
                                  iv_blp_rival_logdist, iv_hausman, iv_sw_rival)))

# Products-per-market distribution (important for IV1 and logit identification)
cat("\nProducts per market distribution:\n")
n_prod_tbl <- table(blp_data$n_products)
print(n_prod_tbl)
cat("Avg products/market:", 
    round(nrow(blp_data) / n_distinct(blp_data$market_id), 2), "\n")

# Save
write_csv(blp_data, paste0(proc_path, "blp_airline_data.csv"))
cat("\n Saved: data/processed/blp_airline_data.csv\n\n")


# SECTION 13: Final Verification & Log Close

cat("\n=== SECTION 13: Final Verification ===\n\n")

cat("\n")
cat("  FINAL BLP DATASET — SUMMARY\n")
cat("\n")
cat("  Markets (origin × dest):  ",
    format(n_distinct(blp_data$market_id), big.mark=","), "\n")
cat("  Products (carrier×route): ",
    format(nrow(blp_data), big.mark=","), "\n")
cat("  Carriers:                 ",
    n_distinct(blp_data$carrier), "\n")
cat("  Avg products/market:      ",
    round(nrow(blp_data) / n_distinct(blp_data$market_id), 2), "\n")
cat("  Price range:             $",
    round(min(blp_data$price), 2), "—$",
    round(max(blp_data$price), 2), "\n")
cat("  Share range:              ",
    round(min(blp_data$share), 6), "—",
    round(max(blp_data$share), 4), "\n")
cat("  Outside share (all mkts): ",
    round(unique(blp_data$outside_share), 4),
    "(= 1 - 1/", MARKET_MULTIPLIER, ")\n")
cat("  y_logit range:            ",
    round(min(blp_data$y_logit), 2), "—",
    round(max(blp_data$y_logit), 2), "\n")
cat("\n\n")

# Carrier-level summary
cat("Carrier summary:\n")
carrier_summary <- blp_data %>%
  group_by(carrier) %>%
  summarise(
    n_routes   = n(),
    avg_price  = round(weighted.mean(price, passengers), 2),
    avg_share  = round(mean(share), 5),
    pct_nonstop = round(mean(frac_nonstop) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n_routes))
print(carrier_summary)

cat("\nRun completed:", format(Sys.time()), "\n")
sink()  # close log

