#dfa(3).zip downloaded on 2026-04-11

# read csv
data <- read.csv("dfa-networth-levels.csv", header = TRUE, stringsAsFactors = FALSE)

# show column names
colnames(data)

tail(data, 20) # Up to 2025:Q4

# convert "0000:Q0" to date format
data$Date <- as.Date(paste0(substr(data$Date, 1, 4), "-", 
                            as.numeric(substr(data$Date, 7, 7)) * 3 - 2, "-01"))

# ggplot "Corporate.equities.and.mutual.fund.shares"
library(ggplot2)
ggplot(data, aes(x = Date, y = Corporate.equities.and.mutual.fund.shares, color = Category)) +
    geom_line() +
    facet_wrap(~Category) +
    labs(title = "Corporate Equities and Mutual Fund Shares Over Time",
             x = "Date",
             y = "Value") +
    theme_minimal()

# label "Corporate.equities.and.mutual.fund.shares" as "L"
data$L <- data$Corporate.equities.and.mutual.fund.shares

# label "Assets" as "A"
data$A <- data$Assets

# create equity holdings share of total assets
data$S <- data$L / data$A

# ggplot "S"
ggplot(data, aes(x = Date, y = S, color = Category)) +
    geom_line() +
    facet_wrap(~Category) +
    labs(title = "Equity Holdings Share of Total Assets Over Time",
             x = "Date",
             y = "Share of Total Assets") +
    theme_minimal()

# summary statistics for "L" and "S" by category
library(tidyverse)
data %>%
    group_by(Category) %>%
    summarise(mean_L = mean(L, na.rm = TRUE),
              sd_L = sd(L, na.rm = TRUE),
              mean_S = mean(S, na.rm = TRUE),
              sd_S = sd(S, na.rm = TRUE))



# drop columns except "Date", "Category", "L", "S", "A", "Real.estate", "Private.businesses", "Liabilities"
data <- data %>%
    dplyr::select(Date, Category, L, S, A, Real.estate, Private.businesses, Liabilities)


# read csv
datadetail <- read.csv("dfa-networth-levels-detail.csv", header = TRUE, stringsAsFactors = FALSE)

colnames(datadetail)

tail(datadetail, 20) # Up to 2025:Q4

# convert "0000:Q0" to date format
datadetail$Date <- as.Date(paste0(substr(datadetail$Date, 1, 4), "-", 
                            as.numeric(substr(datadetail$Date, 7, 7)) * 3 - 2, "-01"))

# create a new column "C" as a sum of "Deposits" and "Money.market.fund.shares"
datadetail$C <- datadetail$Deposits + datadetail$Money.market.fund.shares

# create a new column "Fixed.income" as a sum of "Debt.securities", "U.S..government.and.municipal.securities", "Corporate.and.foreign.bonds", "Loans..Assets.", "Other.loans.and.advances..Assets.", and "Mortgages"
datadetail$Fixed.income <- datadetail$Debt.securities + datadetail$U.S..government.and.municipal.securities + datadetail$Corporate.and.foreign.bonds + datadetail$Loans..Assets. + datadetail$Other.loans.and.advances..Assets. + datadetail$Mortgages

# keep only "Date", "Category", "C", "Fixed.income"
datadetail <- datadetail %>%
    dplyr::select(Date, Category, C, Fixed.income)

# merge data and datadetail by "Date" and "Category"
df <- merge(data, datadetail, by = c("Date", "Category"))

# ggplot any variable by category
ggplot(df, aes(x = Date, y = Private.businesses, color = Category)) +
  geom_line() +
  facet_wrap(~Category) +
  labs(title = "Cash and Cash Equivalents Over Time",
       x = "Date",
       y = "Value") +
  theme_minimal()



# F-F_Research_Data_Factors_CSV (2).zip downloaded on 2026-04-11
# read csv
ff <- read.csv("F-F_Research_Data_Factors.csv", header = TRUE, stringsAsFactors = FALSE)

# convert "192607" to date format
ff$Date <- as.Date(paste0(as.numeric(substr(ff$X, 1, 4)), "-", 
                            as.numeric(substr(ff$X, 5, 6)), "-01"))
# convert monthly returns to quarterly returns
ff <- ff %>%
    group_by(quarter = as.yearqtr(Date)) %>%
    summarise(Mkt_RF = prod(1 + Mkt.RF / 100) - 1,
              SMB = prod(1 + SMB / 100) - 1,
              HML = prod(1 + HML / 100) - 1,
              RF = prod(1 + RF / 100) - 1)

# convert quarter to date format
ff$Date <- as.Date(as.yearqtr(ff$quarter))

# keep only "Date" and "Mkt_RF"
ff <- ff %>%
    dplyr::select(Date, Mkt_RF)

# merge df and ff by "Date"
df <- merge(df, ff, by = "Date")

# save df as csv
# write.csv(df, "df.csv", row.names = FALSE)

# ----------------------------------------------------------------------------

library(tidyverse)

df <- read.csv("df.csv", header = TRUE, stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))

head(df)

# add top 1% category that sums up TopPt1 and RemainingTop1, keeping MktRF the same and S recomputed as L/A
df <- bind_rows(
    df,
    df %>%
        filter(Category %in% c("TopPt1", "RemainingTop1")) %>%
        group_by(Date) %>%
        summarise(
            L = sum(L),
            A = sum(A),
            Real.estate = sum(Real.estate),
            Private.businesses = sum(Private.businesses),
            Liabilities = sum(Liabilities),
            C = sum(C),
            Fixed.income = sum(Fixed.income),
            Mkt_RF = first(Mkt_RF),
            .groups = "drop"
        ) %>%
        mutate(
            Category = "Top1",
            S = L / A
        )
)

# group-specific valuation shocks --------------------------------------------

# compute log difference of L and its lag by category
df <- df |>
    group_by(Category) |>
    arrange(Date) |>
    mutate(
        dlnL = c(NA, diff(log(L))),
        dlnL_lag = dplyr::lag(dlnL)
        ) |>
    ungroup()


# Estimate local (global) shocks
estimate_shocks <- function(df) {
  df2 <- df |> filter(!is.na(dlnL), !is.na(dlnL_lag))
  # local
  m <- lm(dlnL ~ dlnL_lag + Mkt_RF, data = df2)
  # global
  # m <- lm(dlnL ~ dlnL_lag, data = df2)
  df2$shock <- resid(m)
  df2
}

shock_df <- df |>
    group_split(Category) |>
    map_dfr(estimate_shocks)

# rebalancing decomposition --------------------------------------------------

compute_active_rebalancing <- function(df, h) {
    # Ensure observations are in time order before using lead/future windows
    df <- df %>% arrange(Date)
    n_obs <- nrow(df)

    # For each time i, compute cumulative gross market return from i+1 to i+h:
    #   Π_{j=1..h} (1 + r_{i+j})
    # If horizon exceeds sample end, return NA.
    future_growth <- vapply(seq_len(n_obs), function(i) {
        if (i + h > n_obs) {
            NA_real_
        } else {
            prod(1 + df$Mkt_RF[(i + 1):(i + h)])
        }
    }, numeric(1))

    df %>%
        mutate(
            # Counterfactual (passive) equity level at horizon h:
            # current equity grown only by market returns
            PL_h = L * future_growth,

            # Counterfactual passive equity share at horizon h:
            # passive equity divided by realized future total assets
            PS_h = PL_h / dplyr::lead(A, h),

            # Active rebalancing measure:
            # realized future share minus passive future share
            AS_h = dplyr::lead(S, h) - PS_h,

            # raw share at horizon h (for sensitivity check):
            S_h = dplyr::lead(S, h)
        )
}

# core: local projections ----------------------------------------------------
# Estimate local-projection regression for a single group at horizon h
estimate_lp_h <- function(df, h) {
    # Build horizon-h active rebalancing variable and keep complete cases
    df_h <- compute_active_rebalancing(df, h) %>%
        filter(!is.na(.data$AS_h), !is.na(.data$shock))

    # Regress raw share without controls
    # m <- lm(S_h ~ shock + dplyr::lag(S_h), data = df_h)

    # Omit the lag not to absorb the shock effect
    # m <- lm(S_h ~ shock, data = df_h)

    # Regress raw share on shock + controls
    # m <- lm(S_h ~ shock + dplyr::lag(S_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

    # Regress active share on shock + controls
    # m <- lm(AS_h ~ shock + dplyr::lag(AS_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

    # Return tidy coefficients with metadata for horizon and group
    broom::tidy(m) %>%
        mutate(h = h, group = unique(df$Category))
}


# Horizons (quarters) for impulse-response estimation
# h = 0 should be excluded as AS_0 = 0 by construction (no future rebalancing at time of shock)
horizons <- 1:8


# Run horizon-by-horizon local-projection regressions for each wealth group
# For each group, estimate impulse responses at horizons 1-8 quarters
# Results combined into single dataframe with horizon (h) and group metadata
lp_results_all <- shock_df |>
    group_split(Category) |>
    map_dfr(function(df_group) {
        map_dfr(horizons, ~ estimate_lp_h(df_group, .x))
    })

# Keep only the shock coefficient to form IRF table
irf <- lp_results_all |>
    filter(term == "shock") |>
    dplyr::select(group, h, estimate, std.error, p.value)

library(ggplot2)

ggplot(irf, aes(x = h, y = estimate, ymin = estimate - 1.96*std.error,
                ymax = estimate + 1.96*std.error, color = group)) +
  geom_line(size = 1.1) +
  geom_ribbon(alpha = 0.15, color = NA) +
  facet_wrap(~group, scales = "free_y") +
  labs(title = "Local Projection IRFs: Active Rebalancing Response to Valuation Shocks",
       x = "Horizon (quarters)", y = "Response of Active Rebalancing") +
  theme_minimal(base_size = 14)

# Assymmetric responses -------------------------------------------------------

# see number of positive and negative shocks by group
shock_df %>%
    group_by(Category) %>%
    summarise(
        n_positive = sum(shock > 0, na.rm = TRUE),
        n_negative = sum(shock < 0, na.rm = TRUE)
    )

estimate_lp_h_asymmetric <- function(df, h) {
    df_h <- compute_active_rebalancing(df, h) %>%
        filter(!is.na(.data$AS_h), !is.na(.data$shock))

    # Create separate variables for positive and negative shocks
    df_h <- df_h %>%
        mutate(
            shock_pos = ifelse(shock > 0, shock, 0),
            shock_neg = ifelse(shock < 0, shock, 0)
        )

    # Regress active share on positive and negative shocks + controls
    m <- lm(AS_h ~ shock_pos + shock_neg + dplyr::lag(AS_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

    broom::tidy(m) %>%
        mutate(h = h, group = unique(df$Category))
}

lp_results_asymmetric <- shock_df |>
    group_split(Category) |>
    map_dfr(function(df_group) {
        map_dfr(horizons, ~ estimate_lp_h_asymmetric(df_group, .x))
    })

irf_asymmetric <- lp_results_asymmetric |>
    filter(term %in% c("shock_pos", "shock_neg")) |>
    dplyr::select(group, h, term, estimate, std.error, p.value) 

ggplot(irf_asymmetric, aes(x = h, y = estimate, color = term)) +
    geom_line(size = 1.1) +
    facet_wrap(~group, scales = "free_y") +
    labs(title = "Local Projection IRFs: Active Rebalancing Response to Positive vs Negative Valuation Shocks",
         x = "Horizon (quarters)", y = "Response of Active Rebalancing") +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank())

irf_asymmetric <- lp_results_asymmetric |>
    filter(term %in% c("shock_pos", "shock_neg"), 
           !group %in% c("RemainingTop1", "TopPt1")) |>
    # rearrange by next9, top1, bottom50, next40 in order
    mutate(group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40"))) %>%
    dplyr::select(group, h, term, estimate, std.error, p.value)

ggplot(irf_asymmetric, aes(x = h, y = estimate, color = term)) +
    geom_line(size = 1.1) +
    facet_wrap(~group, scales = "free_y") +
    labs(title = "Local Projection IRFs: Active Rebalancing Response to Positive vs Negative Valuation Shocks (Excluding Top 1% Subgroups)",
         x = "Horizon (quarters)", y = "Response of Active Rebalancing") +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank())
