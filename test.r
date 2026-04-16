library(dplyr)
library(tidyr)
library(purrr)

set.seed(123)

# --- Wealth groups ---
groups <- c("b50", "p50_90", "p90_99", "p99_999", "p999_1000")

# --- Time index ---
n_periods <- 80
dates <- seq(as.Date("2000-01-01"), by = "quarter", length.out = n_periods)

# --- Simulate market returns ---
market_ret <- rnorm(n_periods, mean = 0.05, sd = 0.03)

# --- Simulate group-level balance sheet data ---
sim_data <- expand_grid(
    date = dates,
    group = groups
) %>%
    left_join(
        tibble(date = dates, market_ret = market_ret),
        by = "date"
    ) %>%
    mutate(
        # higher quantiles -> higher financial sophistication
        sophistication = case_when(
            group == "b50" ~ 1,
            group == "p50_90" ~ 2,
            group == "p90_99" ~ 3,
            group == "p99_999" ~ 4,
            TRUE ~ 5
        ),
        # equity levels differ by group
        L = exp(rnorm(
            n(),
            mean = case_when(
                group == "b50" ~ 8,
                group == "p50_90" ~ 9,
                group == "p90_99" ~ 10,
                group == "p99_999" ~ 11,
                TRUE ~ 12
            ),
            sd = pmax(0.03, 0.12 - 0.015 * sophistication)
        )),
        # more sophistication -> higher/more stable equity share
        S = pmin(
            pmax(
                0.10 + 0.04 * sophistication + 1.5 * market_ret +
                    rnorm(n(), 0, 0.03 / sophistication),
                0.05
            ),
            0.70
        ),
        # total assets implied by equity share
        A = L / S
    )

sim_data <- sim_data %>%
  group_by(group) %>%
  arrange(date) %>%
  mutate(
    dlnL = c(NA, diff(log(L))),
    dlnL_lag = dplyr::lag(dlnL)
  ) %>%
  ungroup()

# Function to estimate shocks for one group
estimate_shocks <- function(df) {
  df2 <- df %>% filter(!is.na(dlnL), !is.na(dlnL_lag))
  m <- lm(dlnL ~ dlnL_lag + market_ret, data = df2)
  df2$shock <- resid(m)
  df2
}

##########################################################
# Estimate shocks without market return control
estimate_shocks <- function(df) {
  df2 <- df %>% filter(!is.na(dlnL), !is.na(dlnL_lag))
  m <- lm(dlnL ~ dlnL_lag, data = df2)
  df2$shock <- resid(m)
  df2
}
##########################################################

shock_data <- sim_data %>%
  group_split(group) %>%
  map_df(estimate_shocks)

# Compute passive benchmark for h = 1
##shock_data <- shock_data %>%
#  group_by(group) %>%
#  arrange(date) %>%
#  mutate(
#    L_passive = L * (1 + dplyr::lead(market_ret)),
#    S_passive = L_passive / dplyr::lead(A),
#    AS = dplyr::lead(S) - S_passive
#  ) %>%
#  ungroup()

library(broom)

#######################################################
#estimate_lp <- function(df) {
#  df2 <- df %>% filter(!is.na(AS), !is.na(shock))
#  m <- lm(AS ~ shock + S + L + A, data = df2)
#  tidy(m) %>% mutate(group = unique(df$group))
#}

#lp_results <- shock_data %>%
#  group_split(group) %>%
#  map_df(estimate_lp)

#lp_results %>%
#  filter(term == "shock") %>%
#  select(group, estimate, std.error, p.value)
########################################################


compute_active_rebalancing <- function(df, h) {
    # Ensure observations are in time order before using lead/future windows
    df <- df %>% arrange(date)
    n_obs <- nrow(df)

    # For each time i, compute cumulative gross market return from i+1 to i+h:
    #   Π_{j=1..h} (1 + r_{i+j})
    # If horizon exceeds sample end, return NA.
    future_growth <- vapply(seq_len(n_obs), function(i) {
        if (i + h > n_obs) {
            NA_real_
        } else {
            prod(1 + df$market_ret[(i + 1):(i + h)])
        }
    }, numeric(1))

    df %>%
        mutate(
            # Counterfactual (passive) equity level at horizon h:
            # current equity grown only by market returns
            L_passive = L * future_growth,

            # Counterfactual passive equity share at horizon h:
            # passive equity divided by realized future total assets
            S_passive = L_passive / dplyr::lead(A, h),

            # Active rebalancing measure:
            # realized future share minus passive future share
            AS_h = dplyr::lead(S, h) - S_passive,

            # raw share at horizon h (for sensitivity check):
            S_h = dplyr::lead(S, h)
        )
}


# Estimate local-projection regression for a single group at horizon h
estimate_lp_h <- function(df, h) {
    # Build horizon-h active rebalancing variable and keep complete cases
    df_h <- compute_active_rebalancing(df, h) %>%
        filter(!is.na(.data$AS_h), !is.na(.data$shock))

    # Regress active rebalancing on valuation shock + controls
     m <- lm(AS_h ~ shock + dplyr::lag(AS_h), data = df_h)

    #############################################################
    # Regress raw share on shock + controls
    # m <- lm(S_h ~ shock + dplyr::lag(S_h), data = df_h)
    ##############################################################

    # Return tidy coefficients with metadata for horizon and group
 
   broom::tidy(m) %>%
        mutate(h = h, group = unique(df$group))
}

# Horizons (quarters) for impulse-response estimation
# h = 0 should be excluded as AS_0 = 0 by construction (no future rebalancing at time of shock)
horizons <- 1:8

# Run horizon-by-horizon local-projection regressions for each wealth group
# For each group, estimate impulse responses at horizons 1-8 quarters
# Results combined into single dataframe with horizon (h) and group metadata
lp_results_all <- shock_data %>%
    group_split(group) %>%
    map_df(function(df_group) {
        map_df(horizons, ~ estimate_lp_h(df_group, .x))
    })

# Keep only the shock coefficient to form IRF table
irf <- lp_results_all %>%
    filter(term == "shock") %>%
    select(group, h, estimate, std.error, p.value)

library(ggplot2)

ggplot(irf, aes(x = h, y = estimate, ymin = estimate - 1.96*std.error,
                ymax = estimate + 1.96*std.error, color = group)) +
  geom_line(size = 1.1) +
  geom_ribbon(alpha = 0.15, color = NA) +
  facet_wrap(~group, scales = "free_y") +
  labs(title = "Local Projection IRFs: Active Rebalancing Response to Valuation Shocks",
       x = "Horizon (quarters)", y = "Response of Active Rebalancing") +
  theme_minimal(base_size = 14)


## Panel LPs #########################################

library(fixest)

# Ensure group is a factor
shock_data <- shock_data %>%
  mutate(group = factor(group))

# Horizons for panel LPs
horizons <- 1:8

# For each horizon, build AS_h and run panel LP with group-specific slopes
run_panel_lp_h <- function(h) {
  df_h <- shock_data %>%
    group_by(group) %>%
    group_modify(~ compute_active_rebalancing(.x, h)) %>%
    ungroup() %>%
    filter(!is.na(AS_h), !is.na(shock))

  # group-specific slopes: shock * group, with group + date FE
  m <- feols(
    AS_h ~ shock : group | group + date,
    data    = df_h,
    #cluster = ~ group

    # option: no clustering
    vcov = "hetero"
  )

  broom::tidy(m) %>%
    mutate(h = h)
}

panel_lp_results <- purrr::map_df(horizons, run_panel_lp_h)

# Keep only the shock-by-group slopes (exclude main effects, FE)
panel_irf <- panel_lp_results %>%
  filter(grepl("^shock:group", term)) %>%
  mutate(
    group = sub("^shock:group", "", term)
  ) %>%
  select(group, h, estimate, std.error, p.value)

# Example: test if slope differs between top 0.1 and bottom 50 at horizon 4
h_test <- 4
mod_h4 <- feols(
  AS_h ~ shock : group | group + date,
  data    = shock_data %>%
              group_by(group) %>%
              group_modify(~ compute_active_rebalancing(.x, h_test)) %>%
              ungroup() %>%
              filter(!is.na(AS_h), !is.na(shock)),
  #cluster = ~ group

  #option: no clustering
  vcov = "hetero"
)

summary(mod_h4)
vcov(mod_h4)
coef(mod_h4)

# test joint null of equal slopes across groups
# wald(mod_h4, keep = c("shock:groupb50", "shock:groupp50_90"))
# wald(mod_h4, keep = c("shock:groupb50", "shock:groupp90_99"))
# wald(mod_h4, keep = c("shock:groupb50", "shock:groupp99_999"))
# wald(mod_h4, keep = c("shock:groupb50", "shock:groupp999_1000"))

# test pairwise differences in slopes across groups -> key results for heterogeneity analysis
library(car)
linearHypothesis(mod_h4, "shock:groupb50 = shock:groupp50_90")
linearHypothesis(mod_h4, "shock:groupb50 = shock:groupp90_99")
linearHypothesis(mod_h4, "shock:groupb50 = shock:groupp99_999")
linearHypothesis(mod_h4, "shock:groupb50 = shock:groupp999_1000")

# check on all horizons

## Bayesian VAR #######################################
#--------------------------------------------------
# BVAR with Minnesota prior using BVAR package
#--------------------------------------------------
library(BVAR)
#--------------------------------------------------
# Add AS_h for a chosen horizon h using your function
#--------------------------------------------------

add_AS_h <- function(df, h) {
  df %>%
    compute_active_rebalancing(h) %>%
    filter(!is.na(AS_h)) %>%
    arrange(date)
}

#--------------------------------------------------
# Prepare BVAR data for one group
# var_choice = "S"  → raw share
# var_choice = "AS" → active share AS_h
#--------------------------------------------------

prepare_bvar_data <- function(df, h = 1, var_choice = c("S", "AS")) {
  var_choice <- match.arg(var_choice)

  df_h <- add_AS_h(df, h)

  if (var_choice == "S") {
    df2 <- df_h %>% select(date, dlnL, S, A)
  } else {
    df2 <- df_h %>% select(date, dlnL, AS_h, A) %>%
      rename(AS = AS_h)
  }

  df2 %>% filter(complete.cases(.))
}

#--------------------------------------------------
# Estimate BVAR for one group
#--------------------------------------------------

bvar_data <- prepare_bvar_data(shock_data %>% filter(group == "b50"), h = 1, var_choice = "AS")

######################################
#bvar_data <- prepare_bvar_data(shock_data %>% filter(group == "b50"), h = 1, var_choice = "S")
######################################

library(tidyverse)

bvar_data <- bvar_data %>%
    select(-A) %>%
    column_to_rownames("date") %>%
    as.matrix()


# lag selection -> AIC = 2 for AS, AIC = 3 for S
library(vars)
sel <- vars::VARselect(bvar_data, lag.max = 5, type = "const")
sel$selection

# 2. Priors (Minnesota default)
priors <- bv_priors()

# 3. Estimate BVAR (1 lag, minimal settings)
x <- bvar(
  bvar_data,
  lags   = 3,
  n_draw = 2000,
  n_burn = 1000,
  verbose = TRUE
)

# 4. Forecasts and IRFs
predict(x) <- predict(x, horizon = 8)
irf(x)     <- irf(x, horizon = 8)

# Optional: plot
plot(irf(x))
a <- plot(irf(x),
    vars_response = "AS",
    #vars_response = "S",
    vars_impulse = "dlnL"
)

a

glimpse(a)
# [,,horizon, variable] -> [,variable]
a$quants[,,3,1]

