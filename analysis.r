library(tidyverse)

df <- read.csv("df.csv", header = TRUE, stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))


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
  m_loc <- lm(dlnL ~ dlnL_lag + Mkt_RF, data = df2)
  # global
  m_glo <- lm(dlnL ~ dlnL_lag, data = df2)
  df2$shock_loc <- resid(m_loc)
  df2$shock_glo <- resid(m_glo)
  df2
}

shock_df <- df |>
    group_split(Category) |>
    map_dfr(estimate_shocks)

# plot (local shock by category)
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    mutate(Category = factor(Category, levels = c("Next9", "Top1", "Bottom50", "Next40"))) %>%
    ggplot(aes(x = Date, y = shock_loc)) +
    geom_line() +
    facet_wrap(~Category, scales = "free_y") +
    labs(title = "Local Valuation Shocks by Wealth Group", x = "Date", y = "Shock") +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank())

# plot (global shock by category)
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    mutate(Category = factor(Category, levels = c("Next9", "Top1", "Bottom50", "Next40"))) %>%
    ggplot(aes(x = Date, y = shock_glo)) +
    geom_line() +
    facet_wrap(~Category, scales = "free_y") +
    labs(title = "Global Valuation Shocks by Wealth Group", x = "Date", y = "Shock") +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank())

# report mean and sd of shocks by category
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    group_by(Category) %>%
    summarise(
        mean_shock_loc = mean(shock_loc, na.rm = TRUE),
        sd_shock_loc = sd(shock_loc, na.rm = TRUE),
        mean_shock_glo = mean(shock_glo, na.rm = TRUE),
        sd_shock_glo = sd(shock_glo, na.rm = TRUE),
        ratio_sd = sd_shock_loc / sd_shock_glo
    )

# also report sd_shock_loc / sd_shock_glo by category
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    group_by(Category) %>%
    summarise(
        sd_shock_loc = sd(shock_loc, na.rm = TRUE),
        sd_shock_glo = sd(shock_glo, na.rm = TRUE),
        ratio_sd = sd_shock_loc / sd_shock_glo
    )

# correlation of local shocks between groups
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    select(Date, Category, shock_loc) %>%
    pivot_wider(names_from = Category, values_from = shock_loc) %>%
    select(-Date) %>%
    cor(use = "pairwise.complete.obs")

# correlation of global shocks between groups
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    select(Date, Category, shock_glo) %>%
    pivot_wider(names_from = Category, values_from = shock_glo) %>%
    select(-Date) %>%
    cor(use = "pairwise.complete.obs")

# shock identification completed
# focus on either local or global shocks

# save shock_df
# write.csv(shock_df, "shock_df.csv", row.names = FALSE)


# bring shock_df -------------------------------------------------------------------
shock_df <- read.csv("shock_df.csv", header = TRUE, stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))

# rename either shock_loc or shock_glo to shock and drop the other
shock_df <- shock_df |> 
    rename(shock = shock_glo) |> 
    dplyr::select(-shock_loc)

# rebalancing decomposition --------------------------------------------------

# Create flexible passive drift measure by regressing dlnL on Mkt_RF and using coef*Mkt_RF as passive drift component

flex_return <- function(df) {
    m <- lm(dlnL ~ Mkt_RF, data = df)
    df %>%
        mutate(
            passive_return = coef(m)["Mkt_RF"] * Mkt_RF
        ) 
}

# report coeff of Mkt_RF in the regression by category
shock_df %>%
    group_by(Category) %>%
    do(broom::tidy(lm(dlnL ~ Mkt_RF, data = .))) %>%
    filter(term == "Mkt_RF") %>%
    dplyr::select(Category, estimate, std.error, p.value)

shock_df <- shock_df |>
    group_split(Category) |>
    map_dfr(flex_return)

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
            # general passive drift
            # prod(1 + df$Mkt_RF[(i + 1):(i + h)])
            # specific passive drift using regression results
            prod(1 + df$passive_return[(i + 1):(i + h)])
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
            S_h = dplyr::lead(S, h),

            # raw cash share at horizon h:
            # compute C/A first
            S_Cash = C / A,
            S_Cash_h = dplyr::lead(S_Cash, h)
        )
}

# core: local projections ----------------------------------------------------
# Estimate local-projection regression for a single group at horizon h
estimate_lp_h <- function(df, h) {
    # Build horizon-h active rebalancing variable and keep complete cases
    df_h <- compute_active_rebalancing(df, h) %>%
        filter(!is.na(.data$AS_h), !is.na(.data$shock))

    # Regress raw share without controls --- Consistent with BVAR (using global shocks)
    m <- lm(S_h ~ shock + dplyr::lag(S_h), data = df_h)

    # Omit the lag not to absorb the shock effect
    # m <- lm(S_h ~ shock, data = df_h)

    # Regress raw share on shock + controls
    # m <- lm(S_h ~ shock + dplyr::lag(S_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

    # Regress cash share on shock + controls
    # m <- lm(S_Cash_h ~ shock + dplyr::lag(S_Cash_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

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
    dplyr::select(group, h, estimate, std.error, p.value) |>
    filter(!group %in% c("RemainingTop1", "TopPt1")) |>
    mutate(group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40")))

ggplot(irf, aes(x = h, y = estimate, ymin = estimate - 1.96*std.error,
                ymax = estimate + 1.96*std.error, color = group)) +
  geom_line(size = 1.1) +
  geom_ribbon(alpha = 0.15, color = NA) +
  facet_wrap(~group, scales = "free_y") +
  labs(title = "Local Projections: Customized Response to Valuation Shocks",
       x = "Horizon (quarters)", y = NULL) +
  theme_minimal(base_size = 14)


# time-varying local projections -------------------------------------------------------

df |>
  dplyr::filter(Category == "Bottom50") |>
  dplyr::select(Date, Mkt_RF) |>
  print(n = 150)

# plot Mkt_RF category Bottom50 for the range between 2006 and 2009
df |>
  dplyr::filter(Category == "Bottom50", Date >= as.Date("2006-01-01"), Date <= as.Date("2009-12-31")) |>
  ggplot(aes(x = Date, y = Mkt_RF)) +
  geom_line() +
  labs(title = "Mkt_RF for Bottom50 (2006-2009)", x = "Date", y = "Mkt_RF") +
  theme_minimal(base_size = 14)

# Takeaway: -22.3% maximum drawdown observed in 2008-Q4 
# repeat local projections by splitting sample into pre-2008 and post-2008
# periods, using 2008-Q4 as cutoff (no dummies/interactions)

estimate_lp_h_period <- function(df, h, start_date = NULL, end_date = NULL) {
    df_h <- compute_active_rebalancing(df, h) |>
        dplyr::filter(!is.na(AS_h), !is.na(shock))

    if (!is.null(start_date)) {
        df_h <- df_h |> dplyr::filter(Date >= start_date)
    }
    if (!is.null(end_date)) {
        df_h <- df_h |> dplyr::filter(Date <= end_date)
    }

    m <- lm(
        AS_h ~ shock + dplyr::lag(AS_h) + Fixed.income +
            Private.businesses + Real.estate + Liabilities,
        data = df_h
    )

    broom::tidy(m) |>
        dplyr::mutate(h = h, group = unique(df$Category))
}

run_lp_period <- function(data, horizons, start_date = NULL, end_date = NULL) {
    data |>
        dplyr::group_split(Category) |>
        purrr::map_dfr(function(df_group) {
            purrr::map_dfr(
                horizons,
                ~ estimate_lp_h_period(
                    df_group,
                    .x,
                    start_date = start_date,
                    end_date = end_date
                )
            )
        })
}

cutoff_date <- as.Date("2008-10-01")

lp_results_pre <- run_lp_period(
    shock_df,
    horizons,
    start_date = NULL,
    end_date = cutoff_date
)

lp_results_post <- run_lp_period(
    shock_df,
    horizons,
    start_date = cutoff_date + 1,
    end_date = NULL
)

irf_pre <- lp_results_pre |>
    dplyr::filter(term == "shock") |>
    dplyr::filter(!group %in% c("RemainingTop1", "TopPt1")) |>
    dplyr::mutate(
        group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40"))
    ) |>
    dplyr::select(group, h, estimate, std.error, p.value)

irf_post <- lp_results_post |>
    dplyr::filter(term == "shock") |>
    dplyr::filter(!group %in% c("RemainingTop1", "TopPt1")) |>
    dplyr::mutate(
        group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40"))
    ) |>
    dplyr::select(group, h, estimate, std.error, p.value)

# Plot 1: Pre-2008
ggplot(irf_pre, aes(x = h, y = estimate)) +
    geom_line(size = 1.1, color = "#1f77b4") +
    geom_ribbon(
        aes(
            ymin = estimate - 1.96 * std.error,
            ymax = estimate + 1.96 * std.error
        ),
        alpha = 0.15,
        fill = "#1f77b4",
        color = NA
    ) +
    facet_wrap(~group, scales = "free_y") +
    labs(
        title = "Local Projections (Pre-2008): Response to Valuation Shocks",
        x = "Horizon (quarters)",
        y = "Response of Active Rebalancing"
    ) +
    theme_minimal(base_size = 14)

# Plot 2: Post-2008
ggplot(irf_post, aes(x = h, y = estimate)) +
    geom_line(size = 1.1, color = "#d62728") +
    geom_ribbon(
        aes(
            ymin = estimate - 1.96 * std.error,
            ymax = estimate + 1.96 * std.error
        ),
        alpha = 0.15,
        fill = "#d62728",
        color = NA
    ) +
    facet_wrap(~group, scales = "free_y") +
    labs(
        title = "Local Projections (Post-2008): Response to Valuation Shocks",
        x = "Horizon (quarters)",
        y = "Response of Active Rebalancing"
    ) +
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
    # m <- lm(AS_h ~ shock_pos + shock_neg + dplyr::lag(AS_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

    # Regress raw equity share
    # m <- lm(S_h ~ shock_pos + shock_neg + dplyr::lag(S_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

    # Regress cash share
    # m <- lm(S_Cash_h ~ shock_pos + shock_neg + dplyr::lag(S_Cash_h) + Fixed.income + Private.businesses + Real.estate + Liabilities, data = df_h)

   # Regress raw equity share with the lag and without controls --- consistent with BVAR
   m <- lm(S_h ~ shock_pos + shock_neg + dplyr::lag(S_h), data = df_h)

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
    filter(!group %in% c("RemainingTop1", "TopPt1")) |>
    # rearrange by next9, top1, bottom50, next40 in order
    mutate(group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40"))) |>
    filter(term %in% c("shock_pos", "shock_neg")) |>
    dplyr::select(group, h, term, estimate, std.error, p.value) 

ggplot(irf_asymmetric, aes(x = h, y = estimate, color = term)) +
    geom_line(size = 1.1) +
    facet_wrap(~group, scales = "free_y") +
    labs(title = "Local Projections: Customized Response to Positive vs Negative Valuation Shocks",
         x = "Horizon (quarters)", y = "Response of Active Rebalancing") +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank())


# Panel LPs -------------------------------------------------------

library(fixest)

# Ensure group is a factor
panel_df <- shock_df %>%
  filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
  mutate(Category = factor(Category))

panel_horizons <- purrr::map_dfr(horizons, function(h) {
    panel_df %>%
        dplyr::group_by(Category) %>%
        dplyr::group_modify(~ compute_active_rebalancing(.x, h)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Category) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
            h = h,
            AS_h_lag = dplyr::lag(AS_h)
        ) %>%
        dplyr::ungroup() %>%
        tidyr::drop_na(AS_h, shock, AS_h_lag, Category, Date)
})

pairwise_groups <- combn(levels(panel_df$Category), 2, simplify = FALSE)

cross_group_results <- purrr::map(horizons, function(h_value) {
    data_h <- panel_horizons %>% dplyr::filter(h == h_value)

    mod_h <- fixest::feols(
        # without controls
        AS_h ~ shock:Category + AS_h_lag | Category + Date,
        # with controls
        # AS_h ~ shock:Category + AS_h_lag + Fixed.income + Private.businesses + Real.estate + Liabilities | Category + Date,
        data = data_h,
        vcov = "hetero"

    )

    tests_h <- purrr::map_dfr(pairwise_groups, function(g) {
        lh <- car::linearHypothesis(
            mod_h,
            sprintf("shock:Category%s = shock:Category%s", g[1], g[2]),
            test = "Chisq"
        )
        lh_df <- as.data.frame(lh)

        p_val <- as.numeric(lh_df[2, "Pr(>Chisq)"])

        tibble::tibble(
            h = h_value,
            group1 = g[1],
            group2 = g[2],
            chisq = as.numeric(lh_df[2, "Chisq"]),
            df = as.numeric(lh_df[2, "Df"]),
            p.value = p_val,
            sig = dplyr::case_when(
                p_val < 0.01 ~ "***",
                p_val < 0.05 ~ "**",
                p_val < 0.10 ~ "*",
                TRUE ~ ""
            )
        )
    })

    list(mod = mod_h, tests = tests_h)
})

cross_group_tests <- purrr::map_dfr(cross_group_results, "tests")
print(cross_group_tests, n = Inf)

mod <- cross_group_results[[1]]$mod
mod

# Bayesian VAR -------------------------------------------------------------------------
# Because the active rebalancing measure is defined relative to a horizon‑specific passive benchmark, it is naturally suited to the local‑projection framework, which estimates separate horizon‑specific responses. In contrast, the BVAR operates on time‑𝑡 state variables and produces multi‑horizon IRFs endogenously. For this reason, the BVAR robustness analysis focuses on the equity share itself rather than the horizon‑specific active component, and checks whether the joint dynamics of equity levels and shares are consistent with the LP‑based rebalancing patterns.
# IRFs from LPs hugged zero with wide bands, so the BVAR is not expected to show strong and significant responses. The main goal is to check whether the point estimates of the BVAR IRFs are generally in line with the LP results, rather than statistical significance.
# The valuation shock is identified externally, and reduced‑form VAR innovations mix shocks to equity wealth and portfolio shares
# Might as well skip BVAR results 

bvar_df_b50 <- shock_df |>
    filter(Category == "Next9") |>
    dplyr::select(Date, dlnL, S) |>
    column_to_rownames("Date")

# AIC, HQ, SC, FPE
sel<- vars::VARselect(bvar_df_b50, lag.max = 5, type = "const")
sel$selection

# BIC
max_lag <- 5
bic_vals <- numeric(max_lag)
for (p in 1:max_lag) {
    var_p <- VAR(bvar_df_b50, p = p, type = "const")
    bic_vals[p] <- BIC(var_p)
}
bic_vals
which.min(bic_vals)   # best lag

# Granger causality ---- non-sense as S is defined as L/A
# var_mod <- VAR(bvar_df_b50, p = 1, type = "const")
# causality(var_mod, cause = "dlnL")$Granger

# BVAR estimation
library(BVAR)

# 2. Priors (Minnesota default)
priors <- bv_priors()

# 3. Estimate BVAR (customize lags and draws)
x <- bvar(
  bvar_df_b50,
  lags   = 1,
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
    vars_response = "S",
    vars_impulse = "dlnL"
)

a

glimpse(a)
# [,,horizon, variable] -> [,variable]
a$quants[,,3,1]






