# color: ac7a33 (yellow), 4e916e (green), 436f9c (blue), 8e517a (red)

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


# check AR(p) structure of dlnL by category
library(forecast)

df_category <- df |> filter(Category == "Bottom50", !is.na(dlnL))
max_p <- 5
aic_vals <- numeric(max_p)
bic_vals <- numeric(max_p)
for (p in 1:max_p) {
    model <- Arima(df_category$dlnL, order = c(p, 0, 0))
    aic_vals[p] <- AIC(model)
    bic_vals[p] <- BIC(model)
}
aic_vals
bic_vals
which.min(aic_vals)   # best lag by AIC
which.min(bic_vals)   # best lag by BIC

# -> Both AIC and BIC suggest AR(1) is sufficient for dlnL across all categories.


# Estimate local (global) shocks
estimate_shocks <- function(df) {
  df2 <- df |> filter(!is.na(dlnL), !is.na(dlnL_lag))
  # local
  m_loc <- lm(dlnL ~ dlnL_lag + Mkt_RF, data = df2)
  # global
  m_glo <- lm(dlnL ~ dlnL_lag, data = df2)
  df2$shock_loc_raw <- resid(m_loc)
  # convert to 1sd shock
  df2$shock_loc <- df2$shock_loc_raw / sd(df2$shock_loc_raw, na.rm = TRUE)
  df2$shock_glo_raw <- resid(m_glo)
  # convert to 1sd shock
  df2$shock_glo <- df2$shock_glo_raw / sd(df2$shock_glo_raw, na.rm = TRUE)
  df2
}

shock_df <- df |>
    group_split(Category) |>
    map_dfr(estimate_shocks)

# plot (local shock by category)
shock_loc <- shock_df %>%
  filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
  mutate(
    Category = recode(Category,
      "Next9" = "Next 9%",
      "Top1" = "Top 1%",
      "Bottom50" = "Bottom 50%",
      "Next40" = "Next 40%"
    ),
    Category = factor(Category,
      levels = c("Next 9%", "Top 1%", "Bottom 50%", "Next 40%")
    )
  ) %>%
  ggplot(aes(x = Date, y = shock_loc_raw)) +
  geom_line(linewidth = 0.25) +
  facet_wrap(~Category, scales = "free_y") +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

shock_loc

# save shock_loc plot
ggsave("figures/shock_loc.png", shock_loc, width = 10, height = 6, dpi = 350)

# plot (global shock by category)
shock_glo <- shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    mutate(
        Category = recode(Category,
            "Next9" = "Next 9%",
            "Top1" = "Top 1%",
            "Bottom50" = "Bottom 50%",
            "Next40" = "Next 40%"
        ),
        Category = factor(Category,
            levels = c("Next 9%", "Top 1%", "Bottom 50%", "Next 40%")
        )
    ) %>%
    ggplot(aes(x = Date, y = shock_glo_raw)) +
    geom_line(linewidth = 0.25) +
    facet_wrap(~Category, scales = "free_y") +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank())

shock_glo

ggsave("figures/shock_glo.png", shock_glo, width = 10, height = 6, dpi = 350)

# report mean and sd of shocks by category
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    group_by(Category) %>%
    summarise(
        mean_shock_loc = mean(shock_loc_raw, na.rm = TRUE),
        sd_shock_loc = sd(shock_loc_raw, na.rm = TRUE),
        mean_shock_glo = mean(shock_glo_raw, na.rm = TRUE),
        sd_shock_glo = sd(shock_glo_raw, na.rm = TRUE),
        # ratio of standard deviations
        ratio_sd = sd_shock_loc / sd_shock_glo
    )


# correlation of local shocks between groups
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    select(Date, Category, shock_loc) %>%
    pivot_wider(names_from = Category, values_from = shock_loc_raw) %>%
    select(-Date) %>%
    cor(use = "pairwise.complete.obs")

# correlation of global shocks between groups
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    select(Date, Category, shock_glo) %>%
    pivot_wider(names_from = Category, values_from = shock_glo_raw) %>%
    select(-Date) %>%
    cor(use = "pairwise.complete.obs")

# shock identification completed
# focus on either local or global shocks

# save shock_df
#write.csv(shock_df, "shock_df.csv", row.names = FALSE)


# bring shock_df -------------------------------------------------------------------
library(tidyverse)

shock_df <- read.csv("shock_df.csv", header = TRUE, stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))

# rename either shock_loc or shock_glo to shock and drop the other
shock_df <- shock_df |> 
    rename(shock = shock_loc) |> 
    dplyr::select(-shock_glo)

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
    dplyr::select(Category, estimate, std.error, t_stat = statistic, p.value)



shock_df <- shock_df |>
    group_split(Category) |>
    map_dfr(flex_return)



# scatter plot of dlnL vs Mkt_RF by category with respective regression line (no facet, on the same plot)
scatter_plot <-
shock_df %>%
    filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
    ggplot(aes(x = Mkt_RF, y = dlnL, color = Category)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = c(
        "Top1" = "#8B1C62",
        "Next9" = "#6B8E23",
        "Next40" = "#8C8C8C",
        "Bottom50" = "#4C72B0"
    ), labels = c(
        "Top1" = "T1 (1.08, 39.5)",
        "Next9" = "N9 (0.975, 44.2)",
        "Next40" = "N40 (0.737, 35.2)",
        "Bottom50" = "B50 (1.26, 25.1)"
    )) +
    labs(title = NULL,
        x = "Aggregate Market Risk Premium", 
        y = "Group-Specific Log Growth Rate of Equity Balances") +
    theme_bw() +
    theme(
        legend.title = element_blank(),
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1)
    )

scatter_plot

#ggsave("figs/scatter_dlnL_MktRF.png", scatter_plot, width = 6, height = 6, dpi = 350)

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
            prod(1 + df$Mkt_RF[(i + 1):(i + h)])
            # specific passive drift using regression results
            # prod(1 + df$passive_return[(i + 1):(i + h)])
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

            # Counterfactual passive equity share using total financial assets as denominator:
            FS = L / (C + L + Fixed.income),
            PFS_h = PL_h / dplyr::lead(C + L + Fixed.income, h),

            # Active rebalancing measure:
            # realized future share minus passive future share
            AS_h = dplyr::lead(S, h) - PS_h,

            #######
            # Alternative active rebalancing measure using financial share:
            # realized future share minus passive future share using financial assets as denominator
        
            AFS_h = dplyr::lead(FS, h) - PFS_h,
            #######

            # raw share at horizon h (for sensitivity check):
            S_h = dplyr::lead(S, h),

            # raw share using total financial assets (cash + equity + fixed income) as denominator at horizon h:
            FS_h = dplyr::lead(FS, h),

            # raw cash share at horizon h:
            # compute C/A first
            S_Cash = C / A,
            S_Cash_h = dplyr::lead(S_Cash, h),

            # raw cash share using total financial assets (cash + equity + fixed income) as denominator at horizon h:
            FS_Cash = C / (C + L + Fixed.income),
            FS_Cash_h = dplyr::lead(FS_Cash, h),

            # raw Fixed.income share at horizon h:
            S_Fixed.income = Fixed.income / A,
            S_Fixed.income_h = dplyr::lead(S_Fixed.income, h),

            # raw Fixed.income share using total financial assets (cash + equity + fixed income) as denominator at horizon h:
            FS_Fixed.income = Fixed.income / (C + L + Fixed.income),
            FS_Fixed.income_h = dplyr::lead(FS_Fixed.income, h),

            # raw Real.estate share at horizon h:
            S_Real.estate = Real.estate / A,
            S_Real.estate_h = dplyr::lead(S_Real.estate, h),

            # raw Private.businesses share at horizon h:
            S_Private.businesses = Private.businesses / A,
            S_Private.businesses_h = dplyr::lead(S_Private.businesses, h),

            # raw Liabilities at horizon h:
            Liabilities_h = dplyr::lead(Liabilities, h)
        )
}

# plot Mkt_RF and its cumulative return from the initial date
return <- shock_df %>%
    filter(Category == "Bottom50") %>%
    arrange(Date) %>%
    mutate(cum_return = cumprod(1 + Mkt_RF) - 1) %>%
    pivot_longer(c(Mkt_RF, cum_return), names_to = "series", values_to = "value") %>%
    mutate(series = factor(series, levels = c("cum_return", "Mkt_RF"))) %>%
    ggplot(aes(x = Date, y = value)) +
    geom_line(linewidth = 0.25) +
    facet_wrap(~series, ncol = 1, scales = "free_y",
               labeller = as_labeller(c(
                   cum_return = "Cumulative Market Excess Return",
                   Mkt_RF = "Market Excess Return"
               ))) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal(base_size = 14)

return

ggsave("figures/return_cum.png", return, width = 10, height = 6, dpi = 350)

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
    m <- lm(S_h ~ shock + dplyr::lag(S_h) + Private.businesses + Real.estate + Liabilities, data = df_h)

    # Regress raw share on shock + controls + lagged dependent variable using total financial assets as denominator
    # m <- lm(FS_h ~ shock + dplyr::lag(FS_h) + Liabilities, data = df_h)

    # Regress active share on shock + controls + lagged dependent variable
    # m <- lm(AS_h ~ shock + dplyr::lag(AS_h) + Private.businesses + Real.estate + Liabilities, data = df_h)

    # Regress active share on shock + controls + lagged dependent variable using total financial assets as denominator
    # m <- lm(AFS_h ~ shock + dplyr::lag(AFS_h) + Liabilities, data = df_h)

    # Return tidy coefficients with metadata for horizon and group
    broom::tidy(m) %>%
        mutate(h = h, group = unique(df$Category))
}


# Horizons (quarters) for impulse-response estimation
# h = 0 should be excluded as AS_0 = 0 by construction (no future rebalancing at time of shock)
horizons <- 1:8

# longer
horizons <- 1:20

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

irf_raw_plot <- ggplot(irf, aes(x = h, y = estimate)) +
    geom_line(size = 1.1, color = "#ac7a33") +
    geom_ribbon(
        aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
        fill = "#ac7a33",
        alpha = 0.15,
        color = NA
    ) +
    geom_ribbon(
        aes(ymin = estimate - 1 * std.error, ymax = estimate + 1 * std.error),
        fill = "#ac7a33",
        alpha = 0.15,
        color = NA
    ) +
    geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dotted") +
    facet_wrap(~ group, scales = "free_y", 
    labeller = as_labeller(c(
        "Next9" = "Next 9%",
        "Top1" = "Top 1%",
        "Bottom50" = "Bottom 50%",
        "Next40" = "Next 40%"
    ))) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

irf_raw_plot

#ggsave("figs/irf_loc_raw.png", irf_raw_plot, width = 10, height = 6, dpi = 350)


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
        # 2. active component
        AS_h ~ shock + dplyr::lag(AS_h) + Private.businesses + Real.estate + Liabilities,

        # active component using total financial assets as denominator
        # AFS_h ~ shock + dplyr::lag(AFS_h) + Liabilities,

        # 1. raw share
        # S_h ~ shock + dplyr::lag(S_h) + Private.businesses + Real.estate + Liabilities,

        # raw share using total financial assets as denominator
        # FS_h ~ shock + dplyr::lag(FS_h) + Liabilities,

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

# store irf_pre_raw and irf_post_raw
irf_pre_raw <- lp_results_pre |>
    dplyr::filter(term == "shock") |>
    dplyr::filter(!group %in% c("RemainingTop1", "TopPt1")) |>
    dplyr::mutate(
        group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40"))
    ) |>
    dplyr::select(group, h, estimate, std.error, p.value)


irf_post_raw <- lp_results_post |>
    dplyr::filter(term == "shock") |>
    dplyr::filter(!group %in% c("RemainingTop1", "TopPt1")) |>
    dplyr::mutate(
        group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40"))
    ) |>
    dplyr::select(group, h, estimate, std.error, p.value)

# bind, mutatig estimate_pre and estimate_post into estimate, and add period variable
irf_pre_raw <- irf_pre_raw |>
    mutate(period = "pre-2008")
irf_post_raw <- irf_post_raw |>
    mutate(period = "post-2008")

irf_raw <- bind_rows(irf_pre_raw, irf_post_raw)


irf_raw_plot <- ggplot(
    irf_raw,
    aes(x = h, y = estimate, color = period, linetype = period)
) +
    geom_line(size = 1.1) +
    geom_ribbon(
        aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error, fill = period),
        alpha = 0.15,
        color = NA,
        show.legend = FALSE
    ) +
    geom_ribbon(
        aes(ymin = estimate - 1 * std.error, ymax = estimate + 1 * std.error, fill = period),
        alpha = 0.15,
        color = NA,
        show.legend = FALSE
    ) +
    geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dotted") +
    facet_wrap(~ group, scales = "free_y", 
               labeller = as_labeller(c(
                   "Next9" = "Next 9%",
                   "Top1" = "Top 1%",
                   "Bottom50" = "Bottom 50%",
                   "Next40" = "Next 40%"
               ))) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.98, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.5), color = NA)
    ) +
    labs(color = "Period", fill = "Period", linetype = "Period") +
    scale_color_manual(values = c("pre-2008" = "#ac7a33", "post-2008" = "#436f9c"),
                       labels = c("pre-2008" = "pre-2008", "post-2008" = "post-2008")) +
    scale_fill_manual(values = c("pre-2008" = "#ac7a33", "post-2008" = "#436f9c"),
                      labels = c("pre-2008" = "pre-2008", "post-2008" = "post-2008")) +
    scale_linetype_manual(values = c("pre-2008" = "solid", "post-2008" = "solid"),
                          labels = c("pre-2008" = "pre-2008", "post-2008" = "post-2008"))

irf_raw_plot

# ggsave("figs+/irf_loc_2008_raw.png", irf_raw_plot, width = 10, height = 6, dpi = 350)

# active component
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

# report magnitude and volatility using from both irf_pre and irf_post
irf_pre %>%
    group_by(group) %>%
    summarise(
        mean_estimate = mean(estimate, na.rm = TRUE),
        sd_estimate = sd(estimate, na.rm = TRUE)
    )
irf_post %>%
    group_by(group) %>%
    summarise(
        mean_estimate = mean(estimate, na.rm = TRUE),
        sd_estimate = sd(estimate, na.rm = TRUE)
    )

# mechanical component (passive drift) = raw share response - active component response
irf_mechanical_pre <- irf_pre_raw %>%
    left_join(irf_pre, by = c("group", "h"), suffix = c("_raw", "_active")) %>%
    mutate(estimate_mechanical = estimate_raw - estimate_active) %>%
    select(group, h, estimate_mechanical)

irf_mechanical_post <- irf_post_raw %>%
    left_join(irf_post, by = c("group", "h"), suffix = c("_raw", "_active")) %>%
    mutate(estimate_mechanical = estimate_raw - estimate_active) %>%
    select(group, h, estimate_mechanical)

plot_pre <- ggplot(
  irf_pre,
  aes(x = h, y = estimate)
) +
  geom_line(
    aes(linetype = "behavioral"),
    size = 1.1,
    color = "black"
  ) +
  geom_line(
    data = irf_mechanical_pre,
    aes(x = h, y = estimate_mechanical, linetype = "mechanical"),
    color = "black",
    size = 1.1
  ) +
  geom_ribbon(
    aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
    alpha = 0.25,
    fill = "black",
    color = NA
  ) +
  geom_ribbon(
    aes(ymin = estimate - 1 * std.error, ymax = estimate + 1 * std.error),
    alpha = 0.25,
    fill = "black",
    color = NA
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dotted") +
  facet_wrap(
    ~group,
    scales = "free_y",
    labeller = as_labeller(
      c(
        "Next9" = "Next 9%",
        "Top1" = "Top 1%",
        "Bottom50" = "Bottom 50%",
        "Next40" = "Next 40%"
      )
    )
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_linetype_manual(
    name = "Component",
    values = c(behavioral = "solid", mechanical = "dashed"),
    labels = c(
      behavioral = "behavioral component",
      mechanical = "mechanical component\n(counterfactual)"
    )
  ) +
theme_minimal(base_size = 14) +
theme(
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.position = c(0.98, 1),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.5), color = NA)
) +
guides(linetype = guide_legend(title = NULL))

plot_pre

#ggsave("figures/irf_loc_pre_active_flexible.png", plot_pre, width = 10, height = 6, dpi = 350)

# Plot 2: Post-2008
plot_post <- ggplot(
  irf_post,
  aes(x = h, y = estimate)
) +
  geom_line(
    aes(linetype = "behavioral"),
    size = 1.1,
    color = "black"
  ) +
  geom_line(
    data = irf_mechanical_post,
    aes(x = h, y = estimate_mechanical, linetype = "mechanical"),
    color = "black",
    size = 1.1
  ) +
  geom_ribbon(
    aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error),
    alpha = 0.25,
    fill = "black",
    color = NA
  ) +
  geom_ribbon(
    aes(ymin = estimate - 1 * std.error, ymax = estimate + 1 * std.error),
    alpha = 0.25,
    fill = "black",
    color = NA
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dotted") +
  facet_wrap(
    ~group,
    scales = "free_y",
    labeller = as_labeller(
      c(
        "Next9" = "Next 9%",
        "Top1" = "Top 1%",
        "Bottom50" = "Bottom 50%",
        "Next40" = "Next 40%"
      )
    )
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_linetype_manual(
    name = "Component",
    values = c(behavioral = "solid", mechanical = "dashed"),
    labels = c(
      behavioral = "behavioral component",
      mechanical = "mechanical component\n(counterfactual)"
    )
  ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.98, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.5), color = NA)
    ) +
    guides(linetype = guide_legend(title = NULL))


plot_post

#ggsave("figures/irf_loc_post_active_flexible.png", plot_post, width = 10, height = 6, dpi = 350)

# compare pre- and post-2008 in a single plot with different colors and linetypes for behavioral and mechanical components
plot_combined <- ggplot() +
    geom_line(
        data = irf_pre,
        aes(x = h, y = estimate, linetype = "behavioral", color = "pre-2008"),
        size = 1.1
    ) +
    geom_ribbon(
        data = irf_pre,
        aes(x = h, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, fill = "pre-2008", color = "pre-2008"),
        alpha = 0.15,
        color = NA,
        show.legend = FALSE
    ) +
    geom_line(
        data = irf_mechanical_pre,
        aes(x = h, y = estimate_mechanical, linetype = "mechanical", color = "pre-2008"),
        size = 1.1,
        inherit.aes = FALSE
    ) +
    geom_line(
        data = irf_post,
        aes(x = h, y = estimate, linetype = "behavioral", color = "post-2008"),
        size = 1.1
    ) +
    geom_ribbon(
        data = irf_post,
        aes(x = h, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, fill = "post-2008", color = "post-2008"),
        alpha = 0.15,
        color = NA,
        show.legend = FALSE
    ) +
    geom_line(
        data = irf_mechanical_post,
        aes(x = h, y = estimate_mechanical, linetype = "mechanical", color = "post-2008"),
        size = 1.1,
        inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dotted") +
    facet_wrap(
        ~group,
        scales = "free_y",
        labeller = as_labeller(
            c(
                "Next9" = "Next 9%",
                "Top1" = "Top 1%",
                "Bottom50" = "Bottom 50%",
                "Next40" = "Next 40%"
            )
        )
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_color_manual(
        name = "Period",
        values = c("pre-2008" = "#ac7a33", "post-2008" = "#436f9c"),
        labels = c("pre-2008" = "pre-2008", "post-2008" = "post-2008")
    ) +
    #scale_linetype_manual(
    #    name = "Component",
    #    values = c(behavioral = "solid", mechanical = "dashed"),
    #    labels = c(
    #        behavioral = "behavioral component",
    #        mechanical = "mechanical component\n(counterfactual)"
    #    )
    #) +
    scale_fill_manual(
        name = "Period",
        values = c("pre-2008" = "#ac7a33", "post-2008" = "#436f9c"),
        labels = c("pre-2008" = "pre-2008", "post-2008" = "post-2008")
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.98, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.5), color = NA)
    ) +
    guides(color = guide_legend(title = NULL), linetype = "none")

plot_combined

#ggsave("figs+/irf_glo_2008_dec.png", plot_combined, width = 10, height = 6, dpi = 350)



# Assymmetric responses -------------------------------------------------------

horizons <- 1:8

# see number of positive and negative shocks by group
shock_df %>%
    group_by(Category) %>%
    summarise(
        n_positive = sum(shock_loc_raw > 0, na.rm = TRUE),
        n_negative = sum(shock_loc_raw < 0, na.rm = TRUE)
    )

estimate_lp_h_asymmetric <- function(df, h) {
    df_h <- compute_active_rebalancing(df, h) %>%
        filter(!is.na(.data$AS_h), !is.na(.data$shock_loc_raw))

    # Create separate variables for positive and negative shocks
    df_h <- df_h %>%
        mutate(
            # split first, then standardize to 1sd shock
            shock_pos_raw = ifelse(shock_loc_raw > 0, shock_loc_raw, 0),
            shock_neg_raw = ifelse(shock_loc_raw < 0, shock_loc_raw, 0),
            shock_pos = shock_pos_raw / sd(shock_pos_raw[shock_pos_raw > 0], na.rm = TRUE),
            shock_neg = shock_neg_raw / sd(shock_neg_raw[shock_neg_raw < 0], na.rm = TRUE)
        )

    # 2: Regress active share on positive and negative shocks + controls
    # m <- lm(AS_h ~ shock_pos + shock_neg + dplyr::lag(AS_h) + Private.businesses + Real.estate + Liabilities, data = df_h)

    ## Regress active share using total financial assets as denominator on positive and negative shocks + controls
    # m <- lm(AFS_h ~ shock_pos + shock_neg + dplyr::lag(AFS_h) + Liabilities, data = df_h)

    # 1: Regress raw equity share
    # m <- lm(S_h ~ shock_pos + shock_neg + dplyr::lag(S_h) + Private.businesses + Real.estate + Liabilities, data = df_h)

    ## Regress raw equity share using total financial assets as denominator
    # m <- lm(FS_h ~ shock_pos + shock_neg + dplyr::lag(FS_h) + Liabilities, data = df_h)

    # Regress cash share
    # m <- lm(S_Cash_h ~ shock_pos + shock_neg + dplyr::lag(S_Cash_h) + Private.businesses + Real.estate + Liabilities, data = df_h)

    ## Regress cash share using total financial assets as denominator
    # m <- lm(FS_Cash_h ~ shock_pos + shock_neg + dplyr::lag(FS_Cash_h) + Liabilities, data = df_h)

    # Regress fixed income share
    # m <- lm(S_Fixed.income_h ~ shock_pos + shock_neg + dplyr::lag(S_Fixed.income_h) + Private.businesses + Real.estate + Liabilities, data = df_h)

    ## Regress fixed income share using total financial assets as denominator
    # m <- lm(FS_Fixed.income_h ~ shock_pos + shock_neg + dplyr::lag(FS_Fixed.income_h) + Liabilities, data = df_h)

    # Regress real estate share
    # m <- lm(S_Real.estate_h ~ shock_pos + shock_neg + dplyr::lag(S_Real.estate_h) + Private.businesses + Liabilities, data = df_h)

    # Regress private business share
    # m <- lm(S_Private.businesses_h ~ shock_pos + shock_neg + dplyr::lag(S_Private.businesses_h) + Real.estate + Liabilities, data = df_h)

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

# preserve results in raw share
irf_asymmetric_raw <- lp_results_asymmetric |>
    filter(term %in% c("shock_pos", "shock_neg")) |>
    filter(!group %in% c("RemainingTop1", "TopPt1")) |>
    # rearrange by next9, top1, bottom50, next40 in order
    mutate(group = factor(group, levels = c("Next9", "Top1", "Bottom50", "Next40"))) |>
    filter(term %in% c("shock_pos", "shock_neg")) |>
    dplyr::select(group, h, term, estimate, std.error, p.value)

# plot irfs without dashed lines
irf_raw <- ggplot(irf_asymmetric_raw, aes(x = h, y = estimate, color = term)) +
    geom_line(size = 1.1) +
        geom_ribbon(
            aes(
                ymin = estimate - 1.96 * std.error,
                ymax = estimate + 1.96 * std.error,
                fill = term
            ),
            alpha = 0.15,
            color = NA,
            show.legend = FALSE
        ) +
    geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dotted") +
    facet_wrap(~group, scales = "free_y",
    labeller = as_labeller(c(
        "Next9" = "Next 9%",
        "Top1" = "Top 1%",
        "Bottom50" = "Bottom 50%",
        "Next40" = "Next 40%"
    ))) +
    scale_color_manual(
        values = c("shock_pos" = "#4e916e", "shock_neg" = "#8e517a"),
        breaks = c("shock_pos", "shock_neg"),
        labels = c("shock positive", "shock negative")
    ) +
    scale_fill_manual(
        values = c("shock_pos" = "#4e916e", "shock_neg" = "#8e517a"),
        breaks = c("shock_pos", "shock_neg"),
        labels = c("shock positive", "shock negative")
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.98, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = alpha("white", 0.5), color = NA)
    )

irf_raw

# ggsave("figs+/irf_glo_privatebusiness.png", irf_raw, width = 10, height = 6, dpi = 350)

# check irf at each horizon sums up to zero (equity + cash + fixed income)
# separately for positive and negative shocks, and by group
# prepare each irf first
irf_equity <- irf_asymmetric_raw %>%
    filter(term %in% c("shock_pos", "shock_neg")) %>%
    select(group, h, term, estimate) %>%
    rename(estimate_equity = estimate)

irf_cash <- irf_asymmetric_raw %>%
    filter(term %in% c("shock_pos", "shock_neg")) %>%
    select(group, h, term, estimate) %>%
    rename(estimate_cash = estimate)

irf_fixed_income <- irf_asymmetric_raw %>%
    filter(term %in% c("shock_pos", "shock_neg")) %>%
    select(group, h, term, estimate) %>%
    rename(estimate_fixed_income = estimate)

irf_sum <- irf_equity %>%
    left_join(irf_cash, by = c("group", "h", "term")) %>%
    left_join(irf_fixed_income, by = c("group", "h", "term")) %>%
    mutate(estimate_sum = estimate_equity + estimate_cash + estimate_fixed_income)

irf_sum$estimate_sum

# ggplot with term = shock_pos. facet by group.
sum_pos <-ggplot(irf_sum %>% filter(term == "shock_pos"), aes(x = h, y = estimate_sum)) +
    geom_line(aes(color = "sum"), size = 0.5, linetype = "dashed") +
    geom_line(aes(y = estimate_equity, color = "equity"), size = 0.5, linetype = "solid") +
    geom_line(aes(y = estimate_cash, color = "cash"), size = 1.1, linetype = "solid") +
    geom_line(aes(y = estimate_fixed_income, color = "fixed income"), size = 1.1, linetype = "solid") +
    facet_wrap(~group, scales = "free_y",
        labeller = as_labeller(c(
            "Next9" = "Next 9%",
            "Top1" = "Top 1%",
            "Bottom50" = "Bottom 50%",
            "Next40" = "Next 40%"
        ))) +
    scale_color_manual(
        name = "Component",
        values = c(
            "sum" = "black",
            "equity" = "#1A6FD9",
            "cash" = "#D9B200",
            "fixed income" = "#1A9F58"
        ),
        breaks = c("equity", "cash", "fixed income", "sum")
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", legend.title = element_blank())

sum_pos

# ggsave("figs+/irf_loc_finsum_pos.png", sum_pos, width = 10, height = 6, dpi = 350)

# with negative shocks
sum_neg <- ggplot(irf_sum %>% filter(term == "shock_neg"), aes(x = h, y = estimate_sum)) +
    geom_line(aes(color = "sum"), size = 0.5, linetype = "dashed") +
    geom_line(aes(y = estimate_equity, color = "equity"), size = 0.5, linetype = "solid") +
    geom_line(aes(y = estimate_cash, color = "cash"), size = 1.1, linetype = "solid") +
    geom_line(aes(y = estimate_fixed_income, color = "fixed income"), size = 1.1, linetype = "solid") +
    facet_wrap(~group, scales = "free_y",
               labeller = as_labeller(c(
                   "Next9" = "Next 9%",
                   "Top1" = "Top 1%",
                   "Bottom50" = "Bottom 50%",
                   "Next40" = "Next 40%"
               ))) +
    scale_color_manual(
        name = "Component",
        values = c(
            "sum" = "black",
            "equity" = "#1A6FD9",
            "cash" = "#D9B200",
            "fixed income" = "#1A9F58"
        ),
        breaks = c("equity", "cash", "fixed income", "sum")
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", legend.title = element_blank())

sum_neg

# ggsave("figs+/irf_loc_finsum_neg.png", sum_neg, width = 10, height = 6, dpi = 350)

    
# -(passive drift) = active component - raw share
# irf_asymmetric - irf_asymmetric_raw
passive_drift <- irf_asymmetric %>%
    left_join(irf_asymmetric_raw, by = c("group", "h", "term"), suffix = c("_active", "_raw")) %>%
    mutate(estimate_passive = -(estimate_active - estimate_raw)) %>%
    select(group, h, term, estimate_passive)


irf <- ggplot(irf_asymmetric, aes(x = h, y = estimate, color = term)) +
        geom_line(aes(linetype = "dashed"), size = 1.1) +
        geom_line(
            data = passive_drift,
            aes(x = h, y = estimate_passive, color = term, linetype = "solid"),
            size = 1.1,
            inherit.aes = FALSE
        ) +
        geom_ribbon(
            aes(
                ymin = estimate - 1.96 * std.error,
                ymax = estimate + 1.96 * std.error,
                fill = term
            ),
            alpha = 0.15,
            color = NA,
            show.legend = FALSE
        ) +
        geom_hline(yintercept = 0, color = "black", size = 0.5, linetype = "dotted") +
        facet_wrap(
            ~group,
            scales = "free_y",
            labeller = as_labeller(c(
                "Next9" = "Next 9%",
                "Top1" = "Top 1%",
                "Bottom50" = "Bottom 50%",
                "Next40" = "Next 40%"
            ))
        ) +
        scale_color_manual(
            values = c("shock_pos" = "#4e916e", "shock_neg" = "#8e517a"),
            breaks = c("shock_pos", "shock_neg"),
            labels = c("shock positive", "shock negative")
        ) +
        scale_fill_manual(
            values = c("shock_pos" = "#4e916e", "shock_neg" = "#8e517a"),
            breaks = c("shock_pos", "shock_neg"),
            labels = c("shock positive", "shock negative")
        ) +
        guides(linetype = "none") +
        #scale_linetype_manual(
        #    name = "Component",
        #    values = c(active = "solid", passive = "dashed"),
        #    labels = c(active = "behavioral component", passive = "mechanical component\n(counterfactual)")
        #) +
        labs(title = NULL, x = NULL, y = NULL) +
        theme_minimal(base_size = 14) +
        theme(
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.5, "cm"),
            legend.position = c(0.98, 1),
            #legend.position = "bottom",
            legend.justification = c(1, 1),
            legend.background = element_rect(fill = alpha("white", 0.5), color = NA)
        )
irf

#ggsave("figs/irf_loc_asym_dec_flex.png", irf, width = 10, height = 6, dpi = 350)


# Panel LPs -------------------------------------------------------
# set shock global, benchmark general
# or, check all the four patterns

library(fixest)

# Ensure group is a factor
panel_df <- shock_df %>%
  filter(!Category %in% c("RemainingTop1", "TopPt1")) %>%
  mutate(Category = factor(Category))

horizons <- 1:8

panel_horizons <- purrr::map_dfr(horizons, function(h) {
    panel_df %>%
        dplyr::group_by(Category) %>%
        dplyr::group_modify(~ compute_active_rebalancing(.x, h)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Category) %>%
        dplyr::arrange(Date, .by_group = TRUE) %>%
        dplyr::mutate(
            h = h,
            AS_h_lag = dplyr::lag(AS_h),
            AFS_h_lag = dplyr::lag(AFS_h),
        ) %>%
        dplyr::ungroup() %>%
        tidyr::drop_na(AS_h, shock, AS_h_lag, Category, Date)
})

pairwise_groups <- combn(levels(panel_df$Category), 2, simplify = FALSE)


cross_group_results <- purrr::map(horizons, function(h_value) {
    data_h <- panel_horizons %>% dplyr::filter(h == h_value)

    mod_h <- fixest::feols(
        # without controls
        # AS_h ~ shock:Category + AS_h_lag | Category + Date,

        # with controls (baseline)
        AS_h ~ shock:Category + AS_h_lag + Private.businesses + Real.estate + Liabilities | Category + Date,

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

mod <- cross_group_results[[6]]$mod
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






