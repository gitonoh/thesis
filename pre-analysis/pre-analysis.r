library(tidyverse)

df <- read.csv("df.csv", header = TRUE, stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))


glimpse(df)

# omit "TopPt1" and "RemainingTop1" categories for now
df <- df %>%
    filter(!Category %in% c("TopPt1", "RemainingTop1")) %>%
    mutate(
        Category = recode(
            Category,
            Top1 = "Top 1%",
            Next9 = "Next 9%",
            Next40 = "Next 40%",
            Bottom50 = "Bottom 50%"
        )
    )

###########################################################################

# ggplot L by category ----------------------------------------------------------------
line_equity <- ggplot(df, aes(x = Date, y = L, color = Category)) +
    geom_line() +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_color_manual(values = c(
        "Top 1%" = "#8B1C62",
        "Next 9%" = "#6B8E23",
        "Next 40%" = "#8C8C8C",
        "Bottom 50%" = "#4C72B0"
    )) +
    theme_bw() +
    theme(
        legend.position = c(0.15, 0.80),
        legend.title = element_blank(),
        legend.text = element_text(size = 12.5),
        legend.key.size = unit(1.0, "cm"),
        panel.grid = element_blank()
    )

line_equity
ggsave("pre-analysis/figures/line_equity.png", line_equity, width = 8, height = 6, dpi = 350)


# ggplot A by category ----------------------------------------------------------------
ggplot(df, aes(x = Date, y = A, color = Category)) +
    geom_line() +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_color_manual(values = c("Top 1%" = "#8B1C62", "Next 9%" = "#6B8E23", "Next 40%" = "#8C8C8C", "Bottom 50%" = "#4C72B0")) +
    theme_bw() +
    theme(legend.position = c(0.15, 0.85), legend.title = element_blank(),
          legend.text = element_text(size = 10), legend.key.size = unit(0.8, "cm"))

# plot L in the inner circle of the donut plot, A outside
# use multiple-level pie chart

df <- df |> mutate(net_worth = A - Liabilities)

pie_data <- df %>%
    filter(Date == max(Date)) %>% #2025Q4
    select(Category, L, A, Real.estate, Private.businesses) %>%
    pivot_longer(cols = c(L, A, Real.estate, Private.businesses), names_to = "Type", values_to = "Value") %>%
    mutate(Type = recode(Type, L = "equity", A = "total assets", Real.estate = "real estate", Private.businesses = "private business"))

pie_data <- pie_data |>
    mutate(Type = factor(Type, levels = c("private business", "real estate", "equity", "total assets")))

pie_plot <- ggplot(pie_data, aes(x = Type, y = Value, fill = Category)) +
    geom_bar(stat = "identity", position = "fill") +
    coord_polar(theta = "y") +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_fill_manual(values = c("Top 1%" = "#8B1C62", "Next 9%" = "#6B8E23", "Next 40%" = "#8C8C8C", "Bottom 50%" = "#4C72B0")) +
    theme_bw() +
    theme(legend.position = c(0.9, 0.1), legend.title = element_blank())

pie_plot

#ggsave("pre-analysis/figures/pie_plot.png", pie_plot, width = 10, height = 6, dpi = 350)

# area plot of asset shares by category --------------------------------------------------------------------------

# -> Among selected asset types, real estate (and secondly cash) are dominant for the bottom 50%. 
# -> As wealth distribution moves up, the share of equity, fixed income and private business increases.
# -> Equity sees a dramatic shift, with the top 1% holding it as the largest share of their portfolio.

# mutate liabilities to be negative for area plot
df <- df %>%
    mutate(Liabilities = -Liabilities)

df_long <- df %>%
    pivot_longer(cols = c(L, Private.businesses, Real.estate, Fixed.income, C), names_to = "AssetType", values_to = "Share")

df_long_base <- df %>%
    pivot_longer(cols = c(A, Liabilities), names_to = "AssetType", values_to = "Share")

df_long <- df_long %>%
    mutate(Category = factor(Category, levels = c("Next 9%", "Top 1%", "Bottom 50%", "Next 40%")))

df_long_base <- df_long_base %>%
    mutate(Category = factor(Category, levels = c("Next 9%", "Top 1%", "Bottom 50%", "Next 40%")))

area_plot <-ggplot(df_long, aes(x = Date, y = Share, fill = AssetType)) +
    # Add liabilities (below zero) and total assets (above) as background areas
    #geom_area(
    #    data = df_long_base %>% filter(AssetType == "Liabilities"),
    #    aes(x = Date, y = Share),
    #    inherit.aes = FALSE,
    #    fill = "#4A4A4A",
    #   alpha = 0.5
    #) +
    geom_area(
        data = df_long_base %>% filter(AssetType == "A"),
        aes(x = Date, y = Share),
        inherit.aes = FALSE,
        fill = "#4A4A4A",
        alpha = 0.25
    ) +
    # Stacked areas for asset components on top
    geom_area(position = "stack") +
    #facet_wrap(~Category, ncol = 2, scales = "free_y") +
    facet_wrap(~Category, ncol = 2) +
    scale_fill_manual(
        breaks = c(
            "L",
            "C",
            "Fixed.income",
            "Real.estate",
            "Private.businesses"
        ),
        values = c(
            L = "#1A6FD9",
            Private.businesses = "#7A3EC8",
            Real.estate = "#E66A17",
            Fixed.income = "#1A9F58",
            C = "#D9B200"
        ),
        labels = c(
            L = "Equity",
            Private.businesses = "Private business",
            Real.estate = "Real estate",
            Fixed.income = "Fixed income",
            C = "Cash"
        )
    ) +
    labs(
        title = NULL,
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

area_plot

#ggsave("pre-analysis/figures/area_plot.png", area_plot, width = 10, height = 6, dpi = 350)
#ggsave("pre-analysis/figures/area_plot_yfree_noliabilities.png", area_plot, width = 10, height = 6, dpi = 350)

# Bottom 50% area plot
area_plot_bottom50 <- ggplot(df_long %>% filter(Category == "Bottom 50%"), aes(x = Date, y = Share, fill = AssetType)) +
    geom_area(position = "stack") +
    scale_fill_manual(
        breaks = c(
            "L",
            "C",
            "Fixed.income",
            "Real.estate",
            "Private.businesses"
        ),
        values = c(
            L = "#1A6FD9",
            Private.businesses = "#7A3EC8",
            Real.estate = "#E66A17",
            Fixed.income = "#1A9F58",
            C = "#D9B200"
        ),
        labels = c(
            L = "Equity",
            Private.businesses = "Private business",
            Real.estate = "Real estate",
            Fixed.income = "Fixed income",
            C = "Cash"
        )
    ) +
    labs(
        title = NULL,
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

area_plot_bottom50

#ggsave("pre-analysis/figures/area_plot_bottom50.png", area_plot_bottom50, width = 10, height = 6, dpi = 350)

# Next 40% area plot
area_plot_next40 <- ggplot(df_long %>% filter(Category == "Next 40%"), aes(x = Date, y = Share, fill = AssetType)) +
    geom_area(position = "stack") +
    scale_fill_manual(
        breaks = c(
            "L",
            "C",
            "Fixed.income",
            "Real.estate",
            "Private.businesses"
        ),
        values = c(
            L = "#1A6FD9",
            Private.businesses = "#7A3EC8",
            Real.estate = "#E66A17",
            Fixed.income = "#1A9F58",
            C = "#D9B200"
        ),
        labels = c(
            L = "Equity",
            Private.businesses = "Private business",
            Real.estate = "Real estate",
            Fixed.income = "Fixed income",
            C = "Cash"
        )
    ) +
    labs(
        title = NULL,
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

area_plot_next40

#ggsave("pre-analysis/figures/area_plot_next40.png", area_plot_next40, width = 10, height = 6, dpi = 350)

# next 9% area plot
area_plot_next9 <- ggplot(df_long %>% filter(Category == "Next 9%"), aes(x = Date, y = Share, fill = AssetType)) +
    geom_area(position = "stack") +
    scale_fill_manual(
        breaks = c(
            "L",
            "C",
            "Fixed.income",
            "Real.estate",
            "Private.businesses"
        ),
        values = c(
            L = "#1A6FD9",
            Private.businesses = "#7A3EC8",
            Real.estate = "#E66A17",
            Fixed.income = "#1A9F58",
            C = "#D9B200"
        ),
        labels = c(
            L = "Equity",
            Private.businesses = "Private business",
            Real.estate = "Real estate",
            Fixed.income = "Fixed income",
            C = "Cash"
        )
    ) +
    labs(
        title = NULL,
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

area_plot_next9

#ggsave("pre-analysis/figures/area_plot_next9.png", area_plot_next9, width = 10, height = 6, dpi = 350)

# top 1% area plot
area_plot_top1 <- ggplot(df_long %>% filter(Category == "Top 1%"), aes(x = Date, y = Share, fill = AssetType)) +
    geom_area(position = "stack") +
    scale_fill_manual(
        breaks = c(
            "L",
            "C",
            "Fixed.income",
            "Real.estate",
            "Private.businesses"
        ),
        values = c(
            L = "#1A6FD9",
            Private.businesses = "#7A3EC8",
            Real.estate = "#E66A17",
            Fixed.income = "#1A9F58",
            C = "#D9B200"
        ),
        labels = c(
            L = "Equity",
            Private.businesses = "Private business",
            Real.estate = "Real estate",
            Fixed.income = "Fixed income",
            C = "Cash"
        )
    ) +
    labs(
        title = NULL,
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

area_plot_top1

#ggsave("pre-analysis/figures/area_plot_top1.png", area_plot_top1, width = 10, height = 6, dpi = 350)


## Log difference of asset shares by category --------------------------------------------------------------------------
df_all_categories <- df %>%
  select(Date, Category, L, Private.businesses, C, Fixed.income, Real.estate) %>%
  group_by(Category) %>%
  mutate(
    dlnL = c(NA, diff(log(L))),
    dlnPB = c(NA, diff(log(Private.businesses))),
    dlnC = c(NA, diff(log(C))),
    dlnFI = c(NA, diff(log(Fixed.income))),
    dlnRE = c(NA, diff(log(Real.estate)))
  ) %>%
  ungroup()

df_combined_long <- df_all_categories %>%
  pivot_longer(
    cols = starts_with("dln"),
    names_to = "AssetType",
    values_to = "Value"
  ) %>%
  mutate(AssetType = recode(
    AssetType,
    dlnL = "Equity",
    dlnPB = "Private business",
    dlnC = "Cash",
    dlnFI = "Fixed income",
    dlnRE = "Real estate"
  )) %>%
  # enforce desired order: Equity, Cash, Fixed income, Real estate,
  # Private businesses
  mutate(AssetType = factor(
    AssetType,
    levels = c(
      "Equity",
      "Cash",
      "Fixed income",
      "Real estate",
      "Private business"
    )
  )) %>%
  mutate(Category = factor(Category, levels = c("Top 1%", "Next 9%", "Next 40%", "Bottom 50%")))

volatility <- ggplot(df_combined_long, aes(x = Date, y = Value)) +
  geom_line(aes(color = AssetType, alpha = AssetType)) +
  scale_color_manual(
    values = c(
      "Equity" = "#1A6FD9",
      "Cash" = "#D9B200",
      "Fixed income" = "#1A9F58",
      "Real estate" = "#E66A17",
      "Private business" = "#7A3EC8"
    )
  ) +
  scale_alpha_manual(
    values = c(
      "Equity" = 1,
      "Cash" = 1,
      "Fixed income" = 1,
      "Real estate" = 1,
      "Private business" = 1
    ),
    guide = "none"
  ) +
  facet_grid(Category ~ AssetType) +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

volatility

#ggsave("pre-analysis/figures/volatility_plot.png", volatility, width = 10, height = 6, dpi = 350)

#-> financial assets, particularly equity and fixed income, exhibit relatively pronounced volatility in long differences.
#-> overall, equity is the most volatile type of asset across all wealth groups.


# check cross-group comovement --------------------------------------------------------------------------

df_cross_group <- df_all_categories %>%
    select(Date, Category, dlnL, dlnPB, dlnC, dlnFI, dlnRE) %>%
    pivot_longer(
        cols = starts_with("dln"),
        names_to = "AssetType",
        values_to = "Value"
    ) %>%
    mutate(
        AssetType = recode(
            AssetType,
            dlnL = "Equity",
            dlnPB = "Private business",
            dlnC = "Cash",
            dlnFI = "Fixed income",
            dlnRE = "Real estate"
        ),
        AssetType = factor(
            AssetType,
            levels = c("Equity", "Cash", "Fixed income", "Real estate", "Private business")
        )
    ) %>%
    pivot_wider(names_from = Category, values_from = Value)

cross_group_cor <- df_cross_group %>%
    # omit NA values for correlation calculation
    filter(!is.na(`Top 1%`) & !is.na(`Next 9%`) & !is.na(`Next 40%`) & !is.na(`Bottom 50%`)) %>%
    group_by(AssetType) %>%
    group_split() %>%
    set_names(map_chr(., ~ as.character(unique(.x$AssetType)))) %>%
    map(~ {
        .x %>%
            select(`Top 1%`, `Next 9%`, `Next 40%`, `Bottom 50%`) %>%
            cor(use = "pairwise.complete.obs")
    })

cross_group_cor

cross_group_cor_long <- imap_dfr(cross_group_cor, ~ {
    as_tibble(as.table(.x), .name_repair = "minimal") %>%
        setNames(c("Category1", "Category2", "Correlation")) %>%
    mutate(AssetType = factor(.y, levels = c("Equity", "Cash", "Fixed income", "Real estate", "Private business")))
})

comovement_heatmap <- ggplot(cross_group_cor_long, aes(x = Category1, y = Category2, fill = Correlation)) +
    geom_tile() +
    facet_wrap(~AssetType, ncol = 3) +
    #scale_fill_gradient2(low = "#fcfdbf", mid = "#f98e09", high = "#0a0822", midpoint = 0.5, limits = c(-0.1, 1)) +
    scale_fill_gradient2(low = "white", mid = "#ac7a33", high = "black", midpoint = 0.5, limits = c(-0.1, 1)) +
    labs(title = NULL, x = NULL, y = NULL, fill = "Corr") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = c(0.85, 0.20), legend.title = element_blank())

comovement_heatmap

#ggsave("pre-analysis/figures/comovement_heatmap.png", comovement_heatmap, width = 10, height = 7, dpi = 350)

# -> equity is the strongest in terms of cross-group comovement, suggesting the need to consider flow-based diagnostics.

# flow-based diagnostics --------------------------------------------------------------------------

library(tidyverse)

df <- read.csv("df.csv", header = TRUE, stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))


# omit "TopPt1" and "RemainingTop1" categories for now
df <- df %>%
    filter(!Category %in% c("TopPt1", "RemainingTop1")) %>%
    mutate(
        Category = recode(
            Category,
            Top1 = "Top 1%",
            Next9 = "Next 9%",
            Next40 = "Next 40%",
            Bottom50 = "Bottom 50%"
        )
    )

# bring in market return data
ff <- read.csv("F-F_Research_Data_Factors.csv", header = TRUE, stringsAsFactors = FALSE)

# convert "192607" to date format
ff$Date <- as.Date(paste0(as.numeric(substr(ff$X, 1, 4)), "-", 
                            as.numeric(substr(ff$X, 5, 6)), "-01"))
                            
# convert monthly returns to quarterly returns
ff <- ff %>%
    group_by(quarter = zoo::as.yearqtr(Date)) %>%
    summarise(Mkt_RF = prod(1 + Mkt.RF / 100) - 1,
                        SMB = prod(1 + SMB / 100) - 1,
                        HML = prod(1 + HML / 100) - 1,
                        RF = prod(1 + RF / 100) - 1)

ff <- ff |> mutate(Mkt = Mkt_RF + RF)

# convert quarter "1926 Q3" to "1926-07-01" date format
ff$Date <- zoo::as.Date(zoo::as.yearqtr(ff$quarter, format = "%Y Q%q"), frac = 0)

# keep only "Date" and "Mkt"
ff <- ff %>%
    dplyr::select(Date, Mkt, RF)

# merge df and ff by "Date" narrowing the ff side
df <- merge(df, ff, by = "Date", all.x = TRUE)


# focus on equity
# approximate flow_t = level_t - level_t-1 * (1 + Mkt)

# ln_flow_L

df_equity <- df %>%
    group_by(Category) %>%
    arrange(Date) %>%
    mutate(
        L_lag = lag(L),
        flow_L = L - L_lag * (1 + Mkt),
        dL = c(NA, diff(L)),
        check = dL - flow_L
    ) %>%
    ungroup()

ggplot(df_equity, aes(x = Date, y = flow_L)) +
    geom_line() +
    labs(title = NULL, x = NULL, y = NULL, color = NULL) +
    facet_wrap(~factor(Category, levels = c("Next 9%", "Top 1%", "Bottom 50%", "Next 40%")), ncol = 2) +
    theme_minimal() +
    theme(legend.position = "bottom")

ggplot(df_equity, aes(x = Date, y = dL)) +
    geom_line() +
    labs(title = NULL, x = NULL, y = NULL, color = NULL) +
    facet_wrap(~factor(Category, levels = c("Next 9%", "Top 1%", "Bottom 50%", "Next 40%")), ncol = 2) +
    theme_minimal() +
    theme(legend.position = "bottom")

flow_plot <- ggplot(df_equity, aes(x = Date, y = flow_L)) +
    geom_line(aes(color = "flow approximation")) +
    geom_line(aes(y = dL, color = "level change"), alpha = 0.25) +
    scale_color_manual(values = c("flow approximation" = "#ac7a33", "level change" = "black")) +
    labs(title = NULL, x = NULL, y = NULL, color = NULL) +
    facet_wrap(~factor(Category, levels = c("Next 9%", "Top 1%", "Bottom 50%", "Next 40%")), ncol = 2, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "bottom")

flow_plot
ggsave("pre-analysis/figures/flow_plot.png", flow_plot, width = 10, height = 6, dpi = 350)


# check correlation of flow_L across categories
flow_L_cor <- df_equity %>%
    select(Date, Category, flow_L) %>%
    pivot_wider(names_from = Category, values_from = flow_L) %>%
    select(-Date) %>%
    cor(use = "pairwise.complete.obs")
flow_L_cor

# correlation of dL across categories
dL_cor <- df_equity %>%
    select(Date, Category, dL) %>%
    pivot_wider(names_from = Category, values_from = dL) %>%
    select(-Date) %>%
    cor(use = "pairwise.complete.obs")
dL_cor

# heatmap. flow_L_cor left, dL_cor right with facet_wrap
flow_dL_cor_long <- bind_rows(
    flow_L_cor %>%
        as_tibble(rownames = "Category1") %>%
        pivot_longer(-Category1, names_to = "Category2", values_to = "Correlation") %>%
        mutate(Type = "Flow approximation"),
    dL_cor %>%
        as_tibble(rownames = "Category1") %>%
        pivot_longer(-Category1, names_to = "Category2", values_to = "Correlation") %>%
        mutate(Type = "Level change")
)
flow_dL_cor_long <- flow_dL_cor_long %>%
    mutate(Category1 = factor(Category1, levels = c("Bottom 50%", "Next 40%", "Next 9%", "Top 1%")),
           Category2 = factor(Category2, levels = c("Bottom 50%", "Next 40%", "Next 9%", "Top 1%")),
           Type = factor(Type, levels = c("Flow approximation", "Level change")))

comovement_heatmap_flow_dL <- ggplot(flow_dL_cor_long, aes(x = Category1, y = Category2, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.3f", Correlation)), size = 3) +
    facet_wrap(~Type) +
    scale_fill_gradient2(
        low = "#ac7a33",
        mid = "#e1c49b",
        high = "#fcf9f5",
        midpoint = 0.5,
        limits = c(0, 1)
    ) +
    labs(title = NULL, x = NULL, y = NULL, fill = "Corr") +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.title = element_blank()
    )

comovement_heatmap_flow_dL

ggsave("pre-analysis/figures/comovement_heatmap_flow_dL.png", comovement_heatmap_flow_dL, width = 10, height = 6, dpi = 350)


# -> flow_L is less correlated across categories than dL, suggesting heterogeneity in equity trading behavior.
