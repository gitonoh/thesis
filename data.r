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

# write.csv(df, "df.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
library(tidyverse)

df <- read.csv("df.csv", header = TRUE, stringsAsFactors = FALSE) |>
    mutate(Date = as.Date(Date))

