
# US Potato Chip Prices

rm(list = ls(all.names = TRUE))

library(dplyr)
library(lubridate)
library(ggplot2)
library(fredr)
library(scales)

# Get the current month and year
month <- format(Sys.Date(), " |> %B")
year <- format(Sys.Date(), "%Y")
todaysDate <- Sys.Date()
# todaysDate <- format(Sys.Date(), "%d-%b-%Y")

# Set FRED API key
fredr_set_key('a20f11e75b97d7c2b7120f0f9789ea30')

# Retrieve CPI data from FRED
PotatoChipsRawData <- fredr(
    series_id = 'APU0000718311',
    observation_start = as.Date('1971-01-01'),
    observation_end = as.Date(Sys.Date())
)

min_date <- format(min(PotatoChipsRawData$date), '%B %Y')

MonthlyPotatoChips <- PotatoChipsRawData |>
    na.omit() |>
    select(1, 3) %>%
    rename(Date = 1, Value = 2)

# class(medianCPI1$Date)

yearlymedianPotatoChips <- PotatoChipsRawData |>
    rename(Date = 1, Value = 2) |>
    select(1, 3)
    group_by(year = year(Date))  |>
    summarize(yearlyMedianPotatoChips = median(Value))

# Define a custom label function to add a % sign
percent_label <- function(x) {
    paste0(x, "%")
}

# class(yearlymedianSales1$year)

# shows each month
ggplot(MonthlyPotatoChips, aes(Date, Value)) +
    geom_line(linewidth = 0.5) +
    geom_point(color = 'Red', size = 1) +
    scale_x_date(breaks = seq(min(MonthlyPotatoChips$Date, na.rm = TRUE),
                              max(MonthlyPotatoChips$Date, na.rm = TRUE),
                              by = "5 years"),
                 date_labels = "%Y") +  # Ensure correct labeling of years
    scale_y_continuous(limits = c(min(MonthlyPotatoChips$Value, na.rm = TRUE),
                                  max(MonthlyPotatoChips$Value, na.rm = TRUE)),
                       labels = label_dollar()) +
    theme_minimal() +
    ggtitle(paste0("Monthly CPI from ",
                   min(MonthlyPotatoChips$Date, na.rm = TRUE),
                   " to ",
                   max(MonthlyPotatoChips$Date, na.rm = TRUE))) +
    labs(caption = paste0("Blue Hen Analytics - Data from FRED on ",
                          format(Sys.Date(), '%b %d, %Y'))) +  # Ensure Sys.Date() is used
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.90),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.90, 0.25)) +
    xlab("") +
    ylab("")

# Yearly Median Price
# Aggregate data to calculate the median Value for each year
YearlyMedian <- MonthlyPotatoChips %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    summarize(MedianValue = median(Value, na.rm = TRUE), .groups = "drop")

# Create the plot with yearly medians
ggplot(YearlyMedian, aes(x = Year, y = MedianValue)) +
    geom_line(linewidth = 0.5) +
    geom_point(color = 'Red', size = 1) +
    scale_x_continuous(breaks = seq(min(YearlyMedian$Year),
                                    max(YearlyMedian$Year),
                                    by = 5)) +  # Ensure X-axis shows every 5 years
    scale_y_continuous(limits = c(min(YearlyMedian$MedianValue, na.rm = TRUE),
                                  max(YearlyMedian$MedianValue, na.rm = TRUE)),
                       labels = label_dollar()) +
    theme_minimal() +
    ggtitle(paste0("Yearly Median for US Potato Chips from ",
                   min(YearlyMedian$Year),
                   " to ",
                   max(YearlyMedian$Year))) +
    labs(caption = paste0("Blue Hen Analytics - Data from FRED on ",
                          format(Sys.Date(), '%b %d, %Y'))) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.90),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = c(0.90, 0.25)) +
    xlab("Year") +
    ylab("Median CPI")

# Create a box plot showing the median price for each year.
# Add a Year column to group data
MonthlyPotatoChips <- MonthlyPotatoChips %>%
    mutate(Year = year(Date)) |>
    filter(Year >= 2000)

# Create the box plot
ggplot(MonthlyPotatoChips, aes(x = as.factor(Year), y = Value)) +
    geom_boxplot(outlier.color = "red", outlier.size = 1) +
    scale_x_discrete(breaks = seq(min(MonthlyPotatoChips$Year),
                                  max(MonthlyPotatoChips$Year),
                                  by = 5)) +  # Ensure X-axis shows every 5 years
    scale_y_continuous(labels = label_dollar()) +
    theme_minimal() +
    labs(title = paste0("Monthly US Potato Chips from ",
                        min(MonthlyPotatoChips$Year),
                        " to ",
                        max(MonthlyPotatoChips$Year)),
         subtitle = "Cost per 16 Ounces") +
    labs(caption = paste0("Blue Hen Analytics - Data from FRED on ",
                          format(Sys.Date(), '%b %d, %Y'))) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.90),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") + # No legend needed for a box plot
    xlab("") +
    ylab("")






