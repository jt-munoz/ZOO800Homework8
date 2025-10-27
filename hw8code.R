# HW Week 8
# Joey + Jaden
# 24 Oct 2025

# install packages if needed
# install.packages("EDIutils")
# install.packages("readr")
# install.packages("scales")

# load packages
library(EDIutils)
library(readr)
library(ggplot2)
library(dplyr)
library(scales)

#### Objective 1 ####
# Read zooplankton data from EDI

scope <- "knb-lter-ntl"
identifier <- "90"
revision <- list_data_package_revisions(scope, identifier, filter = "newest")
package_id <- paste(scope, identifier, revision, sep = ".")

entity_names <- read_data_entity_names(package_id)
entity_id <- entity_names$entityId[
  grepl("zooplankton", entity_names$entityName, ignore.case = TRUE)
][1]

# Safely read data from raw bytes
raw_bytes <- read_data_entity(package_id, entity_id)
con <- rawConnection(raw_bytes)
on.exit(close(con), add = TRUE)
data <- read_csv(file = con, show_col_types = FALSE)

#### Objective 2 ####

# FIRST FIGURE
# annual mean Zooplankton density by LTER (southern) Lake

# mean monthly Zooplankton density per lake
data$sample_date <- as.Date(data$sample_date)
df <- subset(data, !is.na(lakeid) & !is.na(density) & !is.na(sample_date))

# group based on first day of the month
df$month <- as.Date(paste0(format(df$sample_date, "%Y-%m"), "-01"))

# aggregate data based on month column
monthly <- aggregate(density ~ lakeid + month, data = df, FUN = mean, na.rm = TRUE)

# convert dates to as.Date, add year column
data$sample_date <- as.Date(data$sample_date)
data$year <- format(data$sample_date, "%Y")

# total mean density per year across all species
annual <- aggregate(density ~ lakeid + year, data = data, FUN = mean, na.rm = TRUE)

# plot annual mean zooplankton density by lake
Zoop_annual_by_lake <- ggplot(annual, aes(x = as.numeric(year), y = density, color = lakeid, group = lakeid)) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Annual Mean Zooplankton Density by Lake",
    x = "Year",
    y = "Mean Density (per m³)",
    color = "Lake ID"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

Zoop_annual_by_lake

# this includes data from Mendota past 2019, which doesn't really tell us anything without comparison to the other lakes.

# next step is to trim down to 2019 to compare with other lakes
# don't exclude the data, just trim the graph with xlim and ylim

# Filter out missing values
df <- subset(data, !is.na(lakeid) & !is.na(density) & !is.na(sample_date))

# Create a 'year' variable from sample_date
df$year <- format(df$sample_date, "%Y")

# Aggregate across species: mean zooplankton density per lake per year
yearly <- aggregate(density ~ lakeid + year, data = df, FUN = mean, na.rm = TRUE)

# Plot yearly average density trends for each lake
Zoop_annual_by_lake_trimmed <- ggplot(yearly, aes(x = as.numeric(year), y = density, color = lakeid, group = lakeid)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  xlim(1995, 2019) +
  ylim(0, 3e+05) +
  labs(
    title = "Yearly Mean Zooplankton Density by Lake",
    x = "Year",
    y = "Mean Density (per m³)",
    color = "Lake ID"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

Zoop_annual_by_lake_trimmed

## This plot shows, at a very coarse scale, the Zooplankton densities in each of the NTL Southern lakes.
## We see a large jump in total density around 2010

# SECOND FIGURE
# monthly mean Zoop density over a much shorter span to see monthly resolution

# Ensure proper date format
data$sample_date <- as.Date(data$sample_date)

# Filter valid records
df <- subset(data, !is.na(lakeid) & !is.na(density) & !is.na(sample_date))

# Create monthly variable (first day of each month)
df$month <- as.Date(paste0(format(df$sample_date, "%Y-%m"), "-01"))

# calculate monthly mean density for each lake (use lakeid and month columns)
monthly <- df %>%
  group_by(lakeid, month) %>%
  summarise(mean_density = mean(density, na.rm = TRUE), .groups = "drop")

monthly$year <- as.numeric(format(monthly$month, "%Y"))

# Identify 3 consecutive years where ALL lakes have data
lake_coverage <- monthly %>%
  group_by(year) %>%
  summarise(n_lakes = n_distinct(lakeid))

all_lakes <- length(unique(monthly$lakeid))
valid_years <- lake_coverage$year[lake_coverage$n_lakes == all_lakes]

window_years <- NULL
if (length(valid_years) >= 3) {
  for (i in seq_along(valid_years)) {
    seq3 <- valid_years[i:(i + 2)]
    if (length(seq3) == 3 && diff(range(seq3)) == 2) {
      window_years <- seq3
      break
    }
  }
}

if (is.null(window_years)) stop("No 3 consecutive years found with data for all lakes.")

# Filter to that window
monthly_focus <- subset(monthly, year %in% window_years)

# mark out months to be used to scale x-axis (all months would get crazy fast)
season_months <- c(1, 4, 7, 10)
label_dates <- expand.grid(
  year = window_years,
  month = season_months
) %>%
  mutate(label_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  pull(label_date)

# Month + year labels for axis
label_texts <- format(label_dates, "%b\n%Y")  # stacked month and year

# plot monthly averages in Zoop density for each lake (years 2007, 2008, 2009)
Zoop_monthly_mean_2000s <- ggplot(monthly_focus, aes(x = month, y = mean_density, color = lakeid, group = lakeid)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.8) +
  scale_x_date(
    breaks = label_dates,
    labels = label_texts,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = paste0("Monthly Mean Zooplankton Density by Lake (", min(window_years), "–", max(window_years), ")"),
    x = "Month / Year",
    y = "Mean Density (per m³)",
    color = "Lake ID"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

Zoop_monthly_mean_2000s

## In this dataset, we see some (seemingly isolated) spikes which interrupt the data plotting
## (like Fish Lake Aug 2008 or Monona June 2008).
## A finer temporal resolution as well as other environmental factors may lead to better insights
## as to the drivers of the Zooplankton densities.

# THIRD FIGURE
# relative Zoop composition by lake (all years)

# Filter out species with zero individuals
data_filtered <- subset(data, individuals_measured > 0)

# Summarize totals and compute proportions
lake_species <- aggregate(individuals_measured ~ lakeid + species_name,
                          data = data_filtered, sum, na.rm = TRUE)
lake_species$prop <- ave(lake_species$individuals_measured, lake_species$lakeid,
                         FUN = function(x) x / sum(x))

# Create a color-blind friendly discrete palette (not rainbow)
n_species <- length(unique(lake_species$species_name))
set.seed(42)  # for reproducibility
palette_colors <- hue_pal(l = 65, c = 90, h = c(15, 375))(n_species)
palette_colors <- sample(palette_colors)  # randomize hue order so it’s not rainbow-like

# Plot composition by lake
Zoop_species_comp <- ggplot(lake_species, aes(x = lakeid, y = prop, fill = species_name)) +
  geom_bar(stat = "identity", position = "fill", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = palette_colors, name = "Species") +
  labs(
    title = "Relative Zooplankton Composition by Lake",
    x = "Lake ID",
    y = "Proportion of Individuals"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    legend.key.size = grid::unit(0.4, "cm"),
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold")
  )

Zoop_species_comp

## There are some species which are very rare showing up here, which change the balances of species composition.
## Some species with minimal observations may not have enough information for full statistics.
