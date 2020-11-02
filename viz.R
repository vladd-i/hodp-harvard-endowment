source('styleguide.R')
library(readxl)
library(ggplot2)
library(tidyverse)


# Figure 1:
growth <- read_excel("data/HODP_project.xlsx", 
                      sheet = "endowment_growth")

ggplot(growth, aes(x = average_returns, y = "Returns")) +
  geom_boxplot(outlier.colour = "#EE3838") +
  theme_hodp() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Distribution of Average University\nEndowment Growths In 2008-2019",
       x = "Average Annual Growth Rate") +
  scale_x_continuous(limits = c(1, 1.08), 
                     breaks = c(1, 1.02, 1.04, 1.06, 1.08),
                     labels = c("0%", "2%", "4%", "6%", "8%"))
grid::grid.raster(logo, x = 0, y = 0, just = c('left', 'bottom'), width = unit(1, 'cm'))

# Figure 2:
library(ggrepel)

returns_ivy_plus <- read_excel("data/HODP_project.xlsx", 
                               sheet = "endowment_returns_ivy_plus", 
                               col_types = c("text", "numeric", "numeric", 
                                             "skip", "skip"), n_max = 10)

ggplot(returns_ivy_plus, aes(y = average_returns, x = 0, label = institution)) +
  geom_boxplot(width=0.3) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               dotsize = .5, 
               color = "#EE3838",
               height = 3, 
               stackratio = 1) +
  geom_text_repel(force_pull   = 0, # do not pull toward data points
                  nudge_x      = .2,
                  direction    = "y",
                  hjust        = 0,
                  segment.size = 0.2,
                  max.iter = 1e4, max.time = 1) +
  theme_hodp() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = "Distribution of Average Ivy+\nEndowment Returns In 2009-2020",
       y = "Average Annual Return") +
  scale_y_continuous(limits = c(1.04, 1.1), 
                     breaks = c(1.04, 1.06, 1.08, 1.1),
                     labels = c("4%", "6%", "8%", "10%"))
grid::grid.raster(logo, x = 0, y = 0, just = c('left', 'bottom'), width = unit(1, 'cm'))


# Figure 3:
ggplot(data = returns_ivy_plus, 
       aes(x = stdev, y = average_returns, label = institution)) +
  geom_point() +
  geom_point(data = returns_ivy_plus[ which(returns_ivy_plus$institution == "Harvard"), ],
             aes(x = stdev, y = average_returns), 
             color = "#EE3838",
             size = 3) +
  geom_text(nudge_y = .0023, nudge_x = .0013, family = "Helvetica") +
  theme_hodp() +
  scale_y_continuous(limits = c(1.04, 1.1), 
                     breaks = c(1.04, 1.06, 1.08, 1.1),
                     labels = c("4%", "6%", "8%", "10%")) +
  labs(title = "Risk vs. Average Endowment Returns",
       x = "Standard Deviation of Returns",
       y = "Average Return")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1, 'cm'))

# Figure 4:
asset_allocation <- read_excel("data/HODP_project.xlsx", 
                                sheet = "asset_allocation")

ggplot(asset_allocation, aes(x = year, y = percentage, color = type)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#EE3838", "#4B5973", "#78C4D4")) +
  facet_wrap(~institution) +
  theme_hodp() +
  theme(strip.background = element_rect(fill = "#F2F2F2", color = "#F2F2F2", size=1.5, linetype="solid")) +
  labs(title = "Asset Allocation Over Time",
       x = "Year",
       y = "Percentage") +
  scale_y_continuous(limits = c(0, .5), 
                     breaks = c(0, .1, .2, .3, .4, .5),
                     labels = c("", "10%", "20%", "30%", "40%", "50%"))
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1, 'cm'))

  