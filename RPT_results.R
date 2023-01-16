library(tidyverse)

# Default plot themes
plot_theme <- ggplot2::theme_set(theme_bw() + 
                                   theme(text = element_text(size = 20), 
                                         axis.text.x = element_text(angle = 60, 
                                                                    hjust = 1))) 

################################################################################

### Read the filtered RPT data file
data_RPT <- readr::read_csv('2011_2022_CMIE_Data.csv', 
                            col_types = cols(co_code = col_factor(),
                                             NIC_2 = col_factor(),
                                             NIC_3 = col_factor()))

data_RPT_2 <- data_RPT %>%
  dplyr::select(co_code, year, NIC_2, BG, starts_with('RP_'),
                total_assets:pbdita)

# Computing aggregate RPT---note that it includes double-counting

## With double counting
 # data_RPT_2 <- data_RPT_2 %>%
 #  dplyr::mutate('RPT_agg' = RP_SALES+RP_PURCH+RP_FAINVSALES+RP_FAINVPURCH+
 #                  RP_LOANS+RP_BORROWINGS+RP_GUARGIV+RP_GUARTKN+RP_DOS+RP_INVESTEE)

## Without [?] double counting
# Defining aggregate RPT = RPT sales+loans to avoid double counting
data_RPT_2 <- data_RPT_2 %>%
  dplyr::mutate('RPT_agg' = RP_SALES+RP_LOANS)

# Time series data summary
data_RPT_summary_TS <- data_RPT_2 %>%
  group_by(year) %>%
  summarise('min' = min(RPT_agg, na.rm = T),
            'P10' = quantile(RPT_agg, 0.10, na.rm = T),
            'Q1' = quantile(RPT_agg, 0.25, na.rm = T),
            'med' = quantile(RPT_agg, 0.50, na.rm = T),
            'iqr' = IQR(RPT_agg, na.rm =T),
            'mean' = mean(RPT_agg, na.rm = T),
            'std_dev' = sd(RPT_agg, na.rm = T),
            'Q3' = quantile(RPT_agg, 0.75, na.rm = T),
            'P90' = quantile(RPT_agg, 0.90, na.rm = T),
            'P95' = quantile(RPT_agg, 0.95, na.rm = T),
            'P99' = quantile(RPT_agg, 0.99, na.rm = T),
            'max' = max(RPT_agg, na.rm = T),
            'skew' = moments::skewness(RPT_agg, na.rm = T),
            'kurt' = moments::kurtosis(RPT_agg, na.rm = T)
            )

# Cross-sectional data summary
data_RPT_summary_CS <- data_RPT_2 %>%
  group_by(co_code) %>%
  summarise('min' = min(RPT_agg, na.rm = T),
            'P10' = quantile(RPT_agg, 0.10, na.rm = T),
            'Q1' = quantile(RPT_agg, 0.25, na.rm = T),
            'med' = quantile(RPT_agg, 0.50, na.rm = T),
            'iqr' = IQR(RPT_agg, na.rm =T),
            'mean' = mean(RPT_agg, na.rm = T),
            'std_dev' = sd(RPT_agg, na.rm = T),
            'Q3' = quantile(RPT_agg, 0.75, na.rm = T),
            'P90' = quantile(RPT_agg, 0.90, na.rm = T),
            'P95' = quantile(RPT_agg, 0.95, na.rm = T),
            'P99' = quantile(RPT_agg, 0.99, na.rm = T),
            'max' = max(RPT_agg, na.rm = T),
            'skew' = moments::skewness(RPT_agg, na.rm = T),
            'kurt' = moments::kurtosis(RPT_agg, na.rm = T)
  )

# Cross-industrial data summary
data_RPT_summary_ind <- data_RPT_2 %>%
  group_by(NIC_2) %>%
  summarise('min' = min(RPT_agg, na.rm = T),
            'P10' = quantile(RPT_agg, 0.10, na.rm = T),
            'Q1' = quantile(RPT_agg, 0.25, na.rm = T),
            'med' = quantile(RPT_agg, 0.50, na.rm = T),
            'iqr' = IQR(RPT_agg, na.rm =T),
            'mean' = mean(RPT_agg, na.rm = T),
            'std_dev' = sd(RPT_agg, na.rm = T),
            'Q3' = quantile(RPT_agg, 0.75, na.rm = T),
            'P90' = quantile(RPT_agg, 0.90, na.rm = T),
            'P95' = quantile(RPT_agg, 0.95, na.rm = T),
            'P99' = quantile(RPT_agg, 0.99, na.rm = T),
            'max' = max(RPT_agg, na.rm = T),
            'skew' = moments::skewness(RPT_agg, na.rm = T),
            'kurt' = moments::kurtosis(RPT_agg, na.rm = T)
  )

data_RPT_summary_ind_top10 <- data_RPT_summary_ind %>%
  dplyr::filter(NIC_2 %in% ind_freq$NIC_2)

# Business vs non-business group data summary
data_RPT_summary_BG <- data_RPT_2 %>%
  group_by(BG) %>%
  summarise('min' = min(RPT_agg, na.rm = T),
            'P10' = quantile(RPT_agg, 0.10, na.rm = T),
            'Q1' = quantile(RPT_agg, 0.25, na.rm = T),
            'med' = quantile(RPT_agg, 0.50, na.rm = T),
            'iqr' = IQR(RPT_agg, na.rm =T),
            'mean' = mean(RPT_agg, na.rm = T),
            'std_dev' = sd(RPT_agg, na.rm = T),
            'Q3' = quantile(RPT_agg, 0.75, na.rm = T),
            'P90' = quantile(RPT_agg, 0.90, na.rm = T),
            'P95' = quantile(RPT_agg, 0.95, na.rm = T),
            'P99' = quantile(RPT_agg, 0.99, na.rm = T),
            'max' = max(RPT_agg, na.rm = T),
            'skew' = moments::skewness(RPT_agg, na.rm = T),
            'kurt' = moments::kurtosis(RPT_agg, na.rm = T)
  )

plot_ECDF_RPT_agg_BG <- ggplot(data_RPT_2, 
                               aes(y = RPT_agg, group = BG, color = BG)) +
  stat_ecdf(geom = 'step', linewidth = 0.7) +
  labs(x = 'Quantiles', y = 'Empirical cumulative distribution function')

##############################################################################
########### Plots with time ##################################################
##############################################################################

# Aggregate RPT histogram
plot_hist_RPT_agg <- ggplot(data_RPT_2, aes(x = RPT_agg)) +
  geom_histogram(bins = 50)

# Power laws?
# plot_RPT_agg_power <- plot_hist_RPT_agg +
#   # Add a log-log scale to the x and y axis
#   scale_x_log10(limits = c(0.1, 10), breaks = trans_breaks("log10", function(x) 10^x)) +
#   scale_y_log10(limits = c(0.1, 100), breaks = trans_breaks("log10", function(x) 10^x)) +
#   # Add a regression line
#   geom_smooth(method = "lm", se = FALSE) +
#   ggtitle("Power law distribution")

# Median firm's RPT level with time
year_breaks <- 2010:2020

# Median firm's aggregate RPT across time
plot_med_RPT_agg <- ggplot(data = data_RPT_summary_TS, 
                           mapping = aes(x = year, y = med)) +
  geom_point() +
  geom_line(linewidth = 0.9, linetype = 'longdash') +
  scale_x_continuous(breaks = year_breaks) +
  geom_vline(xintercept = 2014, linetype = 'dashed') +
  labs(x = '', y = "Median firm's aggregate RPT level")

# Median + Percentile XYZ firms's agg RPT with time
plot_med_RPT_agg_P90P10 <- ggplot(data = data_RPT_summary_TS, 
                           mapping = aes(x = year)) +
  geom_point(aes(y = P10)) +
  geom_line(aes(y = P10), linewidth = 0.6, linetype = 'solid') +
  geom_point(aes(y = Q1)) +
  geom_line(aes(y = Q1), linewidth = 0.4, linetype = 'dotdash') +
  geom_point(aes(y = med)) +
  geom_line(aes(y = med), linewidth = 0.9, linetype = 'longdash') +
  geom_point(aes(y = Q3)) +
  geom_line(aes(y = Q3), linewidth = 0.4, linetype = 'dotdash') +
  geom_point(aes(y = P90)) +
  geom_line(aes(y = P90), linewidth = 0.6, linetype = 'solid') +
  scale_x_continuous(breaks = year_breaks) +
  geom_vline(xintercept = 2014, linetype = 'dashed') +
  labs(x = '', y = "Comparative aggregate RPT levels")

# Largest percentile's aggregate RPTs
# Median + P10 + P90 firms's agg RPT with time
plot_med_RPT_agg_P99P95P90 <- ggplot(data = data_RPT_summary_TS, 
                                  mapping = aes(x = year)) +
  geom_point(aes(y = P99)) +
  geom_line(aes(y = P99), linewidth = 0.5, linetype = 'solid') +
  geom_point(aes(y = P95)) +
  geom_line(aes(y = P95), linewidth = 0.5, linetype = 'dotdash') +
  geom_point(aes(y = P90)) +
  geom_line(aes(y = P90), linewidth = 0.5, linetype = 'dashed') +
  geom_point(aes(y = Q3)) +
  geom_line(aes(y = Q3), linewidth = 0.5, linetype = 'longdash') +
  scale_x_continuous(breaks = year_breaks) +
  geom_vline(xintercept = 2014, linetype = 'dashed') +
  labs(x = '', y = "Comparative aggregate RPT levels")


# Median business group's RPT
#With outliers
plot_RPT_agg_BG_full <- ggplot(data_RPT_2, aes(x = BG, y = RPT_agg, group = BG)) +
  geom_boxplot() +
  labs(x = '', y = 'Aggregate RPT usse: Business group vs non-BG')
# Without outliers
plot_RPT_agg_BG_full_no_out <- ggplot(data_RPT_2, aes(x = BG, y = RPT_agg, group = BG)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0, 0.75) +
  labs(x = '', y = 'Aggregate RPT usse: Business group vs non-BG')

#########################################
### Aggregate RPT boxplots with time ####
#########################################

# With outliers---see how they drive the aggregate behavior
plot_box_RPT_agg_out <- ggplot(data = data_RPT_2,
                               mapping = aes(x = year, y = RPT_agg, group = year)) +
  geom_boxplot() +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = 'Aggregate RPT distribution')

# Without outliers
plot_box_RPT_agg_no_out <- ggplot(data = data_RPT_2, 
                                  mapping = aes(x = year, y = RPT_agg, group = year)) +
  ylim(0, 0.4) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = 'Aggregate RPT distribution')

### Aggregate RPT boxplots for business groups with time

# Business groups
plot_box_RPT_agg_BG <- ggplot(dplyr::filter(data_RPT_2, BG == 1), 
                              aes(year, RPT_agg, group = year)) +
  geom_boxplot() +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = 'Aggregate RPT for business groups')

# Non-business groups
plot_box_RPT_agg_nonBG <- ggplot(dplyr::filter(data_RPT_2, BG == 0), 
                              aes(year, RPT_agg, group = year)) +
  geom_boxplot() +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = 'Aggregate RPT for non business groups')

#####################################################################
#### Aggregate RPT boxplots with industries, classifications etc. ###
#####################################################################

# How many observations for each industry classification (NIC_2)
ind_freq <- data_RPT_2 %>%
  dplyr::group_by(NIC_2) %>%
  dplyr::count() %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::filter(n >= 400)

# Make boxplots for each industry
# With outliers
plot_box_RPT_agg_industry <- ggplot(data_RPT_2 %>% filter(NIC_2 %in% ind_freq$NIC_2), 
                                    aes(x = NIC_2, y = RPT_agg)) +
  geom_boxplot()

# Without outliers
plot_box_RPT_agg_industry_out_no <- ggplot(data_RPT_2 %>% filter(NIC_2 %in% ind_freq$NIC_2), 
                                    aes(x = NIC_2, y = RPT_agg)) +
  ylim(0, 0.5) +
  geom_boxplot(outlier.shape = NA)
