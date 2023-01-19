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
                total_assets:pbdita) %>%
  dplyr::rename('RPT_SALES' = RP_SALES,
                'RPT_LOANS' = RP_LOANS)


########## Computing aggregate RPT ############

## Without [?] double counting
# Defining aggregate RPT = RPT sales+loans to avoid double counting
data_RPT_2 <- data_RPT_2 %>%
  dplyr::mutate('RPT_agg' = RPT_SALES + RPT_LOANS)

### Based on years

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

# How many observations for each industry classification (NIC_2)
ind_freq <- data_RPT_2 %>%
  dplyr::group_by(NIC_2) %>%
  dplyr::count() %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::filter(n >= 400)

# The top 10
data_RPT_summary_ind_top10 <- data_RPT_summary_ind %>%
  dplyr::filter(NIC_2 %in% ind_freq$NIC_2)

# Aggregate RPT histogram
plot_hist_RPT_agg <- ggplot(data_RPT_2, aes(x = RPT_agg)) +
  geom_histogram(bins = 50)

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

# Empirical distribution function for BG vs non BG RPT
plot_ECDF_RPT_agg_BG <- ggplot(data_RPT_2, 
                               aes(y = RPT_agg, group = BG, color = BG)) +
  stat_ecdf(geom = 'step', linewidth = 0.7) +
  labs(x = 'Quantiles', y = 'Empirical cumulative distribution function')

#######################################
### Descriptive stats with RPT type ###
#######################################

###### SALES ##########

### Yearly ###

data_rpt_sales_summary_TS <- data_RPT_2 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise('min' = min(RPT_SALES, na.rm = T),
            'P10' = quantile(RPT_SALES, 0.10, na.rm = T),
            'Q1' = quantile(RPT_SALES, 0.25, na.rm = T),
            'med' = quantile(RPT_SALES, 0.50, na.rm = T),
            'iqr' = IQR(RPT_SALES, na.rm =T),
            'mean' = mean(RPT_SALES, na.rm = T),
            'std_dev' = sd(RPT_SALES, na.rm = T),
            'Q3' = quantile(RPT_SALES, 0.75, na.rm = T),
            'P90' = quantile(RPT_SALES, 0.90, na.rm = T),
            'P95' = quantile(RPT_SALES, 0.95, na.rm = T),
            'P99' = quantile(RPT_SALES, 0.99, na.rm = T),
            'max' = max(RPT_SALES, na.rm = T),
            'skew' = moments::skewness(RPT_SALES, na.rm = T),
            'kurt' = moments::kurtosis(RPT_SALES, na.rm = T))

### By industry classification ###

data_rpt_sales_summary_ind <- data_RPT_2 %>%
  dplyr::group_by(NIC_2) %>%
  dplyr::summarise('min' = min(RPT_SALES, na.rm = T),
                   'P10' = quantile(RPT_SALES, 0.10, na.rm = T),
                   'Q1' = quantile(RPT_SALES, 0.25, na.rm = T),
                   'med' = quantile(RPT_SALES, 0.50, na.rm = T),
                   'iqr' = IQR(RPT_SALES, na.rm =T),
                   'mean' = mean(RPT_SALES, na.rm = T),
                   'std_dev' = sd(RPT_SALES, na.rm = T),
                   'Q3' = quantile(RPT_SALES, 0.75, na.rm = T),
                   'P90' = quantile(RPT_SALES, 0.90, na.rm = T),
                   'P95' = quantile(RPT_SALES, 0.95, na.rm = T),
                   'P99' = quantile(RPT_SALES, 0.99, na.rm = T),
                   'max' = max(RPT_SALES, na.rm = T),
                   'skew' = moments::skewness(RPT_SALES, na.rm = T),
                   'kurt' = moments::kurtosis(RPT_SALES, na.rm = T))

### By BG classification ###

data_rpt_sales_summary_BG <- data_RPT_2 %>%
  dplyr::group_by(BG) %>%
  dplyr::summarise('min' = min(RPT_SALES, na.rm = T),
                   'P10' = quantile(RPT_SALES, 0.10, na.rm = T),
                   'Q1' = quantile(RPT_SALES, 0.25, na.rm = T),
                   'med' = quantile(RPT_SALES, 0.50, na.rm = T),
                   'iqr' = IQR(RPT_SALES, na.rm =T),
                   'mean' = mean(RPT_SALES, na.rm = T),
                   'std_dev' = sd(RPT_SALES, na.rm = T),
                   'Q3' = quantile(RPT_SALES, 0.75, na.rm = T),
                   'P90' = quantile(RPT_SALES, 0.90, na.rm = T),
                   'P95' = quantile(RPT_SALES, 0.95, na.rm = T),
                   'P99' = quantile(RPT_SALES, 0.99, na.rm = T),
                   'max' = max(RPT_SALES, na.rm = T),
                   'skew' = moments::skewness(RPT_SALES, na.rm = T),
                   'kurt' = moments::kurtosis(RPT_SALES, na.rm = T))


###### LOANS ##########

### Yearly ###

data_rpt_loans_summary_TS <- data_RPT_2 %>%
  dplyr::group_by(year) %>%
  dplyr::summarise('min' = min(RPT_LOANS, na.rm = T),
                   'P10' = quantile(RPT_LOANS, 0.10, na.rm = T),
                   'Q1' = quantile(RPT_LOANS, 0.25, na.rm = T),
                   'med' = quantile(RPT_LOANS, 0.50, na.rm = T),
                   'iqr' = IQR(RPT_LOANS, na.rm =T),
                   'mean' = mean(RPT_LOANS, na.rm = T),
                   'std_dev' = sd(RPT_LOANS, na.rm = T),
                   'Q3' = quantile(RPT_LOANS, 0.75, na.rm = T),
                   'P90' = quantile(RPT_LOANS, 0.90, na.rm = T),
                   'P95' = quantile(RPT_LOANS, 0.95, na.rm = T),
                   'P99' = quantile(RPT_LOANS, 0.99, na.rm = T),
                   'max' = max(RPT_LOANS, na.rm = T),
                   'skew' = moments::skewness(RPT_LOANS, na.rm = T),
                   'kurt' = moments::kurtosis(RPT_LOANS, na.rm = T))

### By industry classification ###

data_rpt_loans_summary_ind <- data_RPT_2 %>%
  dplyr::group_by(NIC_2) %>%
  dplyr::summarise('min' = min(RPT_LOANS, na.rm = T),
                   'P10' = quantile(RPT_LOANS, 0.10, na.rm = T),
                   'Q1' = quantile(RPT_LOANS, 0.25, na.rm = T),
                   'med' = quantile(RPT_LOANS, 0.50, na.rm = T),
                   'iqr' = IQR(RPT_LOANS, na.rm =T),
                   'mean' = mean(RPT_LOANS, na.rm = T),
                   'std_dev' = sd(RPT_LOANS, na.rm = T),
                   'Q3' = quantile(RPT_LOANS, 0.75, na.rm = T),
                   'P90' = quantile(RPT_LOANS, 0.90, na.rm = T),
                   'P95' = quantile(RPT_LOANS, 0.95, na.rm = T),
                   'P99' = quantile(RPT_LOANS, 0.99, na.rm = T),
                   'max' = max(RPT_LOANS, na.rm = T),
                   'skew' = moments::skewness(RPT_LOANS, na.rm = T),
                   'kurt' = moments::kurtosis(RPT_LOANS, na.rm = T))

### By BG classification ###

data_rpt_loans_summary_BG <- data_RPT_2 %>%
  dplyr::group_by(BG) %>%
  dplyr::summarise('min' = min(RPT_LOANS, na.rm = T),
                   'P10' = quantile(RPT_LOANS, 0.10, na.rm = T),
                   'Q1' = quantile(RPT_LOANS, 0.25, na.rm = T),
                   'med' = quantile(RPT_LOANS, 0.50, na.rm = T),
                   'iqr' = IQR(RPT_LOANS, na.rm =T),
                   'mean' = mean(RPT_LOANS, na.rm = T),
                   'std_dev' = sd(RPT_LOANS, na.rm = T),
                   'Q3' = quantile(RPT_LOANS, 0.75, na.rm = T),
                   'P90' = quantile(RPT_LOANS, 0.90, na.rm = T),
                   'P95' = quantile(RPT_LOANS, 0.95, na.rm = T),
                   'P99' = quantile(RPT_LOANS, 0.99, na.rm = T),
                   'max' = max(RPT_LOANS, na.rm = T),
                   'skew' = moments::skewness(RPT_LOANS, na.rm = T),
                   'kurt' = moments::kurtosis(RPT_LOANS, na.rm = T))



##############################################################################
########### Plots with time ##################################################
##############################################################################



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

# Median firm's aggregate RPT over time
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


# Median business group's RPT agg
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


#####################################################################
####### Plots with RPT types ########################################
#####################################################################

### Median firm's aggregate and type-wise RPTs
data_plot_ts_rpt_full <- data_RPT_summary_TS %>%
  select(year, med) %>%
  rename('med_agg' = med) %>%
  left_join(., data_rpt_sales_summary_TS, by = 'year') %>%
  select(year, med_agg, med) %>%
  rename('med_s' = med) %>%
  left_join(., data_rpt_loans_summary_TS, by = 'year') %>%
  select(year, med_agg, med_s, med) %>%
  rename('med_l' = med)

# Making data into panel format
data_plot_rpt_full_long <- data_plot_ts_rpt_full %>%
  tidyr::pivot_longer(., -year, names_to = 'Type', values_to = 'Medians')

# Plotting median firm's rpt by type: agg, sales, loans
plot_med_rpt_type_ts <- ggplot(data_plot_rpt_full_long, 
                               aes(year, Medians, 
                                   shape = Type, 
                                   linetype = Type)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = "Median firm's RPT by type") +
  geom_vline(xintercept = 2014, linetype = 'dashed')

### Top RPT quantiles by type ###

### Loans
data_plot_ts_rpt_type_quantile <- data_RPT_summary_TS %>%
  select(year, P90, P95, P99) %>%
  rename_at(vars(-year), funs(paste0(., '_agg'))) %>%
  left_join(., data_rpt_loans_summary_TS, by = 'year') %>%
  select(year, starts_with('P9')) %>%
  rename('P90_l' = P90,
         'P95_l' = P95,
         'P99_l' = P99) %>%
  left_join(., data_rpt_sales_summary_TS, by = 'year') %>%
  select(year, starts_with('P9')) %>%
  rename('P90_s' = P90,
         'P95_s' = P95,
         'P99_s' = P99)

data_plot_rpt_loans_quantile_long <- data_plot_ts_rpt_type_quantile %>%
  select(year, P90_l, P95_l, P99_l) %>%
  pivot_longer(., -year, names_to = 'Quantiles', values_to = 'RPT_loans')

plot_P90s_rpt_loans_ts <- ggplot(data_plot_rpt_loans_quantile_long, 
                                 aes(x = year, y = RPT_loans, 
                                     shape = Quantiles, 
                                     linetype = Quantiles)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = 'Top quantiles RPT loans') +
  geom_vline(xintercept = 2014, linetype = 'dashed')

### Sales
data_plot_rpt_sales_quantile_long <- data_plot_ts_rpt_type_quantile %>%
  select(year, P90_s, P95_s, P99_s) %>%
  pivot_longer(., -year, names_to = 'Quantiles', values_to = 'RPT_sales')

plot_P90s_rpt_sales_ts <- ggplot(data_plot_rpt_sales_quantile_long, 
                                 aes(x = year, y = RPT_sales, 
                                     shape = Quantiles, 
                                     linetype = Quantiles)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = 'Top quantiles RPT sales') +
  geom_vline(xintercept = 2014, linetype = 'dashed')

#####################################################################
########## Write out relevant .csv files ############################
#####################################################################

# write_csv(data_RPT_summary_BG, 'Agg_RPT_usage_BG.csv')
# write_csv(data_RPT_summary_ind_top10, 'Agg_RPT_usage_industry.csv')
# write_csv(data_RPT_summary_TS, 'Agg_RPT_usage_yearly.csv')
# 