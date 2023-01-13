library(tidyverse)

# Default plot themes
plot_theme <- ggplot2::theme_set(theme_bw() + 
                                   theme(text = element_text(size = 20), 
                                         axis.text.x = element_text(angle = 60, 
                                                                    hjust = 1))) 

################################################################################

### Read the filtered RPT data file
data_RPT <- readr::read_csv('2011_2022_CMIE_Data.csv')

data_RPT_2 <- data_RPT %>%
  dplyr::select(co_code, year, NIC_2, BG, starts_with('RP_'),
                total_assets:pbdita)

# Computing aggregate RPT---note that it includes double-counting
data_RPT_2 <- data_RPT_2 %>%
  dplyr::mutate('RPT_agg' = RP_SALES+RP_PURCH+RP_FAINVSALES+RP_FAINVPURCH+
                  RP_LOANS+RP_BORROWINGS+RP_GUARGIV+RP_GUARTKN+RP_DOS+RP_INVESTEE)


data_RPT_summary <- data_RPT_2 %>%
  group_by(year) %>%
  summarise('min' = min(RPT_agg, na.rm = T),
            'P10' = quantile(RPT_agg, 0.10, na.rm = T),
            'Q1' = quantile(RPT_agg, 0.25, na.rm = T),
            'med' = quantile(RPT_agg, 0.50, na.rm = T),
            'Q3' = quantile(RPT_agg, 0.75, na.rm = T),
            'P90' = quantile(RPT_agg, 0.90, na.rm = T),
            'max' = max(RPT_agg, na.rm = T),
            'iqr' = IQR(RPT_agg, na.rm =T))

# Median firm's RPT level with time
year_breaks <- 2010:2020

# Median firm's aggregate RPT
plot_med_RPT_agg <- ggplot(data = data_RPT_summary, mapping = aes(x = year, y = med)) +
  geom_point() +
  geom_line(lwd = 0.9, linetype = 'longdash') +
  scale_x_continuous(breaks = year_breaks) +
  geom_vline(xintercept = 2014, linetype = 'dashed') +
  labs(x = '', y = "Median firm's aggregate RPT level")

# Median business group's RPT
# plot_med_RPT_agg_BG <- ggplot(filter(data_RPT_2, BG == 1) %>% group_by(year))

### Aggregate RPT boxplots with time

# With outliers---see how they drive the aggregate behavior
plot_box_RPT_agg_out <- ggplot(data = data_RPT_2,
                               mapping = aes(x = year, y = RPT_agg, group = year)) +
  geom_boxplot() +
  scale_x_continuous(breaks = year_breaks) +
  labs(x = '', y = 'Aggregate RPT distribution')

# Without outliers
plot_box_RPT_agg_no_out <- ggplot(data = data_RPT_2, 
                                  mapping = aes(x = year, y = RPT_agg, group = year)) +
  ylim(0, 1) +
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