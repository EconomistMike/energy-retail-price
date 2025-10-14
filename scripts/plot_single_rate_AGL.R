# --- Packages ---
library(targets)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)

# --- 1) Build AGL single-rate electricity dataset (kWh, ex-GST) ---
ts    <- tar_read(tariff_single_df)                       # single-rate usage rows
plans <- tar_read(plans_core) %>% select(planId, brandName, type, fuelType)
geo   <- tar_read(geo_tbl)     %>% distinct(planId, distributor)

agl_single <- ts %>%
  inner_join(plans %>% filter(brandName == "AGL"), by = "planId") %>%  # keep only AGL planIds
  left_join(geo, by = "planId") %>%
  mutate(
    measure_unit    = coalesce(measure_unit, if_else(fuelType == "ELECTRICITY","KWH","MJ")),
    unit_price_num  = parse_number(unit_price),       # $/kWh ex-GST
    unit_price_c_ex = 100 * unit_price_num            # c/kWh ex-GST
  ) %>%
  filter(
    fuelType == "ELECTRICITY",
    toupper(measure_unit) == "KWH",
    !is.na(unit_price_c_ex), unit_price_c_ex > 0,
    !is.na(distributor),
    type %in% c("MARKET","STANDING")
  )

# --- 2) Boxplots: Market vs Standing per DNSP (two boxes per DNSP) ---
# Order DNSPs by overall median for nicer plotting
dnsps_order <- agl_single %>%
  group_by(distributor) %>%
  summarise(med = median(unit_price_c_ex, na.rm = TRUE), .groups = "drop")

agl_plot <- agl_single %>%
  left_join(dnsps_order, by = "distributor") %>%
  mutate(distributor = fct_reorder(distributor, med))

p_agl <- ggplot(agl_plot, aes(x = distributor, y = unit_price_c_ex, fill = type)) +
  geom_boxplot(width = 0.7, outlier.alpha = 0.2,
               position = position_dodge2(preserve = "single", padding = 0.4)) +
  coord_flip() +
  labs(
    title    = "AGL single-rate usage prices by DNSP",
    subtitle = "Market vs Standing · Electricity · c/kWh (ex-GST)",
    x = "DNSP", y = "c/kWh (ex-GST)", fill = "Offer type",
    caption  = "Source: AER PRD via {targets}"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9),
        plot.title  = element_text(face = "bold"))

print(p_agl)

# --- 3) Differences (Standing − Market) by DNSP: average & median, clustered bars ---
dnsps_diff <- agl_single %>%
  group_by(distributor, type) %>%
  summarise(
    avg_c    = mean(unit_price_c_ex,   na.rm = TRUE),
    median_c = median(unit_price_c_ex, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  pivot_wider(names_from = type, values_from = c(avg_c, median_c)) %>%
  mutate(
    diff_avg_c    = avg_c_STANDING    - avg_c_MARKET,
    diff_median_c = median_c_STANDING - median_c_MARKET
  )

dnsps_long <- dnsps_diff %>%
  select(distributor, diff_avg_c, diff_median_c) %>%
  pivot_longer(starts_with("diff_"), names_to = "metric", values_to = "difference_c") %>%
  mutate(
    metric      = recode(metric, diff_avg_c = "Average difference", diff_median_c = "Median difference"),
    distributor = fct_reorder(distributor, difference_c, .fun = ~ median(.x, na.rm = TRUE))
  )

p_diff <- ggplot(dnsps_long, aes(x = distributor, y = difference_c, fill = metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  coord_flip() +
  labs(
    title    = "AGL: Standing vs Market price difference by DNSP",
    subtitle = "Difference = Standing − Market (single-rate electricity, c/kWh ex-GST)",
    x = "DNSP", y = "Difference (c/kWh, ex-GST)", fill = "Metric",
    caption  = "Source: AER PRD via {targets}"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9),
        plot.title  = element_text(face = "bold"))

print(p_diff)

# ---- Optional GST-inclusive variants ----
# agl_plot   <- agl_plot   %>% mutate(unit_price_c_inc = unit_price_c_ex * 1.1)
# dnsps_long <- dnsps_long %>% mutate(difference_c     = difference_c     * 1.1)
# ggsave("agl_market_vs_standing_by_dnsp.png", p_agl,  width = 11, height = 8, dpi = 150)
# ggsave("agl_standing_minus_market_diff_by_dnsp.png", p_diff, width = 11, height = 8, dpi = 150)