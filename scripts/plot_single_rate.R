# install.packages("ggplot2")  # if needed
library(targets)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(purrr)

# ---------- 0) Helper: plan → customerType (from detail) ----------
# EnergyPlan (detail) has `customerType` (RESIDENTIAL/BUSINESS). We extract from details_list.
plan_customer_type <- tar_read(details_list) %>%
  map_dfr(~ tibble(
    planId = .x$data$planId %||% NA_character_,
    customerType = .x$data$customerType %||% NA_character_
  )) %>%
  distinct()

# ---------- 1) Base tables ----------
plans_core <- tar_read(plans_core)    # planId, brandName, type (MARKET/STANDING), fuelType, ...
contracts  <- tar_read(contracts_df)  # planId, contract_type, pricingModel
single     <- tar_read(tariff_single_df)  # single-rate usage rows
geo_dnsps  <- tar_read(geo_tbl) %>% select(planId, distributor) %>% distinct()
elig_raw   <- tar_read(eligibility_df)    # planId, type (NEW_CUSTOMER/EXISTING_CUST/etc.)

# Keep just AGL plans
agl_plans <- plans_core %>%
  filter(brandName == "AGL")

# Eligibility flags for NEW / EXISTING
elig_flags <- elig_raw %>%
  filter(type %in% c("NEW_CUSTOMER", "EXISTING_CUST")) %>%
  group_by(planId) %>%
  summarise(
    has_new      = any(type == "NEW_CUSTOMER"),
    has_existing = any(type == "EXISTING_CUST"),
    .groups = "drop"
  )

# ---------- 2) Build AGL single-rate electricity usage dataset ----------
agl_single <- single %>%
  left_join(agl_plans %>% select(planId, brandName, type, fuelType, displayName), by = "planId") %>%
  left_join(contracts %>% select(planId, contract_type, pricingModel), by = "planId") %>%
  left_join(plan_customer_type, by = "planId") %>%
  left_join(elig_flags, by = "planId") %>%
  left_join(geo_dnsps, by = "planId") %>%
  # Filter to electricity, single-rate contracts, DNSP known
  filter(contract_type == "ELECTRICITY",
         pricingModel == "SINGLE_RATE",
         !is.na(distributor)) %>%
  # CDS default for electricity is KWH – fill if missing
  mutate(measure_unit = coalesce(measure_unit, "KWH")) %>%
  filter(toupper(measure_unit) == "KWH") %>%
  # c/kWh ex-GST
  mutate(
    unit_price_num  = parse_number(unit_price),
    unit_price_c_ex = 100 * unit_price_num
  ) %>%
  filter(!is.na(unit_price_c_ex), unit_price_c_ex > 0)

# We only want RESIDENTIAL
agl_single_res <- agl_single %>%
  filter(toupper(customerType) == "RESIDENTIAL" | is.na(customerType)) 
# If customerType missing, you can decide to drop or keep; here we keep (common in PRD)

# ---------- 3) Plot function for a cohort ----------
plot_agl_single_by_dnsps <- function(offer_type = c("MARKET","STANDING"),
                                     cohort = c("NEW","EXISTING")) {
  offer_type <- match.arg(offer_type)
  cohort     <- match.arg(cohort)
  
  dat <- agl_single_res %>%
    filter(type == offer_type) %>%
    {
      if (cohort == "NEW") filter(., has_new %in% TRUE) else filter(., has_existing %in% TRUE)
    }
  
  # Order DNSPs by median for nicer plotting
  dat <- dat %>%
    mutate(distributor = fct_reorder(distributor, unit_price_c_ex, .fun = median, na.rm = TRUE))
  
  ggplot(dat, aes(x = distributor, y = unit_price_c_ex)) +
    geom_boxplot(outlier.alpha = 0.2, width = 0.7) +
    coord_flip() +
    labs(
      title = paste("AGL single-rate usage by DNSP"),
      subtitle = paste(
        "Offer:", offer_type, "| Customer:", if (cohort == "NEW") "New" else "Existing",
        "| Residential only\nUnits: c/kWh (ex GST); single-rate usage blocks"
      ),
      x = "DNSP",
      y = "c/kWh (ex GST)",
      caption = "Source: AER PRD via {targets}"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(size = 9),
      plot.title = element_text(face = "bold")
    )
}

# ---------- 4) Produce the four plots ----------
p1 <- plot_agl_single_by_dnsps("MARKET",   "NEW")      # Market · Residential · New
p2 <- plot_agl_single_by_dnsps("MARKET",   "EXISTING") # Market · Residential · Existing
p3 <- plot_agl_single_by_dnsps("STANDING", "NEW")      # Standing · Residential · New
p4 <- plot_agl_single_by_dnsps("STANDING", "EXISTING") # Standing · Residential · Existing

# Print them (or ggsave if you like)
print(p1); print(p2); print(p3); print(p4)

# Example save:
# ggsave("agl_market_res_new.png", p1, width=10, height=7, dpi=150)
# ggsave("agl_market_res_existing.png", p2, width=10, height=7, dpi=150)
# ggsave("agl_standing_res_new.png", p3, width=10, height=7, dpi=150)
# ggsave("agl_standing_res_existing.png", p4, width=10, height=7, dpi=150)