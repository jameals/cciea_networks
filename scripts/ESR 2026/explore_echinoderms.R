# first run 00_process_fish_tickets_for_networks_ESR2026.Rmd section 1 (all lines above 424)

rawdat.sub6 <- rawdat.sub5 %>%
  mutate(crab_year = ifelse(tweek < 46, year, year + 1))

## for each crab year...
  ## subset the data frame
tmp_out <- rawdat.sub6 %>%
  filter(crab_year == 2025) %>%
  dplyr::select(c(trip_id, year, crab_year, tdate,agid, 
                  pcgroup, pcid, IOPAC,
                  spid, spid_recode, SPGRPN2, council,
                  grgroup, grid, removal_type,removal_type_code,
                  drvid, drvid_year,proc, fleet, participation_group, pounds, afi_ppp, adj_afi_ppp, revenue, afi_revenue, adj_afi_revenue))

glimpse(tmp_out)

# 1. Join and Prepare the data
plot_data_named <- tmp_out %>%
  filter(SPGRPN2 == 60) %>%
  # Join with your species reference table
  left_join(sp_grps, by = join_by(spid_recode == SPID)) %>%
  # Group by the name and port group
  group_by(IOPAC, PACFIN_SPECIES_COMMON_NAME) %>%
  summarize(total_revenue = sum(adj_afi_revenue, na.rm = TRUE), .groups = "drop_last") %>%
  # Calculate percentages within each port group
  mutate(pct_revenue = (total_revenue / sum(total_revenue)) * 100) %>%
  ungroup() %>%
  # Apply string wrapping to the long names (adjust width as needed, e.g., 20 chars)
  mutate(PACFIN_SPECIES_COMMON_NAME = str_wrap(PACFIN_SPECIES_COMMON_NAME, width = 20)) |>
  # drop port groups without an echinoderm node in the ESR 2026
  filter(
    IOPAC == 'Puget Sound' |
      IOPAC == 'North WA Coast' |
      IOPAC == 'Newport' |
      IOPAC == 'Brookings' |
      IOPAC == 'Fort Bragg' |
      IOPAC == 'Bodega Bay' |
      IOPAC == 'Santa Barbara' |
      IOPAC == 'Los Angeles' |
      IOPAC == 'San Diego'
    )

# 2. Create the ggplot
ggplot(plot_data_named, aes(x = reorder(PACFIN_SPECIES_COMMON_NAME, pct_revenue), 
                            y = pct_revenue, 
                            fill = IOPAC)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~IOPAC, scales = "free_y") +
  labs(
    title = "Percent of Revenue by Species (Echinoderms)",
    subtitle = "Calculated as percentage of total echinoderm revenue within each Port Group",
    x = "Species Name",
    y = "Percent of Echinoderm Group Revenue"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"), # Makes port group labels pop
    axis.text.y = element_text(size = 8)      # Shrink text slightly if list is long
  )

ggsave(
  here::here(
    'data','networks','participation_vessel_ports', 'plots', 'comparable', '2025 only', paste0('echinoderm revenue by iopac_', Sys.Date(),'.png')
  ), 
  width = 10, height = 7,
  dpi=300
)