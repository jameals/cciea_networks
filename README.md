# cciea_networks

This repo is intended to support development of fisheries participation networks on the US West Coast, for use in the annual Ecosystem Status Report developed by the CCIEA team.

Workflow for 2022 ESR

1) Pull fish tickets from PacFIN
- Use git repo: https://github.com/jameals/cciea_networks

2) 00_process_fish_tickets_for_networks.Rmd: Process fish tickets to prepare them to make network graphs. 

3) 01_create_networks_cciea.Rmd: Create .rds files that are igraph objects, representing IOPAC port-group specific participation networks for years of interest using 

4) 02_plot_annual_networks_cciea.Rmd: Make IOPAC port-group specific network graphs for years of interest 

5) IOPAC-network-map_script_cciea.Rmd: Add port graphs to maps, create plots of port-specific networks for multiple years or for all ports in a single year

6) network_stats_boxplots.Rmd: Make boxplots of importance and betweenness (basic_btwn) for all species and port groups, barplots of proportion of ports with large values for these network metrics for each port group

7) networks_stats_timeseries.Rmd: Create time series of importance and node strength of the tuna node for WA and OR ports, HCI for comparable PNW region, and edge density of entire networks for all port groups

8) network_stats_boxplots_nonDTS.Rmd: Make boxplots of importance and node strength of the non-DTS groundfish node for all port groups in OR and CA

9) nonDTSgfish_nodestrength_rev_OR_CA_boxplots.Rmd: Make boxplots of importance and node strength, and aggregate revenue and aggregate landings, of the non-DTS groundfish node for all port groups in OR and CA; make scatter plot of node strength and revenue for non-DTS groundfish in OR and CA

10) Write up the text for the ESR. Fisheries Participation Networks 2022 ESR google doc

[Note that Mary made “node_stats_v_network_size.Rmd” to examine sensitivity of network metrics to network size (# nodes)