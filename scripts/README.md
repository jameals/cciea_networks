## Scripts

Conduct the network analyses used in the CCIEA Annual Ecosystem Status Report to the PFMC and in Fisher et al. (2021, PNAS) using these RMarkdown scripts. They are ordered by step in the analysis, and will source functions from the `R` directory. Landings data are organized by fish ticket, and metiers identified within each port group. Where possible, `.html` files are provided as examples.

CCIEA Annual Ecosystem Status Report to the PFMC processing steps:

The following scripts are the workhorses contained within each "ESR 20xx" subdirectory.

00. Process the raw landings data from PacFIN

01. Construct fisheries participation networks

02. Makes network graphs

03. Makes time series of network statistics (edge density, node strength, etc)

04. Draws port group specific networks for a specified year and places them on a map

summarize_wdfw. creates table describing the % of fish tickets and landings with reported vs derived port information.

Fisher et al. 2021 data processing steps:

(0. process the raw landings data from PacFIN)

1. Use infomap to identify metiers in the landings data of the two reference crab years

2. Select parameter values for the k-nearest neighbor function in script 3

3. Assign metiers to remaining landings data, by fish ticket, using k-nearest neighbor (KNN) and the reference fish tickets from script 1

4. Use infomap to identify metiers in any landings data which could not be assigned a metier using KNN (i.e. novel species / gear group combinations in a single fish ticket)

5. Organize the landings data for use in network construction. This includes combining data across port groups within the same crab year, and producing a 'key' of metiers for each port group.

6. Construct fisheries participation networks for the early / late season of each crab year.

7. Calculate network metrics from participation networks, and graph spatiotemporal variation in the metrics (supplementary figures)

8. Identify the most informative model for each network metric from a set of nested generalized linear models

9. Construct directed networks for the (a) early and (b) late season of the 2015 --> 2016 crab years.
<br>
<br>



Scripts in the **Figures** folder produce the figures used in the Fisher et al. 2021 main text or the supplement.

*Note that the code for the directed networks is specifically constructed to understand vessel participation and spillover centered around the Dungeness crab fishery. If you wish to apply this analysis to a different event / fishery, please contact the authors first!*
