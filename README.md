## Attribute LSOA11 Counts, Percents, Proportions or Scores to LSOA21 Geography

This tool, written using R-Shiny and which can be run interactively via [https://alexgibsonplymouth.shinyapps.io/LSOA_Attribution/](https://alexgibsonplymouth.shinyapps.io/LSOA11_21Attribution/), uses open source data to attribute LSOA11-based data to LSOA21 geographies, specifically the August 2024 **ONS Postcode Directory** and the **2021 Census** estimate of the number of persons and households in each postcode as of March 2021. These counts are used to weight the attribution of 2011 LSOA-based data to 2021 LSOAs.

The tool has been extensively tested and should be self-explanatory, but it essential that great care is taken to ensure the settings match the data being attributed - specifically (1) whether the attribution relates to Count, Percent, Proportion or Score data, and (2) whether the attribution is to be based on the persons or households. **Use the tool with care and check your results!**

This repository contains the R-Shiny script that runs on shinyapps (LSOA11_21Attribution.R), the datasets it calls upon (LSOA11_LSOA21PopLookups.csv, LSOA11_LSOA21HHLookups.csv & AttributionOrigin.csv), and the R script used to create those datasets (Linking2011and2021StatisticalGeographies.R). Two directories with additional information used by the R-Shiny script are also included. These files/directories can be downloaded and run on a local implementation of R if required, though it will be easier to use the shinyapps implementation noted above.

**Note:** Results may differ (slightly!) from other methods and should only be used if 'official' attributions to 2021 geographies are unavailable (see below for more details).

### Sources:

**August 2024 ONS Postcode Directory**: This lists the 2011 and 2021 LSOAs within which all postcodes are located. Office for National Statistics licensed under the Open Government Licence v.3.0

**Postcode resident and household estimates, England and Wales: Census 2021**: Estimates of usual residents for postcodes, postcode sectors and postcode districts and estimates of households for postcodes, Census 2021 data available via NOMIS under Open Government Licence v.3.0

### Method:

The underlying idea is that, where there is a mismatch between 2011 and 2021 LSOAs, we establish how many people (or households) are in each 2011/2021 overlap and these are converted to proportions. How these proportions are used then depends on whether we are dealing with count or score data (and percent and proportion data are converted to counts on the basis of the number of people or households in each LSOA and then treated as if count data - before being converted back to percents or proportions).

-    Where a 2011 LSOA is split into two (or more) 2021 LSOAs then any 2011 scores (e.g. 2019 IMD Scores) are attributed to all 2021 LSOA whereas any counts (e.g. of recipients of benefits) are split proportionately between the two 2021 LSOAs.

-    Where a number of 2011 LSOA merge to create a single 2021 LSOA any counts are simply added together but for scores the weighted average of 2011 LSOA scores is calculated and attributed to the 2021 LSOA.

-    Where there is (rarely) a more complex relationship between 2011 and 2021 LSOAs then the attribution will reflect the proportionate splits and mergers than underlie the relationship.

It is important to recognise that the attribution uses 2021 Census counts of all persons or households so, even if score, percent, proportion or count data refer to a subset of people or households (e.g. children aged 0-15), proportionate splits are based on the relative size of the total population rather than on the relevant population subset. This may affect results. (Data are not available for population subsets at unit postcode level.) For any given data (particularly composite 'scores' which may be based on a variety of population and/or household datasets) the user will need to decide whether attribution using total 'persons' or 'households' is most appropriate and the final attribution to 2021 LSOAs should be considered as a 'best available fit'.

This this tool has been used whenever 2011 LSOA-based data has been attributed to 2021 LSOAs as part of the ESRC Coastal Classifications project - See https://alexgibsonplymouth.shinyapps.io/CESA_Beta/ (opens new tab)

### Warning

It is important to remember that although the vast majority (94.1%) of 2021 LSOAs are unchanged since 2011, where changes have taken place it is because there have been significant population changes - e.g. large-scale housing developments. This obviously means that older data are likely to be of limited use simply because they describe populations which will, by definition, have changed substantially.

Thus although contemporary data that is still being published using 2011 LSOAs (e.g. benefit data available via Stat-Explore) can be attributed to 2021 LSOAs with some confidence, older data (e.g. 2019 IMD scores which are actually based on 2016 data) are likely to be of limited value.

To help contextualise the attributed data each 2021 output file includes a column stating whether the 2021 LSOA is unchanged since 2011 ('U') or is based on split ('S'), merged ('M') or more complex combinations of 2011 LSOAs ('X').
