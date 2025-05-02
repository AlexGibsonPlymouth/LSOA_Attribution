## Attribute LSOA11 Counts, Percents, Proportions or Scores to LSOA21 Geography

This tool uses open source data to attribute LSOA11-based data to LSOA21 geographies, specifically the August 2024 ONS Postcode Directory and the ONS Census estimate of the number of persons and households in each postcode as of March 2021. These counts are used to weight the attribution of 2011 LSOA-based data to 2021 LSOAs.

The tool has been extensively tested and should be self-explanatory, but it essential that great care is taken to ensure the settings match the data being attributed - specifically (1) whether the attribution relates to Count, Percent, Proportion or Score data, and (2) whether the attribution is to be based on the persons or households. Use the tool with care and check your results!

Note: Results will differ (slightly!) from other methods, but this tool has been used whenever 2011 LSOA-based data has been attributed to 2021 LSOAs as part of the ESRC Coastal Classifications project. Also see https://alexgibsonplymouth.shinyapps.io/CESA_Beta/ (opens new tab)

###Sources:
August 2024 ONS Postcode Directory: This lists the 2011 and 2021 LSOAs within which all postcodes are located. Office for National Statistics licensed under the Open Government Licence v.3.0

Postcode resident and household estimates, England and Wales: Census 2021: Estimates of usual residents for postcodes, postcode sectors and postcode districts and estimates of households for postcodes, Census 2021 data available via NOMIS under Open Government Licence v.3.0

###Method:
The R script which integrates the two datasets is Linking2011and2021StatisticalGeographies.R and is available on my Ubuntu PC at /media/alex/c19ab764-fa52-4715-a18c-dfb59d6608c6/alex/!00_CoastalClassificationProject/00_OpenSourceLink2011and2021StatisticalGeographies and on GitHub at I must put this on GitHub!. The R script for the toll - which uses R-Shiny - is also available on GitHub.

The underlying idea is that, where there is a mismatch between 2011 and 2021 LSOAs, we establish how many people (or households) are in each 2011/2021 overlap and these are converted to proportions. How these proportions are used then depends on whether we are dealing with count or score data (and percent and proportion data are converted to counts on the basis of the number of people or households in each LSOA and then treated as if count data - before being converted back to percents or proportions).

Where a 2011 LSOA is split into two (or more) 2021 LSOAs then any 2011 scores (e.g. 2019 IMD Scores) are attributed to all 2021 LSOA whereas any counts (e.g. of recipients of benefits) are split proportionately between the two 2021 LSOAs.

Where a number of 2011 LSOA merge to create a single 2021 LSOA any counts are simply added together but for scores the weighted average of 2011 LSOA scores is calculated and attributed to the 2021 LSOA.

Where there is (rarely) a more complex relationship between 2011 and 2021 LSOAs then the attribution will reflect the proportionate splits and mergers than underlie the relationship.

###Warning
It is important to remember that although the vast majority (94.1%) of 2021 LSOAs are unchanged since 2011, where changes have taken place it is because there have been significant population changes - e.g. large-scale housing developments. This obviously means that older data is likely to be of limited use simply because is describes populations which will, by definition, have changed substantially.

Thus although contemporary data that is still being published using 2011 LSOAs (e.g. benefit data available via Stat-Explore) can be attributed to 2021 LSOAs with some confidence, older data (e.g. 2019 IMD scores which are actually based on 2016 data) are likely to be of limited value.

To help contextualise the attributed data each 2021 output file includes a column stating whether the 2021 LSOA is unchanged since 2011 ('U') or is based on split ('S'), merged ('M') or more complex combinations of 2011 LSOAs ('X').
