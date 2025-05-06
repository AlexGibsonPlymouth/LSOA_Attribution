
# Linking2011and2021StatisticalGeographies.R 

library(tidyverse) # contains dplyr, ggplot2, readr, tidyr, stringr, purrr and forcats

# Set & check Working Directory <  THIS WILL NEED TO BE SET AS APPROPRIATE, WITH FILES BELOW DOWN LOADED TO THIS DIRECTORY
setwd()
getwd()

# Read in Census 21 Postcode Resident Estimates
# Downloaded from https://www.nomisweb.co.uk/sources/census_2021_pc
PostcodeResidents = read.csv("./pcd_p001.csv")
head(PostcodeResidents)

# We are only interested in persons (not males & females), so sum to postcode
PostcodeResidents = PostcodeResidents %>% group_by(Postcode) %>% summarise(Persons = sum(Count))
head(PostcodeResidents)

# Read in Census 21 Postcode Resident Estimates
# Download from https://www.nomisweb.co.uk/sources/census_2021_pc
PostcodeHouseholds = read.csv("./pcd_p002.csv")
head(PostcodeHouseholds)


# Read in the ONSPD for August 2024 (which has 2011 and 2021 statistical geographies)
# Download from https://geoportal.statistics.gov.uk/datasets/265778cd85754b7e97f404a1c63aea04/about
ONSPD = read.csv("./ONSPD_AUG_2024/Data/ONSPD_AUG_2024_UK.csv")
names(ONSPD)

# Extract just the postcode, OA11, OA21, LSOA11, LSOA21, MSOA11 and MSOA21
ONSPDExtract = ONSPD %>% select(pcd,pcd2,pcds,oa11,oa21,lsoa11,lsoa21,msoa11,msoa21)
names(ONSPDExtract)

# Now for each postcode in PostcodeResidents attach the statistical geographies
names(PostcodeResidents)[1] = "pcds"
nrow(PostcodeResidents)  # 1377463
PopData = PostcodeResidents %>% left_join(ONSPDExtract, by = "pcds")
head(PopData)

PopData %>% filter(is.na(oa11))
PopData %>% filter(is.na(lsoa11))
PopData %>% filter(is.na(msoa11))
PopData %>% filter(is.na(oa21))
PopData %>% filter(is.na(oa11))
PopData %>% filter(is.na(lsoa11))
PopData %>% filter(is.na(msoa11))
# so looks like every postcode found OK
nrow(PopData) # 1377463 

# There are a few postcodes that are in Scotland (and thus Scottish statistical geographies), so remove
PopData = PopData %>% mutate(Country = substr(oa11,1,1)) %>% filter(Country == "E" | Country == "W")
PopData %>% group_by(oa11) %>% summarise(Count = n()) %>% select(oa11) %>% pull(.) %>% length(.) # 181,317 (should be 181,408)
PopData %>% group_by(lsoa11) %>% summarise(Count = n()) %>% select(lsoa11) %>% pull(.) %>% length(.) # 34753 (should be 34753)
PopData %>% group_by(msoa11) %>% summarise(Count = n()) %>% select(msoa11) %>% pull(.) %>% length(.) # 7201 (should be 7201)
PopData %>% group_by(oa21) %>% summarise(Count = n()) %>% select(oa21) %>% pull(.) %>% length(.) # 188,842 (should be 188,880)
PopData %>% group_by(lsoa21) %>% summarise(Count = n()) %>% select(lsoa21) %>% pull(.) %>% length(.) # 35672 (should be 35672)
PopData %>% group_by(msoa21) %>% summarise(Count = n()) %>% select(msoa21) %>% pull(.) %>% length(.) # 7264 (should be 7264)

# So it does look as if there are some OAs which, by this method, have no populations.  I should check this makes sense!

head(PopData)



# LSOAs ##############################################################################################################
# Select PopData cols we want and create the unique link code
LSOAPopDataEngland = PopData %>% filter(Country == "E") %>% select(pcds,Persons,lsoa11,lsoa21) %>% mutate(LinkCode = paste0(lsoa11,"_",lsoa21))
head(LSOAPopDataEngland)

# Group by link code and get counts of persons 
LSOAPopDataEngland = LSOAPopDataEngland %>% group_by(LinkCode) %>% summarise(TotPersons = sum(Persons), 
                                                                          LSOA11 = first(lsoa11), LSOA21 = first(lsoa21))
head(LSOAPopDataEngland)

# Calculate ratios of LSOA21 and add to LinkDataDF
LSOA11Sums = LSOAPopDataEngland %>% group_by(LSOA11) %>% summarise(LSOA11Denom = sum(TotPersons))
LSOA21Sums = LSOAPopDataEngland %>% group_by(LSOA21) %>% summarise(LSOA21Denom = sum(TotPersons))
LSOAPopDataEngland = LSOAPopDataEngland %>% left_join(LSOA11Sums, by = "LSOA11") %>% left_join(LSOA21Sums, by = "LSOA21")

LSOAPopDataEngland = LSOAPopDataEngland %>% mutate(Prop11of21 = TotPersons/LSOA21Denom) %>% mutate(Prop21of11 = TotPersons/LSOA11Denom) %>%
  select(LSOA11,LSOA21,LinkCode,TotPersons,LSOA11Denom,LSOA21Denom,Prop11of21,Prop21of11) %>% arrange(LSOA11)
names(LSOAPopDataEngland)[1:2] = c("LSOA11CD","LSOA21CD")

# If Prop11of21 != 1 then LSOA11s join to make LSOA21
head(LSOAPopDataEngland %>% filter(Prop11of21 != 1) %>% arrange(LSOA21CD))

# If Prop21of11 != 1 then LSOA11 splits to make LSOA21
head(LinkDataDF %>% filter(Prop21of11 != 1) %>% arrange(LSOA11CD))

# If Prop11of21 != 1 and Prop21of11 !=1 then it is complex, and includes any other instances of any of the LSOA11CDs or LSOA21CDs that appear
head(LinkDataDF %>% filter(Prop11of21 != 1 & Prop21of11 != 1) %>% arrange(LSOA11CD))

# Start by finding and extracting all complex
Complex1 = LSOAPopDataEngland %>% filter(Prop11of21 != 1 & Prop21of11 != 1) %>% arrange(LSOA11CD)
nrow(Complex1) # 28
ComplexLSOA11 = Complex1 %>% select(LSOA11CD) %>% pull(.)
ComplexLSOA21 = Complex1 %>% select(LSOA21CD) %>% pull(.)

AllComplex = LSOAPopDataEngland %>% filter(LSOA11CD %in% ComplexLSOA11 | LSOA21CD %in% ComplexLSOA21)
nrow(AllComplex) # 68

AllNotComplex = LSOAPopDataEngland %>% filter(!LSOA11CD %in% ComplexLSOA11 & !LSOA21CD %in% ComplexLSOA21)
nrow(AllNotComplex) # 33808

# Check
nrow(LSOAPopDataEngland)-nrow(AllComplex)-nrow(AllNotComplex) # 0 > good!

# So all in AllNotComplex are unchanged, merges or joins
AllNotComplex %>% filter(Prop11of21 != 1) %>% arrange(LSOA21CD)


# Now want to add Category = U, M or S to the AllNotComplex & = X to the Complex dataset, and then merge and save
print(AllNotComplex, n=30)
AllNotComplex = AllNotComplex %>% mutate(Category = case_when(Prop11of21 == 1 & Prop21of11 ==1 ~ "U",
                                                              Prop11of21 == 1 & Prop21of11 < 1 ~ "S",
                                                              Prop11of21 < 1 & Prop21of11 == 1 ~ "M",
                                                        .default = "NotFound"))
AllNotComplex %>% filter(Category == "Not Found")
head(AllNotComplex %>% filter(Category == "U"))
head(AllNotComplex %>% filter(Category == "S"))
head(AllNotComplex %>% filter(Category == "M") %>% arrange(LSOA21CD))

AllComplex = AllComplex %>% mutate(Category = "X")
LSOAPopDataEngland = rbind(AllNotComplex,AllComplex)  
LSOAPopDataEngland = LSOAPopDataEngland %>% arrange(LSOA11CD)
nrow(LSOAPopDataEngland)
length(unique(LSOAPopDataEngland$LSOA11CD)) # 32644 as expected
length(unique(LSOAPopDataEngland$LSOA21CD)) # 33755 as expected

write.csv(LSOAPopDataEngland,"./LSOA11_LSOA21PopLookups.csv", row.names = FALSE)





######################################################################################################################################


# Now for each postcode in PostcodeHouseholds attach the statistical geographies
names(PostcodeHouseholds)[1] = "pcds"
HHData = PostcodeHouseholds %>% left_join(ONSPDExtract, by = "pcds")
head(HHData)

HHData = HHData %>% mutate(Country = substr(oa11,1,1)) %>% filter(Country == "E" | Country == "W")
HHData %>% group_by(oa11) %>% summarise(Count = n()) %>% select(oa11) %>% pull(.) %>% length(.) # 181,292 (should be 181,408)
HHData %>% group_by(lsoa11) %>% summarise(Count = n()) %>% select(lsoa11) %>% pull(.) %>% length(.) # 34753 (should be 34753)
HHData %>% group_by(msoa11) %>% summarise(Count = n()) %>% select(msoa11) %>% pull(.) %>% length(.) # 7201 (should be 7201)
HHData %>% group_by(oa21) %>% summarise(Count = n()) %>% select(oa21) %>% pull(.) %>% length(.) # 188,842 (should be 188,880)
HHData %>% group_by(lsoa21) %>% summarise(Count = n()) %>% select(lsoa21) %>% pull(.) %>% length(.) # 35672 (should be 35672)
HHData %>% group_by(msoa21) %>% summarise(Count = n()) %>% select(msoa21) %>% pull(.) %>% length(.) # 7264 (should be 7264)

head(HHData)

# Select HHData cols we want and create the unique link code
LSOAHHDataEngland = HHData %>% filter(Country == "E") %>% select(pcds,Count,lsoa11,lsoa21) %>% mutate(LinkCode = paste0(lsoa11,"_",lsoa21))
head(LSOAHHDataEngland)
LSOAHHDataEngland = LSOAHHDataEngland %>% group_by(LinkCode) %>% summarise(Households = sum(Count), 
                                                                             LSOA11 = first(lsoa11), LSOA21 = first(lsoa21))
head(LSOAHHDataEngland)
write.csv(LSOAHHDataEngland,"./LSOA11_LSOA21Households.csv", row.names = FALSE)


# Calculate ratios of LSOA21 and add to LinkDataDF
LSOA11Sums = LSOAHHDataEngland %>% group_by(LSOA11) %>% summarise(LSOA11Denom = sum(Households))
LSOA21Sums = LSOAHHDataEngland %>% group_by(LSOA21) %>% summarise(LSOA21Denom = sum(Households))
LSOAHHDataEngland = LSOAHHDataEngland %>% left_join(LSOA11Sums, by = "LSOA11") %>% left_join(LSOA21Sums, by = "LSOA21")

LSOAHHDataEngland = LSOAHHDataEngland %>% mutate(Prop11of21 = Households/LSOA21Denom) %>% mutate(Prop21of11 = Households/LSOA11Denom) %>%
  select(LSOA11,LSOA21,LinkCode,Households,LSOA11Denom,LSOA21Denom,Prop11of21,Prop21of11) %>% arrange(LSOA11)
names(LSOAHHDataEngland)[1:2] = c("LSOA11CD","LSOA21CD")

# If Prop11of21 != 1 then LSOA11s join to make LSOA21
head(LSOAHHDataEngland %>% filter(Prop11of21 != 1) %>% arrange(LSOA21CD))
LSOAHHDataEngland %>% filter(LSOA11CD == "E01000954")

# If Prop21of11 != 1 then LSOA11 splits to make LSOA21
head(LSOAHHDataEngland %>% filter(Prop21of11 != 1) %>% arrange(LSOA11CD))

# If Prop11of21 != 1 and Prop21of11 !=1 then it is complex, and includes any other instances of any of the LSOA11CDs or LSOA21CDs that appear
head(LSOAHHDataEngland %>% filter(Prop11of21 != 1 & Prop21of11 != 1) %>% arrange(LSOA11CD))

# Start by finding and extracting all complex
Complex1 = LSOAHHDataEngland %>% filter(Prop11of21 != 1 & Prop21of11 != 1) %>% arrange(LSOA11CD)
nrow(Complex1) # 28
ComplexLSOA11 = Complex1 %>% select(LSOA11CD) %>% pull(.)
ComplexLSOA21 = Complex1 %>% select(LSOA21CD) %>% pull(.)

AllComplex = LSOAHHDataEngland %>% filter(LSOA11CD %in% ComplexLSOA11 | LSOA21CD %in% ComplexLSOA21)
nrow(AllComplex) # 68

AllNotComplex = LSOAHHDataEngland %>% filter(!LSOA11CD %in% ComplexLSOA11 & !LSOA21CD %in% ComplexLSOA21)
nrow(AllNotComplex) # 33808

# Check
nrow(LSOAHHDataEngland)-nrow(AllComplex)-nrow(AllNotComplex) # 0 > good!

# So all in AllNotComplex are unchanged, merges or joins
AllNotComplex %>% filter(Prop11of21 != 1) %>% arrange(LSOA21CD)


# Now want to add Category = U, M or S to the AllNotComplex & = X to the Complex dataset, and then merge and save
print(AllNotComplex, n=30)
AllNotComplex = AllNotComplex %>% mutate(Category = case_when(Prop11of21 == 1 & Prop21of11 ==1 ~ "U",
                                                              Prop11of21 == 1 & Prop21of11 < 1 ~ "S",
                                                              Prop11of21 < 1 & Prop21of11 == 1 ~ "M",
                                                              .default = "NotFound"))
AllNotComplex %>% filter(Category == "Not Found")
head(AllNotComplex %>% filter(Category == "U"))
head(AllNotComplex %>% filter(Category == "S"))
head(AllNotComplex %>% filter(Category == "M") %>% arrange(LSOA21CD))

AllComplex = AllComplex %>% mutate(Category = "X")
LSOAHHDataEngland = rbind(AllNotComplex,AllComplex)  
LSOAHHDataEngland = LSOAHHDataEngland %>% arrange(LSOA11CD)
nrow(LSOAHHDataEngland)
length(unique(LSOAHHDataEngland$LSOA11CD)) # 32844 as expected
length(unique(LSOAHHDataEngland$LSOA21CD)) # 33755 as expected

write.csv(LSOAHHDataEngland,"./LSOA11_LSOA21HHLookups.csv", row.names = FALSE)







# NOT DONE BELOW <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<







# MSOAs ##############################################################################################################
# Select cols we want and create the unique link code
# Select PopData cols we want and create the unique link code
MSOAPopDataEngland = PopData %>% filter(Country == "E") %>% select(pcds,Persons,msoa11,msoa21) %>% mutate(LinkCode = paste0(msoa11,"_",msoa21))
head(MSOAPopDataEngland)

# Group by link code and get counts of persons 
MSOAPopDataEngland = MSOAPopDataEngland %>% group_by(LinkCode) %>% summarise(TotPersons = sum(Persons), 
                                                                             MSOA11 = first(msoa11), MSOA21 = first(msoa21))
head(MSOAPopDataEngland)
write.csv(MSOAPopDataEngland,"./MSOA11_MSOA21Populations.csv", row.names = FALSE)


# Select HHData cols we want and create the unique link code
MSOAHHDataEngland = HHData %>% filter(Country == "E") %>% select(pcds,Count,msoa11,msoa21) %>% mutate(LinkCode = paste0(msoa11,"_",msoa21))
head(MSOAHHDataEngland)
MSOAHHDataEngland = MSOAHHDataEngland %>% group_by(LinkCode) %>% summarise(Households = sum(Count), 
                                                                           MSOA11 = first(msoa11), MSOA21 = first(msoa21))
head(MSOAHHDataEngland)
write.csv(MSOAHHDataEngland,"./MSOA11_MSOA21Households.csv", row.names = FALSE)

