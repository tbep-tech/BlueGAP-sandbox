library(tidyverse)
library(here)

# raw -----------------------------------------------------------------------------------------

# raw data from WQP: https://www.waterqualitydata.us/
# selected US, Virgin Islands, St. Croix, NWIS, STEWARDS, WQX, selected sample results
datraw <- read.csv('~/Desktop/resultphyschem.csv')

save(datraw, file = here('data/datraw.RData'))

# processed -----------------------------------------------------------------------------------

load(file = here('data/datraw.RData'))

datpro <- datraw %>% 
  filter(ActivityTypeCode %in% c('Field Msr/Obs', 'Sample', 'Sample-Other', 'Sample-Routine')) %>% 
  filter(grepl('nitra|nitro|nitrite|ammo', CharacteristicName, ignore.case = TRUE)) %>% 
  filter(!is.na(ActivityLocation.LatitudeMeasure) & !is.na(ActivityLocation.LongitudeMeasure)) %>% 
  select(
    OrganizationIdentifier, 
    OrganizationFormalName, 
    ActivityIdentifier,
    ActivityTypeCode,
    ActivityMediaSubdivisionName,
    ActivityStartDate, 
    ActivityDepthHeightMeasure.MeasureValue,
    ActivityTopDepthHeightMeasure.MeasureUnitCode,
    ProjectName, 
    ActivityLocation.LatitudeMeasure, 
    ActivityLocation.LongitudeMeasure,
    CharacteristicName, 
    ResultMeasureValue, 
    ResultMeasure.MeasureUnitCode, 
    MeasureQualifierCode, 
    ResultDetectionConditionText
  )

save(datpro, file = here('data/datpro.RData'))