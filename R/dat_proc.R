library(tidyverse)
library(here)
library(readxl)
library(sf)

sf_use_s2(FALSE)

# raw -----------------------------------------------------------------------------------------

# raw data from WQP: https://www.waterqualitydata.us/
# selected US, Virgin Islands, St. Croix, NWIS, STEWARDS, WQX, selected sample results
datraw <- read.csv('~/Desktop/resultphyschem.csv')

save(datraw, file = here('data/datraw.RData'), compress = 'xz')

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

# dat raw from FR email 11/17 -----------------------------------------------------------------

# these are from Sennai, apparently just from STORET
datraw2 <- read_excel('~/Desktop/DEP_AWQ_alldata_11_2021.xlsx')

save(datraw2, file = here('data/datraw2.RData'))

locraw2 <- read_excel('~/Desktop/DEP_AWQ_Station_location.xlsx')

save(locraw2, file = here('data/locraw2.RData'))

# dat processed from FR email 11/17 -----------------------------------------------------------

load(file = here('data/datraw2.RData'))
load(file = here('data/locraw2.RData'))

locraw2 <- locraw2 %>% 
  select(
    MonitoringLocationIdentifier, 
    ActivityLocation.LatitudeMeasure = LatitudeMeasure, 
    ActivityLocation.LongitudeMeasure = LongitudeMeasure
    ) %>% 
  unique

datpro2 <- datraw2 %>% 
  filter(ActivityTypeCode %in% c('Field Msr/Obs', 'Sample', 'Sample-Other', 'Sample-Routine')) %>% 
  filter(grepl('nitra|nitro|nitrite|ammo', CharacteristicName, ignore.case = TRUE)) %>% 
  select(
    OrganizationIdentifier, 
    OrganizationFormalName, 
    ActivityIdentifier,
    ActivityTypeCode,
    ActivityMediaSubdivisionName,
    ActivityStartDate, 
    ActivityDepthHeightMeasure.MeasureValue = `ActivityDepthHeightMeasure/MeasureValue`,
    ActivityTopDepthHeightMeasure.MeasureUnitCode = `ActivityTopDepthHeightMeasure/MeasureUnitCode`,
    CharacteristicName, 
    ResultMeasureValue, 
    ResultMeasure.MeasureUnitCode = `ResultMeasure/MeasureUnitCode`, 
    MeasureQualifierCode, 
    ResultDetectionConditionText, 
    MonitoringLocationIdentifier
  ) %>% 
  left_join(locraw2, by = 'MonitoringLocationIdentifier') %>% 
  select(-MonitoringLocationIdentifier)

save(datpro2, file = here('data/datpro2.RData'))

# tb counties ---------------------------------------------------------------------------------

tbco <- st_read(here('data/raw/CountyFDEP.shp')) %>% 
  st_simplify(dTolerance = 10) %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  filter(NAME %in% c('HILLSBOROUGH', 'PINELLAS', 'SARASOTA', 'MANATEE', 'PASCO', 'POLK'))
save(tbco, file = here('data/tbco.RData'), compress = 'xz')

# tb counties clipped by shed plus bay --------------------------------------------------------

load(file = here('data/tbco.RData'))

tbcos <- st_intersection(tbshed, tbco) %>% 
  select(-Acres)

tbshederase <- st_combine(tbcos) %>% 
  st_make_valid() %>% 
  st_difference(tbshed, .) %>% 
  st_cast('POLYGON') %>% 
  mutate(
    area = st_area(.)
  ) %>% 
  filter(area == max(area)) %>% 
  mutate(
    NAME = 'TAMPA BAY'
  ) %>% 
  select(-area, -Acres)

tbshedcos <- bind_rows(tbcos, tbshederase)
row.names(tbshedcos) <- 1:nrow(tbshedcos)

save(tbshedcos, file = here('data/tbshedcos.RData'), compress = 'xz')

