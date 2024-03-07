library(tidyverse)
library(here)
library(readxl)
library(sf)
library(tbeptools)

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

# tb shed zip codes ---------------------------------------------------------------------------

# url with zip gdb to download
urlin <- 'https://www2.census.gov/geo/tiger/GENZ2018/kml/cb_2018_us_zcta510_500k.zip'

# download file
tmp1 <- tempfile(fileext = ".zip")
download.file(url = urlin, destfile = tmp1)

# unzip file
tmp2 <- tempdir()
utils::unzip(tmp1, exdir = tmp2)

# file path
pth <- list.files(tmp2, pattern = '\\.kml$', full.names = T)
pth <- gsub('\\\\', '/', pth)

# read the layer
zip <- st_read(pth) %>% 
  st_make_valid()

# remove temp files
unlink(tmp1, recursive = TRUE)
unlink(pth, recursive = TRUE)

tbzip <- st_intersection(tbshed, zip) %>% 
  select(-Description, -Acres)

tbshederase <- st_combine(tbzip) %>% 
  st_make_valid() %>% 
  st_difference(tbshed, .) %>% 
  st_cast('POLYGON') %>% 
  mutate(
    area = st_area(.)
  ) %>% 
  filter(area == max(area)) %>% 
  mutate(
    Name = 'TAMPA BAY'
  ) %>% 
  select(-area, -Acres)

tbshedzip <- bind_rows(tbzip, tbshederase) %>% 
  mutate(Name = gsub('<at>|<openparen>|<closeparen>', '', Name))
row.names(tbshedzip) <- 1:nrow(tbshedzip)

save(tbshedzip, file = here('data/tbshedzip.RData'))

# census tracts -------------------------------------------------------------------------------

load(file = url('https://github.com/tbep-tech/equity-plan-mapping/raw/main/data/tb_tract.RData'))

tbtra <- tb_tract %>% 
  select(Name = ID)

tbseguni <- st_union(tbseg) %>% 
  st_sf() %>% 
  mutate(Name = 'Tampa Bay')

tbtra <- st_difference(tbtra, tbseguni) %>% 
  select(-Name.1) %>% 
  rename(geometry = Shape)

tbshedtra <- bind_rows(tbtra, tbseguni)
row.names(tbshedtra) <- 1:nrow(tbshedtra)

save(tbshedtra, file = here('data/tbshedtra.RData'))

# all epc data --------------------------------------------------------------------------------

# file path
xlsx <- 'dat.xlsx'

# load and assign to object
epcall <- read_importepc(xlsx, download_latest = T)

save(epcall, file = here('data/epcall.RData'))

file.remove(xlsx)
