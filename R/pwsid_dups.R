library(tidyverse)
library(sf)
library(mapview)
library(tbeptools)
library(readxl)
library(here)

data(tbco)

tbco <- tbco %>% 
  filter(!NAME == 'SARASOTA')

wellraw <- read_excel(here('data/raw/Chem_report_2022.xlsx'))

locraw <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/PWS/MapServer/1/query?outFields=*&where=1%3D1&f=geojson')

locdat <- locraw[tbco, ]

welldat <- wellraw %>% 
  filter(CONTAMDESC %in% c('TOTAL NITRATE+NITRITE', 'NITRATE', 'NITRITE')) %>% 
  filter(PWSID %in% locdat$PWS_ID)
  
locdat <- locdat %>% 
  filter(PWS_ID %in% welldat$PWSID) %>% 
  st_transform(crs = 6443)

# get duplicates
dups <- locdat$PWS_ID[duplicated(locdat$PWS_ID)]
locdup <- locdat[locdat$PWS_ID %in% dups, ]
dupdis <- locdup %>% 
  group_nest(PWS_ID) %>% 
  mutate(
    distkm = purrr::map(data, function(x){
      
      st_distance(x) %>% 
        units::set_units('km') %>% 
        as.numeric() %>% 
        max()
      
    })
  ) %>% 
  select(-data) %>% 
  unnest('distkm') %>% 
  arrange(-distkm)

write.csv(dupdis, here('data/raw/PWSID_dups.csv'), row.names = F)

longdis <- dupdis %>% 
  filter(distkm > 10) %>% 
  pull(PWS_ID) 
tomap <- locdat[locdat$PWS_ID %in% longdis, ] %>% 
  mutate(PWS_ID = factor(PWS_ID))



