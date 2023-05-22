library(tidyverse)
library(here)
library(sf)
library(mapview)
library(tbeptools)

data("tb03100206")

dat <- datraw %>% 
  filter(ActivityTypeCode %in% c('Sample', 'Sample-Routine')) %>% 
  select(org = OrganizationIdentifier, station = MonitoringLocationIdentifier, date = ActivityStartDate, lat = ActivityLocation.LatitudeMeasure, lon = ActivityLocation.LongitudeMeasure, 
         param = CharacteristicName, val = ResultMeasureValue, uni = ResultMeasure.MeasureUnitCode) %>% 
  filter(grepl('nitrate|nitrite', param, ignore.case = T)) %>% 
  filter(!grepl('use Inorganic nitrogen \\(NO2, NO3, \\& NH3\\)', param)) %>% 
  mutate(
    param = case_when(
      grepl('Nitrate as N', param) ~ 'Nitrate', 
      grepl('Nitrite as N', param) ~ 'Nitrite', 
      T ~ param
    ), 
    param = ifelse(param %in% c('Nitrite', 'Nitrate'), param, 'NO23')
  ) %>% 
  filter(!(is.na(lat) | is.na(lon))) %>% 
  filter(!uni == 'ug/l') %>% 
  mutate(
    uni = 'mg/L', 
    val = as.numeric(val)
  ) %>% 
  filter(!is.na(val))

# get no23 from separate nitrate, nitrite
sepn <- dat %>% 
  filter(param %in% c('Nitrate', 'Nitrite')) %>%
  summarize(
    val = mean(val, na.rm = T),
    .by = c('org', 'station', 'date', 'lat', 'lon', 'uni', 'param')
  ) %>% 
  pivot_wider(names_from = 'param', values_from = 'val') %>% 
  filter(!(is.na(Nitrate) | is.na(Nitrite))) %>% 
  mutate(
    val = Nitrate + Nitrite
  ) %>% 
  select(-Nitrate, -Nitrite) %>% 
  mutate(param = 'NO23')

dat <- dat %>% 
  filter(param %in% 'NO23') %>% 
  bind_rows(sepn) %>% 
  rename(NO23 = val) %>% 
  select(-param, -org)

write.csv(dat, here::here('data/raw/no23data.csv'), row.names = F)

# tomap <- dat %>% 
#   summarise(
#     cnt = n(), 
#     .by = c('station', 'lat', 'lon')
#   ) %>% 
#   st_as_sf(coords = c('lon', 'lat'), crs = 4326)
# 
# mapview(tomap, zcol = 'cnt')
# 
# dat %>% 
#   mutate(date = lubridate::ymd(date)) %>% 
#   filter(station == '21FLHILL_WQX-141') %>% 
#   ggplot(aes(x = date, y = NO23)) + 
#     geom_line() + 
#     geom_point() + 
#     scale_y_log10()
