library(tidyverse)
library(lubridate)

# https://www.waterqualitydata.us/#huc=03100206&sampleMedia=Water&characteristicType=Nutrient&mimeType=csv&dataProfile=resultPhysChem&providers=STORET
# https://water.usgs.gov/wsc/acc/031002.html

datraw <- read.csv('~/Desktop/resultphyschem.csv')
save(datraw, file = 'data/tb03100206.RData', compress = 'xz')

dat <- datraw %>% 
  select(org = OrganizationFormalName, date = ActivityStartDate, MonitoringLocationIdentifier,
         CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode) %>% 
  mutate(
    date = ymd(date), 
    org = case_when(
      grepl('Florida Lakewatch', org, ignore.case = T) ~ 'Florida LAKEWATCH', 
      grepl('^Pinellas County', org) ~ 'Pinellas County', 
      grepl('^Manatee County', org) ~ 'Manatee County', 
      grepl('^Environmental Protection Commission', org) ~ 'EPCHC', 
      grepl('^FL Dept\\. Environ', org) ~ 'FLDEP',
      T ~ org
    )
  ) %>% 
  filter(grepl('nitr', CharacteristicName, ignore.case = T)) 

toplo <- dat %>% 
  select(org, date) %>% 
  unique() %>% 
  mutate(
    cnt = n(), 
    .by = 'org'
  )
  
ggplot(toplo, aes(y = reorder(org, cnt), x = date)) + 
  geom_point() + 
  labs(
    x = NULL, 
    y = NULL, 
    title = 'Dates with nitrogen samples'
  )

flt <- toplo %>% 
  select(org, cnt) %>% 
  unique() %>% 
  arrange(-cnt) %>% 
  pull(org) %>% 
  .[1:5]

toplo2 <- dat %>% 
  select(org, CharacteristicName) %>% 
  filter(org %in% flt) %>% 
  summarise(
    cnt = n(), 
    .by = c('org', 'CharacteristicName')
  )

ggplot(toplo2, aes(y = reorder(CharacteristicName, cnt), x = cnt)) + 
  geom_col() + 
  facet_wrap(~org, scales = 'free_y', ncol = 2) + 
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 8)
  ) + 
  labs(
    y = NULL, 
    x = 'Count', 
    title = 'Parameter count by top five'
  )
