library(tidyverse)
library(lubridate)

# https://www.waterqualitydata.us/#huc=03100206&sampleMedia=Water&characteristicType=Nutrient&mimeType=csv&dataProfile=resultPhysChem&providers=STORET
# https://water.usgs.gov/wsc/acc/031002.html

# datraw <- read.csv('~/Desktop/resultphyschem.csv')
# save(datraw, file = 'data/tb03100206.RData', compress = 'xz')

load(file = 'data/tb03100206.RData')

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
  select(org, CharacteristicName, MonitoringLocationIdentifier, date) %>%
  summarise(
    nobs = n(), 
    nstation = length(unique(MonitoringLocationIdentifier)),
    mindate = min(date), 
    maxdate = max(date),
    .by = c('org', 'CharacteristicName')
  ) %>% 
  arrange(org, CharacteristicName)

# summary for ibrahim
write.csv(toplo2, here::here('data/raw/tb03100206.csv'), row.names = F)

toplo2 <- toplo2 %>% 
  filter(org %in% flt)

ggplot(toplo2, aes(y = reorder(CharacteristicName, nobs), x = nobs)) + 
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
