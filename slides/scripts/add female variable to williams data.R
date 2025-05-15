# add gender variable to williams data

library(here)
library(tidyverse)

d = read_csv(here("files/data/external_data/williams.csv"))

if(!("gender" %in% names(d))){
  set.seed(59)

  d = d %>%
    rename(PA.std = P_A.std,
           PA_lag = P_A.lag,
           NA.std = N_A.std,
           NA_lag = N_A.lag,
           id = record_id)

  d <- d[, -1]

  d = d %>%
    with_groups(id, summarise, PA = mean(PA.std)) %>%
    mutate(
      high_PA = PA > 0,
      female = map_dbl(high_PA, ~rbinom(1, 1, p=ifelse(., .7, .3)))) %>%
    select(id, female) %>%
    full_join(d)

}

write_csv(d, here("files/data/external_data/williams.csv"))
