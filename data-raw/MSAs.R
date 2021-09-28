library(tidyverse)

df <- data.table::fread("/home/connor/repo/cancer-ts/data/msa-crc-incidence-50-79.txt", fill = T)
df <- df %>%
    dplyr::filter(
               !is.na(Year) &
               Count != "Suppressed" &
          #     Race %in% racial.groups &
               MSA != "" &
               Population != "Not Applicable" &
               !is.na(Population)
    ) %>%
    mutate(
        Race = ifelse(Ethnicity == "Hispanic", "Hispanic", Race),
        Count = as.numeric(Count),
        Population = as.numeric(Population)
    ) %>%
    group_by(Race, MSA, Year) %>%
    summarise(Count = sum(Count),
              Population = sum(Population)) %>%
    ungroup %>%
    select(Year, Race, MSA, Count, Population)

## time periods per group: 19 each, but only white, black, hispanic
df %>%
    group_by(Race, MSA) %>%
    summarise(n = n())

cat("Check data dimensions, grouping, after removal of suppressed counts\n")
df %>%
    group_by(MSA) %>%
    summarise(
        groups = n_distinct(Race),
        years = n_distinct(Year),
        observations = n()
    ) %>%
    print()

msa <- df

## make availabe to surveil users
usethis::use_data(msa, overwrite = TRUE)
