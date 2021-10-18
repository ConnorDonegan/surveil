

## 2000 US standard million population
standard <- read.table("standard-population-us-2000.txt", 
                      comment.char = "#", 
                      header = TRUE)
usethis::use_data(standard, overwrite = TRUE)

## cancer incidence, all sites
cancer <- read.table("cancer-incidence.txt", 
                     comment.char = "#", 
                     header = TRUE)
cancer <- cancer[,c("Year", "Age.Groups", "Age.Groups.Code", "Count", "Population")]
names(cancer) <- c("Year", "Age.long", "Age", "Count", "Population")
cancer <- cancer[,c("Year", "Age", "Count", "Population")]
usethis::use_data(cancer, overwrite = TRUE)

