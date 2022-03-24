#install.packages("auk")
setwd("C:\\Users\\Kayle\\AppData\\Local\\Temp\\RtmpuI0Z44\\downloaded_packages")
library(auk)
states <- c("US-MD", "CR-P")
system.file("extdata/ebd-sample.txt", package = "auk") %>%
  auk_ebd() %>%
  auk_state(states)
ebd <- auk_ebd(system.file("extdata/ebd-sample.txt", package = "auk"))
auk_distance(ebd, distance = c(0, 10))
system.file("extdata/ebd-sample.txt", package = "auk") %>%
  auk_ebd() %>%
  auk_distance(distance = c(0, 10))

f <- system.file("extdata/ebd-sample.txt", package = "auk")
ebd <- auk_ebd(f) %>% 
  auk_species(species = c("Blue Jay", "Cyanocitta stelleri")) %>%
  auk_country(country = c("US", "Canada", "mexico")) %>%
  auk_bbox(bbox = c(-100, 37, -80, 52)) %>%
  auk_date(date = c("2012-01-01", "2012-12-31")) %>%
  auk_time(start_time = c("06:00", "09:00")) %>%
  auk_duration(duration = c(0, 60)) %>%
  auk_complete()
ebd
system.file("extdata/ebd-sample.txt", package = "auk") %>% 
  read_ebd() %>% 
  str()

#Hypothesis:
#On average Blue Jay's live longer on the East Coast than the West Coast because there is decreased population of Blue Jay's on the East Coast in comparison to the population of Blue Jay's on the West Coast. 