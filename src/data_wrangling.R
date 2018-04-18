library(tidyverse)
library(plyr)

t2dsci_data_wrangling <- function (data_location) {
  raw <- read_csv(data_location)
  
  # remove timestamps and consent
  data <- raw %>% dplyr::select(-Timestamp, -`I agree to the above consent form.`)
  
  # rename columns
  colnames(data) <- c('X', 'Y', 'C1', 'C2', 'C3')
  
  # mappping for X
  maps_X <- tibble(raw=c('I had experience with statistics.',
                         'I had experience with programming.',
                         'I had experience with both programming and statistics.',
                         'I had no more than entry level knowledge of either topic.'),
                   code=c('stats', 'prog', 'both', 'neither'))
  # mapping for Y
  maps_Y <- tibble(raw=c('Easy', 'Somewhat easy', 'Medium',
                         'Somewhat hard', 'Hard'), code=1:5)
  # mapping for C1
  maps_C1 <- tibble(raw=c('<1', '1-3', '3-5', '+5'), code=1:4)
  # mapping for C2
  maps_C2 <- tibble(raw=c('Yes', 'No'), code=c(TRUE, FALSE))
  # mapping for C3
  maps_C3 <- tibble(raw=c('Yes', 'No'), code=c(TRUE, FALSE))
  
  # convert X, C2 and C3
  data$X <- mapvalues(data$X, maps_X$raw, maps_X$code)
  data$C2 <- as.logical(mapvalues(data$C2, maps_C2$raw, maps_C2$code))
  data$C3 <- as.logical(mapvalues(data$C3, maps_C3$raw, maps_C3$code))
  
  # relevel data to align with orders in the survey
  data$X = data$X %>% fct_relevel('stats', 'prog', 'both', 'neither')
  data$C1 <- data$C1 %>% fct_relevel('<1', '1-3', '3-5', '+5')
  data$Y <- data$Y %>% fct_relevel('Easy', 'Somewhat easy',
                                   'Medium', 'Somewhat hard', 'Hard')
  data
}
