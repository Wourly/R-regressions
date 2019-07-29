rm(list=ls())

plants_raw <- read.csv('source_data.csv')
plants_raw <- data.frame(plants_raw$X, plants_raw$pdias, plants_raw$longindex)

#removing NA cases
plants_raw <- plants_raw[complete.cases(plants_raw), ]
plants_final <- data.frame()

raw_i <- 0
final_i <- 0

plants_raw_length <- nrow(plants_raw)

#removing cases with 0 longevity index and rare weights (above 4mg)
while (raw_i < plants_raw_length)
{
  raw_i <- raw_i + 1

  if (!(plants_raw[raw_i, 3] == 0 | plants_raw[raw_i,2] > 3))
  {
    final_i <- final_i + 1
    plants_final[final_i, 1] <- plants_raw[raw_i, 1]
    plants_final[final_i, 2] <- plants_raw[raw_i, 2]
    plants_final[final_i, 3] <- plants_raw[raw_i, 3]
  }
  #raw_i
}

rm(raw_i, final_i, plants_raw_length, plants_raw)

names(plants_final)<-c('Name', 'Diaspore', 'Longevity')

save(plants_final, file = 'model_input.RData')

rm(plants_final)