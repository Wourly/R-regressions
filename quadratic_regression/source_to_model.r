rm(list=ls())

elements <- read.csv('source.csv')

elements <- elements[complete.cases(elements), ]

save(elements, file="model_input.RData")

rm(elements)