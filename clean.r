library(dplyr)
# X is the name given to the first unnamed column, which should not have any values
# if the entry has NA, the row is incorrectly formatted, and should be removed
# when X is read in as a character, the empty space will be changed to and empty string,
# so the dataset can be filtered based on its equality to an empty string
set <- read.csv("prod8.csv", colClasses=c("X"="character"))
set.filtered = dplyr::filter(set, X == "")
write.csv(set.filtered, "prod8-clean.csv")
