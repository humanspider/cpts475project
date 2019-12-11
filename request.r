library(rjson)
library(plyr)
library(R.utils)

sandbox.address <- "https://api.brewerydb.com/v2/"
sandbox.key <- "key=449848915d61a2ed9d214925170caa19"
beersPath <- "/beers/"

start_page = 3
num_pages = 200
first_write = TRUE
path = paste(beersPath,"?",sandbox.key,sep="")
url = paste(sandbox.address,path,sep="")
url.np = url
url = paste(url.np,"&p=",start_page) # use this line if you are starting at page > 1

resp <- fromJSON(file=url)

pages <- resp$numberOfPages

# col.names <= c("id","name", "nameDisplay","description","foodPairings","originalGravity","abv","ibu","isRetired","glasswareId",
#                "glass","styleId","style.id","style.categoryId","style.category.id","style.category.name","style.name","style.description",
#                "style.ibuMin","style.ibuMax","style.abvMin","style.abvMax","style.srmMin","style.srmMax","style.ogMin","style.fgMin",
#                "style.fgMax","style.createDate","style.updateData","isOrganic","labels.icon","labels.medium","labels.large",
#                "labels.contentAwareIcon","labels.contentAwareMedium","labels.contentAwareLarge")

# counter for number of pages in the in-memory chunk
k = 0

# set chunk as header from data you are appending to
# chunk <- read.csv("prod3-clean.csv", sep=",", nrows=1)
# chunk <- chunk[FALSE,]

for(i in seq(from=start_page, to=((num_pages*5)+start_page), by=5)) {
  url = paste(url.np,"&p=",i)
  resp <- fromJSON(file=url)
  data <- resp$data
  
  
  if(!exists("chunk") ) {
    chunk <- as.data.frame(data[1])
    for(j in 2:length(data)) {
      chunk <- rbind.fill(chunk, as.data.frame(data[j]))
    }
  } else {
    for(j in 1:length(data)) {
      chunk <- rbind.fill(chunk, as.data.frame(data[j]))
    }
  }
  
  k = k + 1

  printf("page %d\n",i)

  if(k >= 35) {
    if(first_write) {
      write.table(chunk, file="prod8.csv", append=TRUE, sep=",")
      first_write = FALSE
    } else {
      write.table(chunk, file="prod8.csv", append=TRUE, sep=",", col.names = FALSE) 
    }
    chunk = chunk[FALSE,]
    # reset page counter
    k = 0
  }
}

if(first_write) {
  write.table(chunk, file="prod8.csv", append=TRUE, sep=",")
} else {
  write.table(chunk, file="prod8.csv", append=TRUE, sep=",", col.names = FALSE)
}