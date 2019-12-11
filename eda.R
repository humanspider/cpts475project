library(ggplot2)
library(viridis)
library(tidyverse)
library(randomForest)
library(fastcluster)

set = read.csv("prod-merged.csv", colClasses=c("abv"="numeric","ibu"="numeric"))

set.grouped = group_by(set, style.name) %>% summarise(abv.mean=mean(abv, na.rm=T),
ibu.mean=mean(ibu,na.rm=T), srmId.mean=mean(srmId,na.rm=T), originalGravity.mean=mean(originalGravity,na.rm=T), n = n())

print(set.grouped[order(set.grouped$n, decreasing = T),], n=20)
pop_styles = c("American-Style India Pale Ale", "Imperial or Double India Pale Ale", "French & Belgian-Style Saison", "Specialty Beer", "American-Style Amber/Red Ale", "American-Style Imperial Stout", "American-Style Sour Ale", "Brown Porter", "Golden or Blonde Ale", "American-Style Stout","American-Style Brown Ale","Fruit Beer","German-Style Pilsener","Oatmeal Stout")
set.pop = filter(set, is.element(style.name, pop_styles))

pop.model = replace_na(set.pop, list(abv=mean(set.pop$abv, na.rm=T),ibu=mean(set.pop$ibu, na.rm=T),srmId=mean(set.pop$srmId, na.rm=T),originalGravity=mean(set.pop$originalGravity,na.rm=T)))
pop.model = droplevels(pop.model)

pop.model.five = filter(set, is.element(style.name, pop_styles[1:5]))
pop.model.five = replace_na(pop.model.five, list(abv=mean(pop.model.five$abv, na.rm=T),ibu=mean(pop.model.five$ibu, na.rm=T),srmId=mean(pop.model.five$srmId, na.rm=T),originalGravity=mean(pop.model.five$originalGravity,na.rm=T)))
pop.model.five = droplevels(pop.model.five)

pop.model.over200 = filter(set, is.element(style.name, pop_styles))
pop.model.over200 = replace_na(pop.model.over200, list(abv=mean(pop.model.over200$abv, na.rm=T),ibu=mean(pop.model.over200$ibu, na.rm=T),srmId=mean(pop.model.over200$srmId, na.rm=T),originalGravity=mean(pop.model.over200$originalGravity,na.rm=T)))
pop.model.over200 = droplevels(pop.model.over200)

set.model.view = select(set,id,style.name,abv,ibu,srmId,originalGravity)
set.model.view = replace_na(set.model.view, list(abv=mean(set.model.view$abv, na.rm=T),ibu=mean(set.model.view$ibu, na.rm=T),srmId=mean(set.model.view$srmId, na.rm=T),originalGravity=mean(set.model.view$originalGravity,na.rm=T)))

set.model = select(set, abv,ibu,srmId,originalGravity)
set.model = replace_na(set.model, list(abv=mean(set.model$abv, na.rm=T),ibu=mean(set.model$ibu, na.rm=T),srmId=mean(set.model$srmId, na.rm=T),originalGravity=mean(set.model$originalGravity,na.rm=T)))

set.model.2 = select(set, abv,ibu)
set.model.2 = replace_na(set.model.2, list(abv=mean(set.model.2$abv, na.rm=T),ibu=mean(set.model.2$ibu, na.rm=T)))

# graphs
ggplot(pop.model, aes(style.shortName, fill=style.name)) + geom_bar() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + scale_fill_viridis(discrete=T,option="C")

set.model.forest = replace_na(set, list(abv=mean(set$abv, na.rm=T),ibu=mean(set$ibu, na.rm=T),srmId=mean(set$srmId, na.rm=T),originalGravity=mean(set$originalGravity,na.rm=T)))

ggplot(set.pop, aes(x=abv, y=ibu, colour=style.name)) + geom_point() + labs(fill="style")

ggplot(set.grouped, aes(x=abv.mean,y=ibu.mean, colour=style.name)) + geom_point()

# kmeans
beerCluster = kmeans(set.model, centers=10, iter.max=25)
table(set.model.view$style.name,beerCluster$cluster) # view number of beers of each type in each cluster
beerCluster$cluster = as.factor(beerCluster$cluster)
ggplot(set.model, aes(abv, ibu, colour=beerCluster$cluster)) + geom_point()

# randomForest
output.forest = randomForest(style.name ~ abv + ibu + srmId + originalGravity,data=pop.model)

# knn
library(class)
library(R.utils)
accuracy = function(x){sum(diag(x)/sum(rowSums(x))) * 100}
train.index = sample(nrow(pop.model.over200), nrow(pop.model.over200) * .9) # creates a set of indices
train.features = select(pop.model.over200, style.name, abv, ibu, srmId, originalGravity)
train.knn = train.features[train.index,]
test.knn = train.features[-train.index,]
train.knn.cat = train.knn$style.name
test.knn.cat = test.knn$style.name
# drop style.name from features
train.knn$style.name = NULL
test.knn$style.name = NULL
# insert optional normalization here scale()
for(k in 2:17) {
  knn.model = knn(train.knn, test.knn, cl=train.knn.cat,k=k)
  tab = table(knn.model, test.knn.cat)
  printf("k=%d, %f\n",k,accuracy(tab))
}

