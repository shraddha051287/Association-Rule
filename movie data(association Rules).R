my_movies <- read.csv(file.choose())

View(my_movies)
str(my_movies)
# converting everything into character format 
my_movies[] <- lapply(my_movies,as.character)
View(my_movies)

# a single sentence 
paste_fun <- function(i){
  return (paste(as.character(i),collapse=" "))
}

# Applying the custom function

my_movies["new_col"] <- apply(my_movies,1,paste_fun)
View(my_movies)
install.packages("tm")
library(tm)
x <- Corpus(VectorSource(my_movies$new_col)) 
x <- tm_map(x,stripWhitespace)
dtm0 <- t(TermDocumentMatrix(x))
dtm0_df <- data.frame(as.matrix(dtm0))
View(dtm0_df)

# Association Rules 

library(arules)
library(arulesViz)
barplot(sapply(dtm0_df,sum),col=1:14)
# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.5,minlen=2))
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot
plot(rules,method = "graph")




