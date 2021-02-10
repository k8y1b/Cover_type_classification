train <- read.csv("train.csv")
test <- read.csv("test.csv")

varn = names(train)

varnxs = varn[c(1:15, 56)]


one <- subset(train, Cover_Type == 1, select = c(1:15, 56))
two <- subset(train, Cover_Type == 2, select = c(1:15, 56))

boxplot(one$Elevation, two$Elevation, xlab = "cover type", ylab = "elevation", names = c("one", "two"))




for(i in 1:16) {
    boxplot(one[,i], two[,i], xlab = "Cover_Type", ylab = varnxs[i], names = c("one", "two"))
}