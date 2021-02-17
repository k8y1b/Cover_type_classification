train <- read.csv("train.csv")
test <- read.csv("test.csv")

varn = names(train)

varnxs = varn[c(2:15, 56)]

covertypes = c("spruce/fir", "lodgepole pine", "ponderosa pine", "cottonwood/willow", "aspen", "douglas fir", "krummholz")


# deselect the soil type binary variables and create data frame for each cover type

one <- subset(train, Cover_Type == 1, select = c(1:11, 56))
two <- subset(train, Cover_Type == 2, select = c(1:11, 56))
three <- subset(train, Cover_Type == 3, select = c(1:11, 56))
four <- subset(train, Cover_Type == 4, select = c(1:11, 56))
five <- subset(train, Cover_Type == 5, select = c(1:11, 56))
six <- subset(train, Cover_Type == 6, select = c(1:11, 56))
seven <- subset(train, Cover_Type == 7, select = c(1:11, 56))





for(i in 1:4) {
    boxplot(one[,i], two[,i], three[,i], four[,i], five[,i], six[,i], seven[,i], xlab = "Cover_Type", ylab = varnxs[i], names = covertypes)
}
