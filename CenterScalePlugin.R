library(AppliedPredictiveModeling)
library(caret)

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")


input <- function(inputfile) {
	pfix = prefix()
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
   # Need to get the three files
   csvfile <<- paste(pfix, parameters["csvfile", 2], sep="/")

   myData <<- read.csv(csvfile)
   mdrrClass <<- readLines(paste(pfix, parameters["classes", 2], sep="/"))
}

run <- function() {}

output <- function(outputfile) {

nzv <- nearZeroVar(myData)
filteredDescr <- myData[, -nzv]

set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)

write.csv(trainTransformed, paste(outputfile, "training", "csv", sep="."))
write.csv(testTransformed, paste(outputfile, "test", "csv", sep="."))

}
