
LOFCraft <- function(data=iris[,-5],threshold=2.0,k = c(5:10)) 
{
  
#threshold=3.1
#data=iris[,-5]
#k=c(5:10)

library('Rlof') #Outlier detection library
outlier.scores <- lof(data, k= k)
mean <- rowMeans(outlier.scores) #Calculating the mean of every execution
outlier.scores <- data.frame(outlier.scores, mean) #adding mean to data frame


#library("RegressionLibs")

#DensityPlot(outlier.scores, ncol(outlier.scores)) #Generating a plot of outliers scores

aux <- outlier.scores[,7]>threshold #by default 1.7 is the threshold selected
#aux <- outlier.scores[,7]>1.7 #by default 1.7 is the threshold selected


#install.packages("plyr")
library("plyr") ##required for count()
numberOfOutliersFound<-count(aux)[2,2] #Number of outliers found




if(is.na(numberOfOutliersFound)){
  withoutOutliers.scores <- outlier.scores  
  outliers<-NULL
  dataWithoutOutliers<-data
  }
else{
    outliers <- order(outlier.scores[,7], decreasing=T)[1:numberOfOutliersFound] #Getting the values that are on the threshold
    Score <- outlier.scores[outliers,7] #Getting outliers scores
    outliers <- data.frame(outliers,Score)
    names(outliers) <- c("Position","Score")
    #View(outliers)
      
    withoutOutliers.scores <- outlier.scores[-outliers[1:numberOfOutliersFound,1],] #Eliminating the 527 most remote instances!
    #DensityPlot(outlier.scores, ncol(outlier.scores)) #Generating a plot of outliers scores
    #DensityPlot(withoutOutliers.scores, ncol(outlier.scores)) #Generating a plot of outliers scores
    #select instances
    dataWithoutOutliers<-data[-outliers[1:numberOfOutliersFound,1],]
    #class(outlier.scores)
  }

return(list(outlier.scores,withoutOutliers.scores,dataWithoutOutliers,numberOfOutliersFound,outliers))
}




res<-LOFCraft(iris[,-5],threshold = 1.6,k = c(5:10)) ##calling LOF 
outlier.scores=data.frame(res[1])  ## scores for the original data
#str(outlier.scores) 
withoutOutliers.scores=data.frame(res[2]) ## scores of data without outliers
#str(withoutOutliers.scores)  
dataWithoutOutliers<-data.frame(res[3])  ##the data without outliers
#str(dataWithoutOutliers)
#View(dataWithoutOutliers)
DensityPlot(outlier.scores, ncol(outlier.scores)) #Generating a plot of outliers scores
DensityPlot(withoutOutliers.scores, ncol(outlier.scores)) #Generating a plot of outliers scores



##example 2 ... mtcars

res<-LOFCraft(mtcars,threshold = 1.25,k = c(5:10)) ##calling LOF 
outlier.scores=data.frame(res[1])  ## scores for the original data
#str(outlier.scores) 
withoutOutliers.scores=data.frame(res[2]) ## scores of data without outliers
#str(withoutOutliers.scores)  
dataWithoutOutliers<-data.frame(res[3])  ##the data without outliers
#str(dataWithoutOutliers)
#View(dataWithoutOutliers)
howManyOutliers<-as.numeric(res[4]) ## the total number of outliers
outliers=data.frame(res[5])  ## the positions of the outliers in the original data and theirs respective scores
#str(outliers)

DensityPlot(outlier.scores, ncol(outlier.scores)) #Generating a plot of outliers scores
DensityPlot(withoutOutliers.scores, ncol(outlier.scores)) #Generating a plot of outliers scores



