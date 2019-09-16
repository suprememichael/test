library(uwIntroStats)
library(readxl)
library(tidyverse)
setwd('/Users/jiachenwang/Desktop/bb')
fevDat <- read_excel("./FEV.DAT.xls")
print (fevDat)

print (fevDat[,2])

# stat for age
descrip(fevDat[,2])
descrip(fevDat[,3])
descrip(fevDat[,4])
descrip(fevDat[,5])
descrip(fevDat[,6])

size = nrow(fevDat)
loop = seq(1,size,1)
print (fevDat[,2]) 
library(ggplot2)

ggplot(fevDat, aes(x = Id, y = Age)) + geom_point()




