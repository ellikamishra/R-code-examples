#get current directory
getwd()
dt=read.csv2("dataset1.csv") #read input and view first and last values
head(dt)
tail(dt)
sub=dt[c('comment','like','share')]  #creating subset dataset
head(sub)

write.csv(sub,"dataset-sub.csv")#saving subset dataset

subset1=subset(sub,comment>40)#print record matching query
subset1
data1=read.csv2("dataset1.csv")#rbind to merge 2 dataset
data2=read.csv2("dataset.csv")
data2=read.csv2("dataset.csv")
dim(data2)

new12=rbind(data1,data2)
dim(new12)#verify dimensions

tran=t(sub)
head(tran)

library("reshape")
melt(data=sub,id.vars="comment")#melt data on comment using reshape library

sub=dt[c('Post.Month','Post.Hour','Paid')]#cast data with new data 
head(sub)
cast(sub,Post.Hour ~ Post.Month,mean,value='Paid')
