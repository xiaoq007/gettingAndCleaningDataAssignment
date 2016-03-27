library(dplyr)
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "./data/c3w4_data.zip",method="curl")
setwd("./data/UCI HAR Dataset")
options(stringsAsFactors = F)

#a function to set some default parameters for read.csv
readData<-function(x)(read.csv(x,header=F,sep="\t",strip.white=T)) #set some default parameters

activity_labels<-readData("./activity_labels.txt")
features<-readData("./features.txt")

#generate activity label and features data frame
convert.df<-function(l){
        pos<-function(x){regexpr(" [a-zA-Z]",x)-1}
        col1<-apply(l,1,function(x)substr(x,1,pos(x)))
        col2<-apply(l,1,function(x)substr(x,pos(x)+2,nchar(x)))
        df<-data.frame(label=col1,name=col2)
        return(df)
}
df.al<-convert.df(activity_labels);df.ftrs<-convert.df(features)

#function to read test and train data set into data frames
loadData<-function(path,sf="Inertial Signals/"){
        filePath<-paste0("./",path,"/",sf)
        fileNames<-dir(filePath)[grepl("txt",dir(filePath))]
        fullPath<-paste0(filePath,fileNames)
        data<-lapply(fullPath,function(x) readData(x)) 
        names(data)<-sapply(fileNames,function(x) substr(x,1,(nchar(x)-4)))
        df<-cbind(data$subject_test,data$y_test,data$X_test)
        names(df)<-c("subject","label","data")
        return(df)
        }

#load data to data frames, merge test and train data frames
data.test<-loadData("test",sf=NULL);data.train<-loadData("train",sf=NULL)
data.all<-rbind(data.test,data.train) 

#convert label in data frame "data.all" to activity
act<-sapply(data.all$label,function(x)(df.al$name[grepl(x,df.al$label)]))

#extract only mean and std
lg<-lapply(df.ftrs$name,function(x)grepl("mean\\(\\)|std\\(\\)",x))
lg<-unlist(lg)
data<-lapply(data.all$data,function(x)strsplit(x," +"))
data<-matrix(unlist(data),nrow=length(data));data<-as.data.frame(data)
result<-data[,lg];names(result)<-df.ftrs$name[lg]

#assemble final data frame
output<-data.frame(subject=data.all$subject,activity=act,result)
names(output)<-str_replace_all(names(output), "[^[:alnum:]]", "")


#Calculate avarage for each variable by subject and activity
subjectActivity<-paste0(output$subject,"_",output$activity)
temp<-(select(output,-c(subject,activity)))
temp<-lapply(temp,function(x)as.numeric(x))
temp<-data.frame(subjectActivity,temp)
output.ava<-temp %>% group_by(subjectActivity) %>% summarise_each(funs(mean))
output.ava<-as.data.frame(output.ava)

write.table(output.ava,"./output.txt",row.names=F,sep="\t")

write.csv(names(output.ava),"./outputVariables.txt",row.names=F)

