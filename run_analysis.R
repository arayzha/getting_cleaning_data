current_wd = getwd()
setwd('UCI HAR Dataset/')



############# 1. Merges the training and the test sets to create one data set.

library(data.table)
options(digits = 10)

#--- Train
trainx   = fread('train/X_train.txt',sep=' ',stringsAsFactors = F,header=F)
trainy   = fread('train/y_train.txt',sep=' ',stringsAsFactors = F,header=F)
subtrain = fread('train/subject_train.txt',sep=' ',stringsAsFactors = F,header=F)
train    = data.frame(trainx,trainy,subtrain)
colnames(train)[(ncol(train)-1):ncol(train)]=c('y','subject')
head(train)

#--- Test
testx   = fread('test/X_test.txt',sep=' ',stringsAsFactors = F,header=F)
testy   = fread('test/y_test.txt',sep=' ',stringsAsFactors = F,header=F)
subtest = fread('test/subject_test.txt',sep=' ',stringsAsFactors = F,header=F)
test    = data.frame(testx,testy,subtest)
colnames(test)[(ncol(test)-1):ncol(test)]=c('y','subject')
head(test)


#--- Union
dta = rbind.data.frame(train,test)


############# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

feature = fread('features.txt',stringsAsFactors = F)
head(feature)
selected_feature = grep('mean\\(\\)|std\\(\\)',feature$V2)
head(feature$V2[selected_feature])

dta = dta[,c(selected_feature,ncol(dta)-1,ncol(dta))]
head(dta)


############# 3. Uses descriptive activity names to name the activities in the data set

act_label = fread('activity_labels.txt',stringsAsFactors = F)
temp      = merge(x=dta,y=act_label,by.x='y',by.y='V1',all.x=T,sort = F)
temp      = temp[,c(2:ncol(temp))]
colnames(temp)[ncol(temp)]='activity'
dta       = temp
head(dta)

############# 4. Appropriately labels the data set with descriptive variable names.

colnames(dta)[1:(ncol(dta)-2)] = feature$V2[selected_feature]
colnames(dta)                  = gsub('mean\\(\\)','avg',colnames(dta))
colnames(dta)                  = gsub('std\\(\\)','stdev',colnames(dta))
head(dta)


############# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
tdy_dta = dta
tdy_dta = tdy_dta %>% group_by(subject,activity) %>%  summarise_at(vars(-subject,-activity), funs(mean(.)))
head(tdy_dta)


############# Export Data in Step 5

write.table(tdy_dta,'tdy_data.txt',row.names = F)




