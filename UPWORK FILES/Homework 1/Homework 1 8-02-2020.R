
library(dplyr)


# a.	Read the data into a data frame in R
pdcalls=read.csv("jerseycitypdservicecalls2017csv.csv",header = T)
attach(pdcalls)

# b.	Find the number of variables and number of rows of data
#The first number is for rows and the second for variables
dim(pdcalls)

# c.	Inspect the data by printing out the first six rows.
head(pdcalls,6)

# d.	How many rows are there with missing values?
sum(!complete.cases(pdcalls))


#   e.	Remove the rows with missing data
pdcalls <- pdcalls[complete.cases(pdcalls), ]
dim(pdcalls)


# f.	Check to see if there are any duplicate rows.  If there are duplicate
# rows then remove them.
which(duplicated(pdcalls))
pdcalls=pdcalls[!duplicated(edit1), ]
dim(pdcalls)

# g.	Sort the data by call type
head(pdcalls)
head(pdcalls[order(call.type),])



# h.	Create a new data frame called calls911 by filtering the original 
# dataset for the 911 calls.  Print out the first six rows and check if the
# filtering worked.  How many 911 calls were there?
calls911=filter(pdcalls,call.type=='911')
head(calls911)

# i.	Create a new variable (column) called dispatchduration by subtracting
# time.received from time.dispatched.  Hint:  Use the mutate command in 
# dplyr. You also need to convert the format of time.received and
# time.dispatched using the strptime() command and then subtract. 

library(dplyr)
pdcalls$time_R<-strptime(pdcalls$time.received,  # Apply strptime with timezone
                    format = "%m/%d/%Y %H:%M:%OS",
                    tz = "EST")

pdcalls$time_D<-strptime(pdcalls$time.dispatched,  # Apply strptime with timezone
                 format = "%m/%d/%Y %H:%M:%OS",
                 tz = "EST")

pdcalls$time_D=as.POSIXct(pdcalls$time_D)
class(pdcalls$time_D)

pdcalls$time_R=as.POSIXct(pdcalls$time_D)
class(pdcalls$time_R)


pdcalls$dispatchduration=difftime(testPD1$time_D, testPD1$time_R,
                                  units = "secs")

head(pdcalls)








# j.	Now check if there are missing values in the newly created column 
# and also check for dispatch durations that are negative or zero.
# This is garbage data so remove these rows.

#Missing values
sum(!complete.cases(pdcalls$dispatchduration))

#Negative values
nrow(pdcalls[pdcalls$dispatchduration<0,])

# Zero values
nrow(pdcalls[pdcalls$dispatchduration==0,])

# Remove negative and zero values
pdcalls_clean=pdcalls[pdcalls$dispatchduration>0,]
dim(pdcalls_clean)
head(pdcalls_clean)


# k.	Find the average (mean) dispatch duration using the new variable
#(column) you created above.

mean(pdcalls_clean$dispatchduration)


# l.	Find the average (mean) dispatch duration by call type
install.packages("doBy")
library(doBy)
summaryBy(pddispatchduration~call.type, data=pdclass_clean, FUN=mean)


# m.	How many rows contain the word GUNSHOTS in the call code description
# column.  Hint: Use the stringr package.  Install it if needed.  Use the
# str_detect function in stringr.  Use the help to learn more about the
# function and how to use it.

length(grep("GUNSHOTS", pdcalls$call.code.description))

# n.	Now create a data frame called gunshotdata that has just the rows
# that contain the word GUNSHOTS in the call code description

gunshotdata=dplyr::filter(pdcalls, grepl('GUNSHOTS', call.code.description))
head(gunshotdata)















