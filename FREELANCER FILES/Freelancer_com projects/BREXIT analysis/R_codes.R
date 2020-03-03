
install.packages("rjson")

# Load the package required to read JSON files.
library("rjson")

#Initialize the files
file1="pro_leave1.json"
file2="pro_leave2.json"
file3="pro_leave3.json"
file4="pro_remain.json"
file5="pro_remain2.json"
file6="pro_remain3.json"


# Give the input file name to the function.

result <- fromJSON(file = file5)
# Print the result.
print(result)

# # Convert JSON file to a data frame.
names=result$data$attributes$signatures_by_constituency
#do.call(rbind.data.frame, names)

## Compute maximum length
max.length <- max(sapply(names, length))
## Add NA values to list elements
names1 <- lapply(names, function(v) { c(v, rep(NA, max.length-length(v)))})
## Rbind
my_data=do.call(rbind, names1)

my_data=as.data.frame(my_data)
my_data=my_data[,c("ons_code","signature_count")]


#Write the data into text files 

#Initialize the text file path
path1="C:/Users/Administrator/Desktop/FREELANCER_COM_WORK/BREXIT analysis/proleave1.txt"
path2="C:/Users/Administrator/Desktop/FREELANCER_COM_WORK/BREXIT analysis/proleave2.txt"
path3="C:/Users/Administrator/Desktop/FREELANCER_COM_WORK/BREXIT analysis/proleave3.txt"
path4="C:/Users/Administrator/Desktop/FREELANCER_COM_WORK/BREXIT analysis/proremain1.txt"
path5="C:/Users/Administrator/Desktop/FREELANCER_COM_WORK/BREXIT analysis/proremain22.txt"
path6="C:/Users/Administrator/Desktop/FREELANCER_COM_WORK/BREXIT analysis/proremain3.txt"

library(data.table)
fwrite(x = my_data,
file = path5,sep = ",",col.names=T,append=T)



