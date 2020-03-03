install.packages("tabulizer")


library(tabulizer)
library(dplyr)


remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))





R.version

# Extract the table
out <- extract_tables('MPESA_unlocked.pdf')
mpesa_df=data.frame(out)
final <- do.call(rbind, out[-length(out)])

# table headers get extracted as rows with bad formatting. Dump them.
final <- as.data.frame(final[3:nrow(final), ])

# Column names
headers <- c('Receipt_No','Completion_Time','Details',
    'Transaction_Status','Paid_In','Withdrawn','Balance')

# Apply custom column names
names(out) <- headers

# These dplyr steps are not strictly necessary for dumping to csv, but useful if further data 
# manipulation in R is required. 
final <- final %>%
  # Convert date columns to date objects
  mutate_each(funs(as.Date(., format='%m/%d/%Y')), Notice.Date, Effective.Date, Received.Date) %>%
  # Convert No.of.Employees to numeric
  mutate(No.of.Employees = as.numeric(levels(No.of.Employees)[No.of.Employees]))

# Write final table to disk
write.csv(final, file='CA_WARN.csv', row.names=FALSE)




