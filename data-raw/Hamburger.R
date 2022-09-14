# We are going to need a package to read and write to Excel files!
# Check if it already installed. Else install it!

if(!require("openxlsx")){
  install.packages("openxlsx")
  library("openxlsx")
}

# Read in the data from the Excel file into a data fame (i.e. similar to Excel sheet)

Data <-read.xlsx("../extdata/Hamburger.xlsx")

# Check a few lines to see what it looks like (Print is only required if run as script)

print( head(Data) )
# head(Data)

# Split data into inputs and outputs as needed by DEA

Output <- Data[c("Profit","Satisfaction","Cleanliness")]

Input <- Data[c("Labor.Hrs","Op..Costs")]

# Name the decision making units (DMUs)

DMUs <- c("U1","U2","U3","U4","U5","U6","U7","U8","U9","U10","U11","U12")
row.names(Input)<-DMUs
row.names(Output)<-DMUs

# Check your outputs and  inputs

print( head(Output) )
# head(Output)

print( head(Input) )
# head(Output)
