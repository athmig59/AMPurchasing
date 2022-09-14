# We are going to need a package to read and write to Excel files!
# Check if it already installed. Else install it!

if(!require("openxlsx")){
  install.packages("openxlsx")
  library("openxlsx")
}
# read in the data 

df <- read.xlsx("../extdata/Likert0.xlsx")

# prepare data

n <- as.integer(df$DMUs[1])      # DMUs
m <- as.integer(df$INPUTs[1])    # Inputs
s <- as.integer(df$OUTPUTs[1])   # Outputs

L <- as.integer(df$L[1])         # L, Upper limit in Likert scale, typically 5

# Inputs  <- data.matrix(df[3:(n+2),1:m])

Inputs  <- sapply(df[3:(n+2),1:m], as.numeric)

# Outputs <- data.matrix(df[3:(n+2), (m+1):(m+s)])

Outputs <- sapply(df[3:(n+2), (m+1):(m+s)], as.numeric)

# edit(Inputs)
# edit(Outputs)

colnames(Inputs) <- df[2,1:m]
colnames(Outputs) <- df[2, (m+1):(m+s)]

rownames(Inputs)  <- 1:n
rownames(Outputs) <- 1:n

