df <- read.csv(file = 'miller61.csv', header = TRUE)
str(df)

# Converting data formats from wide to long
df_long <- reshape(df,                       # specify the original data frame
        direction = "long",                  # desired data frame structure
        varying = list(names(df)[2:7]),
        v.names = "count",
        timevar = "response",
        idvar = "X",
        times = names(df)[2:7]
        )

df_long$id <- NULL                           # delete a column
names(df_long)[1] <- 'stimulus'              # rename the first column
rownames(df_long) <- 1:nrow(df_long)         # reset rownames to new order listed sequentially

head(df_long)

cols <- c("stimulus", "response")
df_long[cols] <- lapply(df_long[cols], factor)  # coerce multiple columns to factors at once
str(df_long)   # double-check the structure of the data frame

# generate d matrix by calling function from another file
source("distmatrix.R")
d = distmatrix(df_long)

# Fit a BCM model
m_bcm=glm(count~stimulus+response+d,data=df_long,family=poisson(link="log"))
summary(m_bcm)
