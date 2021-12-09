# Function to generate matrix of distance variables.
# Run the code to generate the function, then you can use the function to generate a d matrix for your data set
# using d = distmatrix(data)
distmatrix=function(data){
  # 'cats' is the list of categories
  cats = levels(data$response)
  n = length(cats)
  # initialize distance matrix, d, as an array of 0s, with the same number of rows as data, 
  # and enough columns for the dummy distance variables
  d = array(0,dim=c(length(data$stimulus),0.5*n*(n-1)))
  # initialize a vector to hold the names of the dummy distance variables
  dvars = rep.int(0,0.5*n*(n-1))
  # initialize col, to keep track of which column of d is currently being populated
  col=0
  # Main loop: Enumerate all the pairs of distinct stimulus categories, and define a column of d for each
  # loop through the cats list from first to penultimate item on the list.
  for (i in 1:(n-1)){
    # cat1 is the ith category
    cat1=cats[i]
    # for each cat1, loop through the cats list from the next category to the end of the list
    for (j in (i+1):n){
      # increment col, the current column of d
      col=col+1
      # cat2 is the jth category
      cat2=cats[j]
      # the name of the current column is the concatenation of cat1 and cat2 joined by a period
      dvars[col]=paste(cat1,cat2,sep=".")
      # for each row in the dataframe, the cat1cat2 distance variable has value 1 if stimulus is cat1 and response is cat2
      # or stimulus is cat2 and response is cat1. Otherwise the value remains at 0.
      for (i in 1:length(data$stimulus)){
        if(data$stimulus[i]==cat1 & data$response[i]==cat2|data$stimulus[i]==cat2 & data$response[i]==cat1){
          d[i,col]=1}
      }
    }
  }
  # set the column names of d to the names stored in dvars
  colnames(d)=dvars
  return(d)}