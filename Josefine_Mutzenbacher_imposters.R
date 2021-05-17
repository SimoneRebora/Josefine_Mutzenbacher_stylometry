library(stylo)

# new minmax function
dist.minmaxfast = function(x){
  
  # test if the input dataset is acceptable
  if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
    stop("cannot apply a distance measure: wrong data format!")
  }
  # then, test whether the number of rows and cols is >1
  if(length(x[1,]) < 2 | length(x[,1]) < 2) {
    stop("at least 2 cols and 2 rows are needed to compute a distance!")
  }
  
  # getting the size of the input table
  rows = length(x[,1])
  # starting a new matrix
  y = matrix(nrow = rows, ncol = rows)
  rownames(y) = rownames(x)
  colnames(y) = rownames(x)
  # iterating over rows and columns
  for(i in 1:rows) {
    for(j in i:rows ) {
      y[j,i] = 1 - sum(pmin(x[i,], x[j,])) / sum(pmax(x[i,], x[j,]))
    }
  }
  
  # converting the matrix to the class 'dist'
  y = as.dist(y)
  
  return(y)
}

# choose distance for analysis
### read configuration file
config <- read.csv("Analysis_configuration.csv", stringsAsFactors = F)

### define variables for the analysis
finale <- as.logical(config$value[which(config$feature == "finale")])
chosen_distance <- config$value[which(config$feature == "imposters_distance")]

# find document-term matrixes
if(finale){
  all_imposters_files <- list.files("corpus/imposters_analysis/finale", pattern = ".csv", full.names = T)
  # extract names of candidates
  all_candidate_names <- gsub(pattern = "corpus/imposters_analysis/finale/Imposters_|.csv", replacement = "", x = all_imposters_files)
}else{
  all_imposters_files <- list.files("corpus/imposters_analysis/full", pattern = ".csv", full.names = T)
  # extract names of candidates
  all_candidate_names <- gsub(pattern = "corpus/imposters_analysis/full/Imposters_|.csv", replacement = "", x = all_imposters_files)
}

# prepare file for results
datestamp <- gsub(pattern = "\\W", replacement = "", x = Sys.time())
if(finale){
  filename <- paste("Imposters_finale_attribution_", datestamp, ".txt", sep = "")
}else{
  filename <- paste("Imposters_full_attribution_", datestamp, ".txt", sep = "")
}

cat("Distance: ", chosen_distance, "\tMFW: 2000\n\n", file = filename, sep = "")

# run analysis for each candidate
for(i in 1:length(all_candidate_names)){
  
  my_candidate <- all_candidate_names[i]
  
  # read data from d-t matrix
  data <- read.csv(all_imposters_files[i], stringsAsFactors = F, row.names = 1)
  
  # get titles of all texts
  all_texts <- rownames(data)

  # get names of all authors
  all_authors <- strsplit(all_texts, "_")
  all_authors <- unlist(lapply(all_authors, function(x) x[1]))
  all_authors <- unique(all_authors)
  
  # isolate imposters
  my_imposters <- all_authors[-c(1,2)]
  
  # identify test text (always the first here) and texts of candidate author
  test_id <- 1
  candidate_id <- which(grepl(pattern = my_candidate, x = all_texts))
  
  # use imposters on selected candidate
  data <- as.matrix(data) # dataframe has to be trasformed into matrix to be read by stylo
  imposters_results <- imposters(reference.set = data[-c(test_id, candidate_id),], test = data[test_id,], candidate.set = data[candidate_id,], distance = chosen_distance)

  # save results    
  cat(my_candidate, "probability:", imposters_results, "\n", file = filename, sep = " ", append = T)

}
