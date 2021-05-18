### Josefine_Mutzenbacher_classify

library(stylo)

n_iterations <- 10 # total number of iterations for each author
# at each iteration, the script will randomly pick up two texts from each author and perform the "classify" function on them

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

### Corpus_creation (with 1-word ngrams)

full_table <- read.csv("corpus/candidates_analysis/table_with_frequencies_full.txt", sep = " ")
full_table <- full_table[,-1]

all_authors <- colnames(full_table)
all_authors <- strsplit(all_authors, "_")
all_authors <- unlist(lapply(all_authors, function(x) x[1]))
authors_selection <- unique(all_authors)

### test quality of attribution methods with classify function

all_methods <- c("svm", "nsc", "knn", "dist.manhattan", "dist.euclidean", "dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg", "dist.argamon", "dist.cosine", "dist.entropy", "dist.simple", "dist.minmaxfast")
results.evaluation <- list()
datestamp <- gsub(pattern = "\\W", replacement = "", x = Sys.time())
out_file <- paste("Classify_results_date", datestamp, ".RData", sep = "")

# main processing loop

for(method_n in 1:length(all_methods)){

  results.evaluation[[method_n]] <- numeric()

  for(my_author_n in 1:length(authors_selection)){

    for(test_n in 1:n_iterations){
      
      my_author_ids <- which(all_authors == authors_selection[my_author_n])
      rand_sel <- sample(my_author_ids, 2, replace = F)
      test_corpus <- as.matrix(t(full_table[,rand_sel]))
      training_corpus <- as.matrix(t(full_table[,-rand_sel]))
            
      if(all_methods[method_n] %in% c("svm", "nsc", "knn")){

        results.stylo <- classify(gui = FALSE, classification.method=all_methods[method_n], mfw.min=50, mfw.max=2000, mfw.incr=50, training.frequencies = training_corpus, test.frequencies = test_corpus)

      }else{

        results.stylo <- classify(gui = FALSE, classification.method="delta", distance.measure=all_methods[method_n], mfw.min=50, mfw.max=2000, mfw.incr=50, training.frequencies = training_corpus, test.frequencies = test_corpus)

      }

      results.evaluation[[method_n]] <- c(results.evaluation[[method_n]], results.stylo$overall.success.rate[1])

    }

  }

  cat("method:", method_n, "author:", my_author_n, "\n", file = "progress.log")

  save.image(out_file)
  
}

### calculate mean efficiency for each method
names(results.evaluation) <- all_methods
efficiency_overview <- unlist(lapply(results.evaluation, mean))
efficiency_overview <- sort(efficiency_overview, decreasing = T)
selected_methods <- names(efficiency_overview[1:5]) # select the five best-performing methods

### save (partial results)

efficiency_w1 <- efficiency_overview
final_results <- as.data.frame(efficiency_w1)
all_methods_old <- rownames(final_results)

### Verify with different ngrams selections

### Step 2 (with 2-word ngrams)
### Corpus_creation 

full_table <- read.csv("corpus/candidates_analysis/table_with_frequencies_ngrams_w2.txt", sep = " ")

all_authors <- colnames(full_table)
all_authors <- strsplit(all_authors, "_")
all_authors <- unlist(lapply(all_authors, function(x) x[1]))
authors_selection <- unique(all_authors)

### test quality of attribution methods with classify function

all_methods <- selected_methods
results.evaluation.w2 <- list()

# main processing loop

for(method_n in 1:length(all_methods)){

  results.evaluation.w2[[method_n]] <- numeric()

  for(my_author_n in 1:length(authors_selection)){

    for(test_n in 1:n_iterations){
      
      my_author_ids <- which(all_authors == authors_selection[my_author_n])
      rand_sel <- sample(my_author_ids, 2, replace = F)
      test_corpus <- as.matrix(t(full_table[,rand_sel]))
      training_corpus <- as.matrix(t(full_table[,-rand_sel]))
            
      if(all_methods[method_n] %in% c("svm", "nsc", "knn")){

        results.stylo <- classify(gui = FALSE, classification.method=all_methods[method_n], mfw.min=50, mfw.max=2000, mfw.incr=50, training.frequencies = training_corpus, test.frequencies = test_corpus)

      }else{

        results.stylo <- classify(gui = FALSE, classification.method="delta", distance.measure=all_methods[method_n], mfw.min=50, mfw.max=2000, mfw.incr=50, training.frequencies = training_corpus, test.frequencies = test_corpus)

      }

      results.evaluation.w2[[method_n]] <- c(results.evaluation.w2[[method_n]], results.stylo$overall.success.rate[1])

    }

  }

  cat("method:", method_n, "author:", my_author_n, "\n", file = "progress.log")

  save.image(out_file)
  
}

### Step 3 (with 4-character ngrams)
### Corpus_creation 

full_table <- read.csv("corpus/candidates_analysis/table_with_frequencies_ngrams_c4.txt", sep = " ")

all_authors <- colnames(full_table)
all_authors <- strsplit(all_authors, "_")
all_authors <- unlist(lapply(all_authors, function(x) x[1]))
authors_selection <- unique(all_authors)

### test quality of attribution methods with classify function

all_methods <- selected_methods
results.evaluation.c4 <- list()

# main processing loop

for(method_n in 1:length(all_methods)){

  results.evaluation.c4[[method_n]] <- numeric()

  for(my_author_n in 1:length(authors_selection)){

    for(test_n in 1:n_iterations){
      
      my_author_ids <- which(all_authors == authors_selection[my_author_n])
      rand_sel <- sample(my_author_ids, 2, replace = F)
      test_corpus <- as.matrix(t(full_table[,rand_sel]))
      training_corpus <- as.matrix(t(full_table[,-rand_sel]))
            
      if(all_methods[method_n] %in% c("svm", "nsc", "knn")){

        results.stylo <- classify(gui = FALSE, classification.method=all_methods[method_n], mfw.min=50, mfw.max=2000, mfw.incr=50, training.frequencies = training_corpus, test.frequencies = test_corpus)

      }else{

        results.stylo <- classify(gui = FALSE, classification.method="delta", distance.measure=all_methods[method_n], mfw.min=50, mfw.max=2000, mfw.incr=50, training.frequencies = training_corpus, test.frequencies = test_corpus)

      }

      results.evaluation.c4[[method_n]] <- c(results.evaluation.c4[[method_n]], results.stylo$overall.success.rate[1])

    }

  }

  cat("method:", method_n, "author:", my_author_n, "\n", file = "progress.log")

  save.image(out_file)
  
}

### save (final results)

names(results.evaluation.c4) <- all_methods
efficiency_c4 <- unlist(lapply(results.evaluation.c4, mean))
efficiency_c4 <- c(efficiency_c4, rep(NA,9))
names(efficiency_c4)[6:14] <- all_methods_old[6:14]
final_results_c4 <- as.data.frame(efficiency_c4)

names(results.evaluation.w2) <- all_methods
efficiency_w2 <- unlist(lapply(results.evaluation.w2, mean))
efficiency_w2 <- c(efficiency_w2, rep(NA,9))
names(efficiency_w2)[6:14] <- all_methods_old[6:14]
final_results_w2 <- as.data.frame(efficiency_w2)

final_results <- cbind(final_results, final_results_w2, final_results_c4)

write.csv(final_results, file = gsub(".RData", ".csv", out_file))

print("Process complete!")