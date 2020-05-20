### Josefine_Mutzenbacher_stylo
### code for running stylometric analyses of the novel "Josefine Mutzenbacher"

library(stylo)
library(tidyverse)
library(reshape2)

### read configuration file
config <- readLines("Analysis_configuration.txt")
config <- config[which(!grepl("#", config))]
config <- config[which(config != "")]
config <- strsplit(config, " = ")

config_value <- unlist(lapply(config, function(x) x[2]))
names(config_value) <- unlist(lapply(config, function(x) x[1]))

### define variables for the analysis
culling_percentage <- as.numeric(config_value[which(names(config_value) == "culling_percentage")])
randomize <- as.logical(config_value[which(names(config_value) == "randomize")])
mfw_min <- as.numeric(config_value[which(names(config_value) == "mfw_min")])
mfw_max <- as.numeric(config_value[which(names(config_value) == "mfw_max")])
mfw_incr <- as.numeric(config_value[which(names(config_value) == "mfw_incr")])
mfw_selection <- (1:mfw_max)*mfw_incr
mfw_selection <- mfw_selection[which(mfw_selection <= mfw_max)]
mfw_selection <- mfw_selection[which(mfw_selection >= mfw_min)]
distances_selection <- unlist(strsplit(config_value[which(names(config_value) == "distances_selection")], " "))

### save all methods in a grid
methods_combination <- expand.grid(mfw_selection, distances_selection, stringsAsFactors = FALSE)

### prepare variables to save results
author_full_attr <- list()
author_full_attr_detailed <- list()
simple_score <- numeric()

### main iteration starts
for(i in 1:dim(methods_combination)[1]){
  
  mfw_choice <- methods_combination[,1][i]
  dist_choice <- methods_combination[,2][i]
  
  ### first part: test quality of attribution method
  
  if(randomize){
    my_frequencies <- "corpus/candidates_analysis/table_with_frequencies_test_random.txt"
  }else{
    my_frequencies <- "corpus/candidates_analysis/table_with_frequencies_test.txt"
  }
  
  my_results <- stylo(gui = F, 
                      frequencies = my_frequencies, # texts are read from the term-document matrix
                      corpus.lang = "German",
                      encoding = "UTF-8",
                      mfw.min = mfw_choice,
                      mfw.max = mfw_choice,
                      mfw.incr = mfw_choice,
                      culling.min = culling_percentage,
                      culling.max = culling_percentage,
                      analysis.type = "CA",
                      distance.measure = dist_choice,
                      write.png.file = F
  )
  
  my_results_classes <- names(my_results$distance.table[1,])
  my_results_classes <- strsplit(my_results_classes, split = "_")
  my_results_classes <- unlist(lapply(my_results_classes, function(x) x[1]))
  
  my_results_unique_classes <- unique(my_results_classes)
  
  simple_score[i] <- 0
  
  standardized_df <- my_results$distance.table
  standardized_df <- (standardized_df-mean(standardized_df))/sd(standardized_df)
  
  for(my_class in my_results_unique_classes){
    
    cat("Checking results for class", my_class, "...\n")
    
    in_group_df_ids <- which(my_results_classes == my_class)
    in_group_df <- standardized_df[in_group_df_ids,in_group_df_ids]
    
    out_group_df <- standardized_df[-in_group_df_ids,-in_group_df_ids]
    
    simple_score_for_class = mean(out_group_df) - mean(in_group_df)
    print(simple_score_for_class)
    
    simple_score[i] <- simple_score[i]+simple_score_for_class
    
  }
  
  simple_score[i] <- simple_score[i]/length(my_results_unique_classes)
  
  ### second part: attribution
  
  if(randomize){
    my_frequencies <- "corpus/candidates_analysis/table_with_frequencies_full_random.txt"
  }else{
    my_frequencies <- "corpus/candidates_analysis/table_with_frequencies_full.txt"
  }
  
  stylo_results <- stylo(gui = F, 
                         frequencies = my_frequencies, # texts are read from the term-document matrix
                         culling.min = culling_percentage,
                         culling.max = culling_percentage,
                         corpus.lang = "German",
                         encoding = "UTF-8",
                         mfw.min = mfw_choice,
                         mfw.max = mfw_choice,
                         mfw.incr = mfw_choice,
                         analysis.type = "CA",
                         distance.measure = dist_choice,
                         write.png.file = F
  )
  
  ### using centroid for attribution
  all_authors_attributions <- strsplit(names(stylo_results$distance.table[1,]), "_")
  all_authors_attributions <- unlist(lapply(all_authors_attributions, function(x) x[1]))
  author_attr <- list()
  author_full_attr_tmp <- numeric()
  
  authors_selection <- unique(all_authors_attributions[-1])
  
  for(author_id in 1:length(authors_selection)){
    my_author <- authors_selection[author_id]
    author_ids <- which(all_authors_attributions == my_author)
    author_attr[[author_id]] <- sort(stylo_results$distance.table[1,][author_ids])[1:3]
    author_full_attr_tmp[author_id] <- mean(author_attr[[author_id]])
  }
  
  author_full_attr_detailed[[i]] <- author_attr
  
  names(author_full_attr_tmp) <- authors_selection
  
  author_full_attr[[i]] <- author_full_attr_tmp
  
  cat("##############\n##############\n##############\n", i, "of", dim(methods_combination)[1], "\n##############\n##############\n##############\n")
  
  unlink("*_EDGES.csv")
  
}

### save results in a dataframe
josefine_results <- as.data.frame(do.call(rbind, author_full_attr))
josefine_results$MFW <- methods_combination[,1]
josefine_results$distance <- methods_combination[,2]
josefine_results$simple_score <- simple_score

josefine_results$attribution <- ""
for(i in 1:length(josefine_results$Salten)){
  josefine_results$attribution[i] <- names(which.min(josefine_results[i,1:length(authors_selection)]))
}

### define outfile title
datestamp <- gsub(pattern = "\\W", replacement = "", x = Sys.time())
if(randomize){
  randomization <- "randomized"
}else{
  randomization <- "serial"
}
out_file <- paste("Stylo_results_",randomization,"_culling",culling_percentage,"_date", datestamp, sep = "")

### write csv of results
write.csv(x = josefine_results, file = paste(out_file, ".csv", sep = ""))
### save all
save.image(file = paste(out_file, ".RData", sep = ""))

### make graph for evolution of attribution with a single measure
chosen_distances <- c("dist.delta", "dist.eder", "dist.canberra", "dist.wurzburg")
for(chosen_distance in chosen_distances){
  
  josefine_results_sel <- josefine_results[which(josefine_results$distance == chosen_distance),]
  
  josefine_results_sel <- melt(josefine_results_sel, measure.vars =  authors_selection)
  
  p1 <- ggplot(josefine_results_sel, aes(x=MFW, y=value)) + 
    geom_line(aes(color = variable)) +
    geom_point(aes(color = variable, shape = variable)) +
    ggtitle(chosen_distance) +
    scale_shape_manual(values=1:length(authors_selection))
  
  ggsave(p1, filename = paste(out_file, "_", chosen_distance, ".png", sep = ""), width = 16, height = 9, dpi = 300, scale = 0.7)
  
}

print("Delta analysis complete!!!")

### rolling Delta (if analysis on full text)

if(randomize){
  my_frequencies <- "corpus/rolling_delta/freq_table_reference_set_random.txt"
}else{
  my_frequencies <- "corpus/rolling_delta/freq_table_reference_set.txt"
}

all_frequencies <- read.csv(my_frequencies, sep = " ")

training_frequencies <- t(all_frequencies)

test_frequencies <- read.csv("corpus/rolling_delta/freq_table_sliced_sample.txt", sep = " ")
test_frequencies <- t(test_frequencies)

classify_results <- rolling.classify(training.frequencies = training_frequencies, test.frequencies = test_frequencies, write.png.file = TRUE, classification.method = "delta", mfw=2000, distance.measure = "wurzburg")

rolling_plot <- list.files(pattern = "rolling-delta")

file.rename(rolling_plot, paste(out_file, "rolling_plot.png", sep = "_"))
  
print("Process finished!!!")
