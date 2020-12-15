library(tidyverse)
library(ROCR)
library(glmnet)
library(data.table)

# -----------------------------------------
# Function to compute AUC (from week 7 discussion notes)
# -----------------------------------------
compute_auc <- function(p, labels) {
  pred <- prediction(p, labels)
  auc <- performance(pred, 'auc')
  auc <- unlist(slot(auc, 'y.values'))
  auc
}

# -----------------------------------------
# Function to plot ROC curve (from week 7 discussion notes)
# -----------------------------------------
plot_roc <- function(p, labels, model_name) {
  pred <- prediction(p, labels)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, col="black", main = paste("ROC for", model_name))
}

#------------------------------------------
# Function to plot ROC and return AUC value
#------------------------------------------
auc_roc <- function(model, lambda, data, model_name) {
  predictions <- predict(model, newx = new_x(data), s = lambda, data, type = 'response')
  plot_roc(predictions, data$target, model_name)
  return(compute_auc(predictions, data$target))
}

#----------------------------------------------------------------------
# Function: convert_keys
# Description: This function takes in a dataframe df and converts the
# integer-valued key to its corresponding character representation.
#----------------------------------------------------------------------
convert_keys <- function(df) {
  df[df$key == 0, 'key'] <- 'C'
  df[df$key == 1, 'key'] <- 'C#/Db'
  df[df$key == 2, 'key'] <- 'D'
  df[df$key == 3, 'key'] <- 'D#/Eb'
  df[df$key == 4, 'key'] <- 'E'
  df[df$key == 5, 'key'] <- 'F'
  df[df$key == 6, 'key'] <- 'F#/Gb'
  df[df$key == 7, 'key'] <- 'G'
  df[df$key == 8, 'key'] <- 'G#/Ab'
  df[df$key == 9, 'key'] <- 'A'
  df[df$key == 10, 'key'] <- 'A#/Bb'
  df[df$key == 11, 'key'] <- 'B'
  return(df)
}

#--------------------------------------------
# Function: convert_time_sig
# Description: Takes in a dataframe and replaces
# all zero-valued time signature entries to NA and 
# casts all time signature values as characters
#--------------------------------------------
convert_time_sig <- function(df) {
  df[df$time_signature == 0, 'time_signature'] <- NA
  df$time_signature <- as.character(df$time_signature)
  return(df)
}

#---------------------------------------------------------------------
# Function: clean
# Description: cleans our data by converting keys and time signature to 
# appropriate type and leveling key, modality, and time signatures
#---------------------------------------------------------------------
clean <- function(df) {
  df <- df %>%
    convert_keys() %>% 
    convert_time_sig() %>%
    mutate(hit = (target == 1))
  
  df[,6] <- factor(df[,6], 
                   levels = c('C', 'C#/Db', 'D', 'D#/Eb', 'E', 'F', 'F#/Gb', 'G', 'G#/Ab', 'A', 'A#/Bb', 'B')
  )
  df[,8] <- factor(df[,8], levels = c(1, 0))
  df[,16] <- factor(df[,16], levels = c(4, 3, 2, 1, 0, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
  return(df)
}

##### Download and clean data for each decade #####

sixties <- read.csv("~/Desktop/hit-predictor/60s.csv")
seventies <- read.csv("~/Desktop/hit-predictor/70s.csv")
eighties <- read.csv("~/Desktop/hit-predictor/80s.csv")
nineties <- read.csv("~/Desktop/hit-predictor/90s.csv")
thousands <- read.csv("~/Desktop/hit-predictor/00s.csv")
tens <- read.csv("~/Desktop/hit-predictor/10s.csv")

### Row bind datasets for all individual decades into one dataset
full <- rbind(sixties, seventies, eighties, nineties, thousands, tens)


# Clean all dataframes
sixties <- clean(sixties)
seventies <- clean(seventies)
eighties <- clean(eighties)
nineties <- clean(nineties)
thousands <- clean(thousands)
tens <- clean(tens)
full <- clean(full)


# --------------------------------------------
# Preliminary Data Exploration/visualization
# --------------------------------------------


###################### Full Data Plots ###########################

title <- 'Decade-Spanning Data'
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(danceability, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(energy, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_bar(mapping = aes(key, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(loudness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(mode, color = hit), position = 'dodge', stat = 'count') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(speechiness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(acousticness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(instrumentalness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(liveness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(valence, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(tempo, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(duration_ms, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_bar(mapping = aes(time_signature, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(chorus_hit, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = full) +
    geom_histogram(mapping = aes(sections, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  

############################### Sixties Data Plots #################################

  title <- '1960s Data'
  df <- sixties
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(danceability, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(energy, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(key, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(loudness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(mode, color = hit), position = 'dodge', stat = 'count') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(speechiness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(acousticness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(instrumentalness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(liveness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(valence, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(tempo, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(duration_ms, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(time_signature, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(chorus_hit, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(sections, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)

############################# Seventies Data Plots ##################################  
  
  title <- '1970s Data'
  df <- seventies
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(danceability, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(energy, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(key, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(loudness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(mode, color = hit), position = 'dodge', stat = 'count') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(speechiness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(acousticness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(instrumentalness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(liveness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(valence, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(tempo, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(duration_ms, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(time_signature, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(chorus_hit, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(sections, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  
################################## Eighties Data Plots #####################################
  
  title <- '1980s Data'
  df <- eighties
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(danceability, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(energy, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(key, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(loudness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(mode, color = hit), position = 'dodge', stat = 'count') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(speechiness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(acousticness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(instrumentalness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(liveness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(valence, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(tempo, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(duration_ms, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(time_signature, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(chorus_hit, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(sections, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  
########################### Nineties Data Plots #################################
  
  title <- '1990s Data'
  df <- nineties
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(danceability, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(energy, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(key, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(loudness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(mode, color = hit), position = 'dodge', stat = 'count') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(speechiness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(acousticness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(instrumentalness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(liveness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(valence, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(tempo, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(duration_ms, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(time_signature, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(chorus_hit, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(sections, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  
############################### 2000s Data Plots #####################################
  
  title <- '2000s Data'
  df <- thousands
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(danceability, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(energy, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(key, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(loudness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(mode, color = hit), position = 'dodge', stat = 'count') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(speechiness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(acousticness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(instrumentalness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(liveness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(valence, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(tempo, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(duration_ms, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(time_signature, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(chorus_hit, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(sections, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  
######################### 2010s Data Plots #########################
  
  title <- '2010s Data'
  df <- tens
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(danceability, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(energy, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(key, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(loudness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(mode, color = hit), position = 'dodge', stat = 'count') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(speechiness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(acousticness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(instrumentalness, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(liveness, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(valence, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(tempo, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(duration_ms, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_bar(mapping = aes(time_signature, color = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(chorus_hit, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplot(data = df) +
    geom_histogram(mapping = aes(sections, fill = hit), position = 'dodge') +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  

  
  
    
#---------------------------------------------
# Splitting data into train/validate/test sets
#---------------------------------------------

set.seed(138)

shuffled <- full[sample(nrow(full)),] # randomize ordering of data
train_len <- round(0.6 * nrow(full)) # 60% for training data
valid_len <- round(0.2 * nrow(full)) # 20% for validation set

# slicing the entire shuffled data set accordingly
train <- shuffled %>%
  slice(1:train_len) 
valid <- shuffled %>%
  slice((train_len + 1):(train_len + valid_len))
test <- shuffled %>%
  slice((train_len + valid_len + 1):nrow(shuffled))

### For sixties dataset ###
set.seed(69)
shuffled_sixties <- sixties[sample(nrow(sixties)),] # randomize ordering of data
train_len_sixties <- round(0.6 * nrow(sixties)) # 60% for training data
valid_len_sixties <- round(0.2 * nrow(sixties)) # 20% for validation set

# slicing the entire shuffled data set accordingly
train_sixties <- shuffled_sixties %>%
  slice(1:train_len_sixties) 
valid_sixties <- shuffled_sixties %>%
  slice((train_len_sixties + 1):(train_len_sixties + valid_len_sixties))
test_sixties <- shuffled_sixties %>%
  slice((train_len_sixties + valid_len_sixties + 1):nrow(shuffled_sixties))


### For seventies dataset ###

set.seed(420) ### Reset Seed to get same shuffle every time 

shuffled_seventies <- seventies[sample(nrow(seventies)),]
train_len_seventies <- round(0.6 * nrow(seventies))
valid_len_seventies <- round(0.2 * nrow(seventies))

train_seventies <- shuffled_seventies %>%
  slice(1:train_len_seventies)
valid_seventies <- shuffled_seventies %>%
  slice((train_len_seventies + 1):(train_len_seventies + valid_len_seventies))
test_seventies <- shuffled_seventies %>%
  slice((train_len_seventies + valid_len_seventies + 1):nrow(shuffled_seventies))

### Eighties ###
set.seed(666)

shuffled_eighties <- eighties[sample(nrow(eighties)),]
train_len_eighties <- round(0.6 * nrow(eighties))
valid_len_eighties <- round(0.2 * nrow(eighties))

train_eighties <- shuffled_eighties %>%
  slice(1:train_len_eighties)
valid_eighties <- shuffled_eighties %>%
  slice((train_len_eighties + 1):(train_len_eighties + valid_len_eighties))
test_eighties <- shuffled_eighties %>%
  slice((train_len_eighties + valid_len_eighties + 1):nrow(shuffled_eighties))


### Nineties ###
set.seed(80085)

shuffled_nineties <- nineties[sample(nrow(nineties)),]
train_len_nineties <- round(0.6 * nrow(nineties))
valid_len_nineties <- round(0.2 * nrow(nineties))

train_nineties <- shuffled_nineties %>%
  slice(1:train_len_nineties)
valid_nineties <- shuffled_nineties %>%
  slice((train_len_nineties + 1):(train_len_nineties + valid_len_nineties))
test_nineties <- shuffled_nineties %>%
  slice((train_len_nineties + valid_len_nineties + 1):nrow(shuffled_nineties))


### 2000s ###
set.seed(123)

shuffled_thousands <- thousands[sample(nrow(thousands)),]
train_len_thousands <- round(0.6 * nrow(thousands))
valid_len_thousands <- round(0.2 * nrow(thousands))

train_thousands <- shuffled_thousands %>%
  slice(1:train_len_thousands)
valid_thousands <- shuffled_thousands %>%
  slice((train_len_thousands + 1):(train_len_thousands + valid_len_thousands))
test_thousands <- shuffled_thousands %>%
  slice((train_len_thousands + valid_len_thousands + 1):nrow(shuffled_thousands))


### 2010s ###
set.seed(456)

shuffled_tens <- tens[sample(nrow(tens)),]
train_len_tens <- round(0.6 * nrow(tens))
valid_len_tens <- round(0.2 * nrow(tens))

train_tens <- shuffled_tens %>%
  slice(1:train_len_tens)
valid_tens <- shuffled_tens %>%
  slice((train_len_tens + 1):(train_len_tens + valid_len_tens))
test_tens <- shuffled_tens %>%
  slice((train_len_tens + valid_len_tens + 1):nrow(shuffled_tens))


#### Create Models ####

# --------------------------------------------------------------
# Function: new_x
# Description: Takes in a given dataframe and returns a model matrix
# to be used to make predictions.
# --------------------------------------------------------------
new_x <- function(data) {
  x <- model.matrix(target ~ 1 + danceability + energy + loudness + mode +
                      speechiness + acousticness + instrumentalness + liveness + valence +
                      tempo + duration_ms + chorus_hit + sections, data)[,-1]
  return(x)
}

# --------------------------------------------------------------
# Function: create_model
# Description: takes in a dataset and a value for alpha (0 for L2
# regularization and 1 for L1 regularization) and returns a logistic
# regression classification model using the below specified attributes.
# --------------------------------------------------------------
create_model <- function(data, alpha_val) {
  x <- new_x(data)
  y <- data$target
  model <- glmnet(x, y, alpha = alpha_val, nlambda = 10, family = 'binomial')
  return(model)
}

# ------------------------------------------------------------------
# Function: evaluate_model
# Description: Takes as parameters a logistic regression model and a 
# validation set. Makes predictions with the model on the validation
# set and computes the AUC for each value of lambda. It returns
# the highest AUC value and the corresponding value of lambda.
# ------------------------------------------------------------------
evaluate_model <- function(model, valid_set) {
  new_x <- new_x(valid_set)
  predictions = predict(model, newx = new_x, type = 'response')
  auc <- apply(predictions, 2, compute_auc, valid_set$target)
  best_auc <- max(auc)
  lambda_index <- match(best_auc, auc)
  lambda <- model$lambda[lambda_index]
  return(c(best_auc, lambda))
}


# --------------------------------------------------------
# Function: pick_model
# Description: Takes in both L1 and L2 models and returns
# the one with better performance on the validatioon set.
# --------------------------------------------------------
pick_model <- function(L1, L2, validation_set) {
  L1_results <- evaluate_model(L1, validation_set)
  L2_results <- evaluate_model(L2, validation_set)
  if (L1_results[1] >= L2_results[1]) {
    return(L1)
  }
  return(L2)
}

# ----------------------------------------------------------------
# Function: test_model
# Description: This function takes in a model, specific value for 
# lambda, and a dataframe, and returns the results (AUC) of the model for
# the given dataset. Also plots ROC curve.
# ----------------------------------------------------------------
test_model <- function(model, lambda, test_set, model_name) {
  predictions <- predict(model, newx = new_x(test_set), s = lambda, data, type = 'response')
  plot_roc(predictions, test_set$target, model_name)
  return(compute_auc(predictions, test_set$target))
}

### Use the create_model function to train two models on each dataset.
### One using L1 regularization and one using L2 regularization.
full_model_L1 <- create_model(train, 1)
full_model_L2 <- create_model(train, 0)
sixties_model_L1 <- create_model(train_sixties, 1)
sixties_model_L2 <- create_model(train_sixties, 0)
seventies_model_L1 <- create_model(train_seventies, 1)
seventies_model_L2 <- create_model(train_seventies, 0)
eighties_model_L1 <- create_model(train_eighties, 1)
eighties_model_L2 <- create_model(train_eighties, 0)
nineties_model_L1 <- create_model(train_nineties, 1)
nineties_model_L2 <- create_model(train_nineties, 0)
thousands_model_L1 <- create_model(train_thousands, 1)
thousands_model_L2 <- create_model(train_thousands, 0)
tens_model_L1 <- create_model(train_tens, 1)
tens_model_L2 <- create_model(train_tens, 0)




#### Evaluation and Selection of Models ####

### Evaluate each model and pick the best performing one for each dataset
### to serve as the final model to be used on its corresponding test set.

full_model <- pick_model(full_model_L1, full_model_L2, valid)
full_model_results <- evaluate_model(full_model, valid)
full_model_results

sixties_model <- pick_model(sixties_model_L1, sixties_model_L2, valid_sixties)
sixties_model_results <- evaluate_model(sixties_model, valid_sixties)
sixties_model_results

seventies_model <- pick_model(seventies_model_L1, seventies_model_L2, valid_seventies)
seventies_model_results <- evaluate_model(seventies_model, valid_seventies)
seventies_model_results

eighties_model <- pick_model(eighties_model_L1, eighties_model_L2, valid_eighties)
eighties_model_results <- evaluate_model(eighties_model, valid_eighties)
eighties_model_results

nineties_model <- pick_model(nineties_model_L1, nineties_model_L2, valid_nineties)
nineties_model_results <- evaluate_model(nineties_model, valid_nineties)
nineties_model_results

thousands_model <- pick_model(thousands_model_L1, thousands_model_L2, valid_thousands)
thousands_model_results <- evaluate_model(thousands_model, valid_thousands)
thousands_model_results

tens_model <- pick_model(tens_model_L1, tens_model_L2, valid_tens)
tens_model_results <- evaluate_model(tens_model, valid_tens)
tens_model_results


### Create vector of coefficient names
coefficients <- c('Intercept', 'Danceability', 'Energy', 'Loudness', 'Minor Mode', 'Speechiness', 'Acousticness',
                  'Instrumentalness', 'Liveness', 'Valence', 'Tempo', 'Duration', 'Chorus_Hit', 'Sections')


# ----------------------------------------------------------------
# Function: create_coef_df
# Description: Creates a dataframe consisting of the coefficients
# for the model and their titles
# ----------------------------------------------------------------
create_coef_df <- function(model, results, coef_names) {
  df <- data.frame(coef_names, as.vector(coef(model, s = results[2])))
  colnames(df)[2] <- "value"
  return(df)
}


# --------------------------------------------------------------------
# Functon: plot_coef
# Description: Plots a bar graph of the coefficients for a given model.
# --------------------------------------------------------------------
plot_coef <- function(coef_df, model_name) {
  ggplot(coef_df, mapping = aes(x = coefficients, y = value)) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 70, hjust = 1), plot.title = element_text(hjust = 0.5)) +
    ylim(-15, 10) +
    ggtitle(model_name)
}

coef_full <- create_coef_df(full_model, full_model_results, coefficients)
coef_60s <- create_coef_df(sixties_model, sixties_model_results, coefficients)
coef_70s <- create_coef_df(seventies_model, seventies_model_results, coefficients)
coef_80s <- create_coef_df(eighties_model, eighties_model_results, coefficients)
coef_90s <- create_coef_df(nineties_model, nineties_model_results, coefficients)
coef_00s <- create_coef_df(thousands_model, thousands_model_results, coefficients)
coef_10s <- create_coef_df(tens_model, tens_model_results, coefficients)

## Plot Coefficient values for each model
plot_coef(coef_60s, '1960s Model')
plot_coef(coef_70s, '1970s Model')
plot_coef(coef_80s, '1980s Model')
plot_coef(coef_90s, '1990s Model')
plot_coef(coef_00s, '2000s Model')
plot_coef(coef_10s, '2010s Model')
plot_coef(coef_full, 'Decade-Spanning Model')

coef_vals <- data.frame(coefficients,
                        as.vector(coef(sixties_model, s = sixties_model_results[2])),
                        as.vector(coef(seventies_model, s = seventies_model_results[2])),
                        as.vector(coef(eighties_model, s = eighties_model_results[2])),
                        as.vector(coef(nineties_model, s = nineties_model_results[2])),
                        as.vector(coef(thousands_model, s = thousands_model_results[2])),
                        as.vector(coef(tens_model, s = tens_model_results[2]))
                        )
colnames(coef_vals)[2] <- 'Sixties_Model'
colnames(coef_vals)[3] <- 'Seventies_Model'
colnames(coef_vals)[4] <- 'Eighties_Model'
colnames(coef_vals)[5] <- 'Nineties_Model'
colnames(coef_vals)[6] <- 'Thousands_Model'
colnames(coef_vals)[7] <- 'Tens_Model'

coef_vals <- reshape2::melt(coef_vals, id.var = 'coefficients')


# -------------------------------------------------------------
# Function: plot_coefs_over_time
# Description: Takes as parameters a dataframe of coefficient 
# values for each decade's respective model and a specific feature.
# Creates a bar graph of the evolution of that specific feature's 
# importance in determining a hit over time.
# -------------------------------------------------------------
plot_coef_over_time <- function(coefs, feature) {
  ggplot(filter(coefs, coefficients == feature), aes(x = variable, y = value)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(feature)
}

plot_coef_over_time(coef_vals, 'Danceability')
plot_coef_over_time(coef_vals, 'Energy')
plot_coef_over_time(coef_vals, 'Loudness')
plot_coef_over_time(coef_vals, 'Minor Mode')
plot_coef_over_time(coef_vals, 'Speechiness')
plot_coef_over_time(coef_vals, 'Acousticness')
plot_coef_over_time(coef_vals, 'Instrumentalness')
plot_coef_over_time(coef_vals, 'Liveness')
plot_coef_over_time(coef_vals, 'Valence')
plot_coef_over_time(coef_vals, 'Tempo')
plot_coef_over_time(coef_vals, 'Duration')
plot_coef_over_time(coef_vals, 'Chorus_Hit')
plot_coef_over_time(coef_vals, 'Sections')

#######################################
#### TEST MODELS FOR FINAL RESULTS ####


# ----------------------------------------------------------
# Function: test_model
# Description: Takes as parameters a model, a lambda value, 
# a model name, and a dataset for the model to be tested on. 
# The function makes predictions and then plots the ROC curve
# and returns the AUC value.
# ----------------------------------------------------------
test_model <- function(model, lambda, test_set, model_name) {
  predictions <- predict(model, newx = new_x(test_set), s = lambda, data = test_set, type = 'response')
  plot_roc(predictions, test_set$target, model_name)
  return(compute_auc(predictions, test_set$target))
}

sixties_train_results <- test_model(sixties_model, sixties_model_results[2], 
                                    train_sixties, '1960s Model (Training Set)')
sixties_test_results <- test_model(sixties_model, sixties_model_results[2], 
                                   test_sixties, '1960s Model (Test Set)')

seventies_train_results <- test_model(seventies_model, seventies_model_results[2], 
                                    train_seventies, '1970s Model (Training Set)')
seventies_test_results <- test_model(seventies_model, seventies_model_results[2], 
                                   test_seventies, '1970s Model (Test Set)')

eighties_train_results <- test_model(eighties_model, eighties_model_results[2], 
                                    train_eighties, '1980s Model (Training Set)')
eighties_test_results <- test_model(eighties_model, eighties_model_results[2], 
                                   test_eighties, '1980s Model (Test Set)')

nineties_train_results <- test_model(nineties_model, nineties_model_results[2], 
                                    train_nineties, '1990s Model (Training Set)')
nineties_test_results <- test_model(nineties_model, nineties_model_results[2], 
                                   test_nineties, '1990s Model (Test Set)')

thousands_train_results <- test_model(thousands_model, thousands_model_results[2], 
                                      train_thousands, '2000s Model (Training Set)')
thousands_test_results <- test_model(thousands_model, thousands_model_results[2], 
                                     test_thousands, '2000s Model (Test Set)')

tens_train_results <- test_model(tens_model, tens_model_results[2], 
                                    train_tens, '2010s Model (Training Set)')
tens_test_results <- test_model(tens_model, tens_model_results[2], 
                                   test_tens, '2010s Model (Test Set)')

full_train_results <- test_model(full_model, full_model_results[2],
                                 train, 'Decade-Spanning Model (Training Set)')
full_test_results <- test_model(full_model, full_model_results[2],
                                 test, 'Decade-Spanning Model (Test Set)')




### Computing Predictions on Full Datasets ###


# ------------------------------------------------------------
# Function: make_predictions
# Description: Add column of predictions to dataframe
# ------------------------------------------------------------
make_predictions <- function(df, model, model_results) {
  return(mutate(df, prob_hit = predict(model, newx = new_x(df), 
                s = model_results[2], data = df, type = 'response')))
}


sixties <- make_predictions(sixties, sixties_model, sixties_model_results)
seventies <- make_predictions(seventies, seventies_model, seventies_model_results)
eighties <- make_predictions(eighties, eighties_model, eighties_model_results)
nineties <- make_predictions(nineties, nineties_model, nineties_model_results)
thousands <- make_predictions(thousands, thousands_model, thousands_model_results)
tens <- make_predictions(tens, tens_model, tens_model_results)
full <- make_predictions(full, full_model, full_model_results)


### Find songs that should not have been hits (according to our models) but were.
### Call these 'mold-breakers'

# -----------------------------------------------------------
# Function: find_mold_breakers
# Description: Given a dataframe, returns the songs which had 
# a very low probability of becoming a hit (according to our model),
# but became hits regardless.
# -----------------------------------------------------------
find_mold_breakers <- function(df) {
  return(filter(df, prob_hit <= 0.1 & hit == TRUE))
}

sixties_mold_breakers <- find_mold_breakers(sixties)
seventies_mold_breakers <- find_mold_breakers(seventies)
eighties_mold_breakers <- find_mold_breakers(eighties)
nineties_mold_breakers <- find_mold_breakers(nineties)
thousands_mold_breakers <- find_mold_breakers(thousands)
tens_mold_breakers <- find_mold_breakers(tens)

mold_breakers <- rbind(sixties_mold_breakers, seventies_mold_breakers, 
                       eighties_mold_breakers, nineties_mold_breakers, 
                       thousands_mold_breakers, tens_mold_breakers)

mold_breakers <- mold_breakers %>% 
  as.data.table() %>% 
  arrange(prob_hit)

nrow(sixties_mold_breakers) / nrow(sixties)
nrow(seventies_mold_breakers) / nrow(seventies)
nrow(eighties_mold_breakers) / nrow(eighties)
nrow(nineties_mold_breakers) / nrow(nineties)
nrow(thousands_mold_breakers) / nrow(thousands)
nrow(tens_mold_breakers) / nrow(thousands)



####################################################################
########################### STOP HERE ##############################
####################################################################


#### PART 2: Linear regression to predict popularity ####

pop_data <- read_csv("~/Desktop/spotify-audio-features_popularity/SpotifyAudioFeaturesApril2019.csv")

lr_model <- lm(popularity ~ 1 + danceability + energy + loudness + mode +
                 speechiness + acousticness + instrumentalness + liveness + valence +
                 tempo + duration_ms, pop_data)

coef(lr_model)

coefficient_vec <- list('danceability', 'energy', 'loudness', 'mode', 'speechiness', 'acousticness',
                  'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')
correlations <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

for (i in 1:11) {
  correlations[i] <- cor(pop_data$coefficient_vec[i], pop_data$popularity)
}


ggplot(pop_data) +
  geom_smooth(aes(x = danceability, y = popularity), color = 'red') +
  geom_smooth(aes(x = energy, y = popularity))

ggplot(pop_data) +
  geom_smooth(aes(x = duration_ms, y = popularity))






