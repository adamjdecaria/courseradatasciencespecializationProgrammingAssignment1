# Adam DeCaria
# Coursera Data Science Specialization
# R Programming Course
# Programming Assignment 1

#Assignment 1 Part 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE)

  dat <- data.frame()

  for (i in 1:332) {
    dat <- rbind(dat, read.csv(files_list[i]))
    }
    dat_subset <- dat[which(dat[, "ID"] %in% id),]
    mean(dat_subset[, pollutant], na.rm = TRUE)
} #end pollutantmean()

#Assignment 1 Part 2
complete <- function(directory, id = 1:332) {
   files_list <- list.files(directory, full.names = TRUE)

   dat <- data.frame()
   dat_subset = data.frame()
   id <- c(id)
   nobs <- c()

   for (i in id) {

     dat <- rbind(dat, read.csv(files_list[i]))
     cleandat <- dat[complete.cases(dat),]

   }

   column <- c(id)

  for (c in column) {

    rowcount <- nrow(cleandat[which(cleandat[,"ID"] %in% c),])
    nobs <- append(nobs, rowcount)
  }

  dat_subset <- cbind(id, nobs)
  dat_subset
} #end complete()

#Assignment 1 Part 3
corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names = TRUE)

  dat <- data.frame()
  cleandat <- data.frame()
  correlation <- c()
  x <- vector(mode="numeric", length = 0)

  for (count in 1:length(files_list)) {

    dat <- read.csv(files_list[count])
    cleandat <- dat[complete.cases(dat),]

    sulfate_data <- cleandat$sulfate
    nitrate_data <- cleandat$nitrate

    if (nrow(cleandat) > threshold) {
      correlation <- append(correlation, cor(sulfate_data, nitrate_data))
    }

    sulfate_data <- NULL
    nitrate_data <- NULL

  }
  correlation
} #end corr()
