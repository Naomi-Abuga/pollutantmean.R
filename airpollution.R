pollutantmean <- function(directory,pollutant,id= 1:332){
  all_file <- list.files(directory,full.names= TRUE)
  dat = data.frame()
  for(i in id){
    dat <- rbind(dat,read.csv(all_files[i]))
  }
  mean(dat[[pollutant]], na.rm = TRUE)
}
pollutantmean("Specdata","sulfate",1)
pollutantmean("Specdata","sulfate")
pollutantmean("Specdata","sulfate",1:10)
pollutantmean("Specdata","nitrate")
pollutantmean("Specdata","sulfate",34)


complete <- function(directory, id = 1:332){
  all_file <- list.files(directory,full.names= TRUE)
  res = data.frame()
  for(i in id){
    dat1 <- read.csv(all_files[i])
    y <- sum(complete.cases(dat1))
    x <- sum(!complete.cases(dat1))
    res <- rbind(res, data.frame(Id= i, complete_obs = y, missing_obs = x, Total_obs= y+x))
  }
  res
}


correlation <- function(directory, threshold =0){
  all_file <- list.files(directory,full.names= TRUE)
  dat = data.frame()
  for (i in 1:332){
    dat1 <- read.csv(all_files[i])
    if(sum(complete.cases(dat1))>threshold){
      dat <- rbind(dat,dat1)
    }
  }
  if (nrow(dat)==0){
    message("no mentor with that threshold")
  }else{
    cor(da[[2]],dat[[3]],use = "pairwise.complete.obs")
  }
}
