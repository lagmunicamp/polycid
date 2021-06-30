#Input size
options(shiny.maxRequestSize=30*1024^2)

error_na <- function(data, erate, maxna){
  datavec <- as.vector(as.matrix(t(data)))
  snperror <- sample(1:length(datavec), max(erate*length(datavec),1))
  for(x in 1:length(snperror)){
    if (is.na(datavec[snperror[x]])){
      error <- sample(c(1,2,3,4),1)
      datavec[snperror[x]] <- error
    }else if (datavec[snperror[x]] == 0){
      error <- sample(c(1,2,3,4),1)
      datavec[snperror[x]] <- error
    } else if (datavec[snperror[x]] == 1){
      error <- sample(c(0,2,3,4),1)
      datavec[snperror[x]] <- error
    } else if (datavec[snperror[x]] == 2){
      error <- sample(c(0,1,3,4),1)
      datavec[snperror[x]] <- error
    } else if (datavec[snperror[x]] == 3){
      error <- sample(c(0,1,2,4),1)
      datavec[snperror[x]] <- error
    } else if (datavec[snperror[x]] == 4){
      error <- sample(c(0,1,2,3),1)
      datavec[snperror[x]] <- error
    } else {
    }
  }
  data2 <- matrix(datavec, nrow=dim(data)[1], ncol=dim(data)[2], byrow = T)
  code <- as.character(1:dim(data)[1])
  rownames(data2) <- code
  colnames(data2) <- colnames(data)
  nas_samples <- sample(seq(0.01, maxna, by = 0.01), dim(data2)[1], replace= T)
  missnumber <- round(nas_samples*dim(data2)[2])
  for(y in 1:dim(data2)[1]){
    nas_pos <- sample(1:dim(data2)[2],missnumber[y])
    data2[y,nas_pos] <- NA
  }
  return(data2)
}

calculateGAI <- function(array1,array2){ return(sum(array1==array2,na.rm = T)/length(array1))}

calculateGAII <- function(array1,array2){ return(sum(array1==array2 & array1==0,na.rm = T)/sum(array1==0,na.rm = T))}

calculateGAIII <- function(ploidy,array1,array2,array3){
  if(ploidy == 4){
    count <- 0
    for(i in 1:length(array1)){
      if(is.na(array1[i]) | is.na(array2[i]) | is.na(array3[i])){ next}
      if((array1[i]==array2[i]) & (array1[i]==0)){count = count + (array3[i] == 1 | array3[i] == 2 | array3[i] == 3 | array3[i] == 4)}
      else if((array1[i]==array2[i]) & (array1[i]==1)){count = count + (array3[i] == 3 | array3[i] == 4)}
      else if((array1[i]==array2[i]) & (array1[i]==3)){count = count + (array3[i] == 0 | array3[i] == 1)}
      else if((array1[i]==array2[i]) & (array1[i]==4)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 3)}
      else if((array1[i]==0 & array2[i]==1) | (array1[i]==1 & array2[i]==0)){count = count + (array3[i] == 2 | array3[i] == 3 | array3[i] == 4)}
      else if((array1[i]==0 & array2[i]==2) | (array1[i]==2 & array2[i]==0)){count = count + (array3[i] == 3 | array3[i] == 4)}
      else if((array1[i]==0 & array2[i]==3) | ((array1[i]==3 & array2[i]==0))){count = count + (array3[i] == 0 | array3[i] == 3 | array3[i] == 4)}
      else if((array1[i]==0 & array2[i]==4) | (array1[i]==4 & array2[i]==0)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 3 | array3[i] == 4)}
      else if((array1[i]==1 & array2[i]==2) | (array1[i]==2 & array2[i]==1)){count = count + (array3[i] == 4)}
      else if((array1[i]==1 & array2[i]==3) | (array1[i]==3 & array2[i]==1)){count = count + (array3[i] == 0 | array3[i] == 4)}
      else if((array1[i]==1 & array2[i]==4) | (array1[i]==4 & array2[i]==1)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 4)}
      else if((array1[i]==2 & array2[i]==3) | (array1[i]==3 & array2[i]==2)){count = count + (array3[i] == 0)}
      else if((array1[i]==2 & array2[i]==4) | (array1[i]==4 & array2[i]==2)){count = count + (array3[i] == 0 | array3[i] == 1)}
      else if((array1[i]==3 & array2[i]==4) | (array1[i]==4 & array2[i]==3)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2)}
    }
    return(count/length(array1))
  }
  else if(ploidy == 6){
    count <- 0
    for(i in 1:length(array1)){
      if(is.na(array1[i]) | is.na(array2[i]) | is.na(array3[i])){ next}
      if((array1[i]==array2[i]) & (array1[i]==0)){count = count + (array3[i] == 1 | array3[i] == 2 | array3[i] == 3 | array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==array2[i]) & (array1[i]==1)){count = count + (array3[i] == 3 | array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==array2[i]) & (array1[i]==2)){count = count + (array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==array2[i]) & (array1[i]==4)){count = count + (array3[i] == 0 | array3[i] == 1)}
      else if((array1[i]==array2[i]) & (array1[i]==5)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 3)}
      else if((array1[i]==array2[i]) & (array1[i]==6)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 3 | array3[i] == 4 | array3[i] == 5)}
      else if((array1[i]==0 & array2[i]==1) | (array1[i]==1 & array2[i]==0)){count = count + (array3[i] == 2 | array3[i] == 3 | array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==0 & array2[i]==2) | (array1[i]==2 & array2[i]==0)){count = count + (array3[i] == 3 | array3[i] == 4| array3[i] == 5| array3[i] == 6)}
      else if((array1[i]==0 & array2[i]==3) | (array1[i]==3 & array2[i]==0)){count = count + (array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==0 & array2[i]==4) | (array1[i]==4 & array2[i]==0)){count = count + (array3[i] == 0 | array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==0 & array2[i]==5) | (array1[i]==5 & array2[i]==0)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==0 & array2[i]==6) | (array1[i]==6 & array2[i]==0)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==1 & array2[i]==2) | (array1[i]==2 & array2[i]==1)){count = count + (array3[i] == 4 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==1 & array2[i]==3) | (array1[i]==3 & array2[i]==1)){count = count + (array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==1 & array2[i]==4) | (array1[i]==4 & array2[i]==1)){count = count + (array3[i] == 0 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==1 & array2[i]==5) | (array1[i]==5 & array2[i]==1)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==1 & array2[i]==6) | (array1[i]==6 & array2[i]==1)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 5 | array3[i] == 6)}
      else if((array1[i]==2 & array2[i]==3) | (array1[i]==3 & array2[i]==2)){count = count + (array3[i] == 6)}
      else if((array1[i]==2 & array2[i]==4) | (array1[i]==4 & array2[i]==2)){count = count + (array3[i] == 0 | array3[i] == 6)}
      else if((array1[i]==2 & array2[i]==5) | (array1[i]==5 & array2[i]==2)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 6)}
      else if((array1[i]==2 & array2[i]==6) | (array1[i]==6 & array2[i]==2)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 6)}
      else if((array1[i]==3 & array2[i]==4) | (array1[i]==4 & array2[i]==3)){count = count + (array3[i] == 0)}
      else if((array1[i]==3 & array2[i]==5) | (array1[i]==5 & array2[i]==3)){count = count + (array3[i] == 0 | array3[i] == 1)}
      else if((array1[i]==3 & array2[i]==6) | (array1[i]==6 & array2[i]==3)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2)}
      else if((array1[i]==4 & array2[i]==5) | (array1[i]==5 & array2[i]==4)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2)}
      else if((array1[i]==4 & array2[i]==6) | (array1[i]==6 & array2[i]==4)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 3)}
      else if((array1[i]==5 & array2[i]==6) | (array1[i]==6 & array2[i]==5)){count = count + (array3[i] == 0 | array3[i] == 1 | array3[i] == 2 | array3[i] == 3 | array3[i] == 4)}
    }
    return(count/length(array1))
  }
}

calculateGAs <- function(markers,prog1,prog2,ploidy){
  cond_p1 <- row.names(markers) %in% prog1
  cond_p2 <- row.names(markers) %in% prog2
  cond_pop <- !(cond_p1 | cond_p2)
  pop <- markers[cond_pop,]
  p1 <- markers[cond_p1,]
  p2 <- markers[cond_p2,]
  results <- data.frame()
  
  results <- data.frame(GAIa=apply(pop,1,calculateGAI,array1=p1),
                        GAIb=apply(pop,1,calculateGAI,array1=p2),
                        GAIIa=apply(pop,1,calculateGAII,array1=p1),
                        GAIIb=apply(pop,1,calculateGAII,array1=p2),
                        GAIII=apply(pop,1,calculateGAIII,ploidy=ploidy,array1=p1,array2=p2))
  return(results)
}

findClusters <- function(GA){
  #Indexes to calculate
  indexes <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", "beale","ratkowsky", "ball", "ptbiserial", "gap", "mcclain", "gamma", "gplus", "tau", "dunn", "sdindex", "sdbw")
  
  #Saving results
  k_clust <- c()
  best_part <- data.frame()
  
  #Trying each one separately
  for(i in indexes){
    tryCatch({
      clustering <- NbClust(data=as.matrix(GA),distance = "euclidean", method="average", index = i)
      best_part <- rbind(best_part, clustering$Best.partition)
      k_clust <- c(k_clust, clustering$Best.nc[[1]])},error=function(e){}
    )
  } 
  
  k_freq <- data.frame(table(k_clust))
  k_freq <- arrange(k_freq, desc(Freq))
  
  #Results
  results <- data.frame()
  
  for(n in 1:3){
    count <- 0
    for(m in 1:length(k_clust)){
      if(k_clust[m] == k_freq[n,1] & count < 1){
        results <- rbind(results,as.factor(as.vector(unlist(best_part[m,]))))
        count <- 1
      }
    }
  }
  
  results <- data.frame(t(results))
  row.names(results) <- row.names(GA)
  colnames(results) <- c("Clusters1","Clusters2","Clusters3")
  indexes <- k_freq[1:3,]
  colnames(indexes) <- c("Number","Frequency")
  indexes$Clusters <- c("Clusters1","Clusters2","Clusters3")
  return(list(results,indexes))
}