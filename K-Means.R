library(dplyr)



##### Input Arguments
#cluster_data  - Data for which clustering has to be done (Note: All the variables are supposed to be numeric)
#k-value  - number of clusters
#columns - the columns in the data(numbers) for which the clustering has to be done

cluster.kmeans <- function(cluster_data, k_value, columns = NA){
  
  
  ## The columns are not specified we take all the columns
  if(is.na(columns)){
    columns <- 1:length(cluster_data)
  }
  
  
  ##### Choosing k random point in the n-dimentional space
  Random_point <- sapply(cluster_data[columns],
                         function(x)sample(seq(min(x),max(x),0.01),k_value)) %>% 
                                data.frame()
  
  ### Assigning cluster number
  Random_point$cluster <- 1:k_value
  
  
  
  ### FUNCTION to calculate or re-calculate the clusters based on the random points
  ## and the actual data
  
  ### Passing in the actual cluster data and the Random/Centroid values
  Nearest_neighbours <- function(cluster_data,Random_point,columns){
  
  ###### Selectting the nearest cluster centroid with respect to the k-points (Euclidean distance)
  clusters <- apply(cluster_data[,columns],1,function(x)
                        which.min(sqrt(colSums((data.frame(t(Random_point[,columns])) 
                              - data.frame(x)[[1]])^2))))
  
  return(clusters)
  
  }
  
  
  
  ########### IMPLEMENTING K-MEANS ###########################################
  
  k_means_list <- list()
  repeat_kmeans = TRUE
  
  ### Assigning each observation a random value
  cluster_data$clusters <- sample(1:k_value,nrow(cluster_data),replace = TRUE)
  
  
  counter <- 0
  
  
  ### BEGINNNING THE LOOP ##
  while(repeat_kmeans){
    
    print(counter)
    counter <- counter + 1
    # storing the values of previous clusters  
    previous_clusters <- cluster_data$clusters
    
    ## Calling the function to find the nearest clusters  
    cluster_data$clusters <- Nearest_neighbours(cluster_data,Random_point,columns)
    
    # Values of new clusters
    new_clusters <- cluster_data$clusters
    
    #Comparing old and new clusters for changes
    if(!"FALSE" %in% names(table(new_clusters == previous_clusters))){
      #Checking if there are atleast one difference between the old and new values
      #If no difference - we set the flag repeat_kmeans to FALSE to stop the loop
      
      repeat_kmeans <- FALSE
      
      
  }
  
  
  ## Calculating Centroid for each cluster 
  Random_point <- select(cluster_data,columns,clusters) %>%
          group_by(clusters) %>% summarise_all(mean) %>% select(-clusters,clusters) %>% data.frame()
  
  
  k_means_plot <- ggplot(cluster_data, aes(cluster_data[[1]],cluster_data[[2]], 
                           col = factor(clusters))) +
    geom_point() + 
    annotate("point", x = Random_point$Sepal.Length, y =Random_point$Sepal.Width, colour = "black") +
    xlab("Sepal.Length") + ylab("Sepal.Width")
  
  k_means_list[[counter]] <- k_means_plot
  
  
  
  }
  l = mget("k_means_list")
  pdf("K-means_convergence.pdf")
  invisible(lapply(l, print))
  
  dev.off()
  
  
  
}