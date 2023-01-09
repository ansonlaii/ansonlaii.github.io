
## Helper Functions

dmc_helper <- function(col){
  
  ## dmc_helper(col) takes a vector of the hex code of colors and returns
  ## a vector containing the corresponding nearest DMC color hex code. This 
  ## function is created to help with the main function "process_image".
  ##
  ## input:
  ## - col: A vector of hex codes of colors (character type vector)
  ##
  ## output:
  ## - A vector of same length as the input but this time all the color hex codes
  ## are converted to their corresponding nearest DMC color hex code.
  ## 
  ## Example:
  ##        library(dmc)
  ##        hex_colors <- c("#000000", "#0000FF")
  ##        dmc_colors <- dmc_helper(hex_colors)
  
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc\") and then try again.")
  }
  dmc_colors <- character()
  for(i in col) {
    dmc_colors <- append(dmc_colors, dmc(i, visualize = FALSE)$hex )
  }
  return(dmc_colors)
}


dmc_helper2 <- function(col){
  
  ## dmc_helper2(col) takes a vector of the hex code of colors and returns
  ## a vector containing the corresponding nearest DMC color name. This 
  ## function is created to help with the main function "process_image". The 
  ## difference between this and dmc_helper is that this function returns the name
  ## instead of the dmc hex code.
  ##
  ## input:
  ## - col: A vector of hex codes of colors (character type vector)
  ##
  ## output:
  ## - A vector of same length as the input but this time all the color hex codes
  ## are converted to their corresponding nearest DMC color name.
  ## 
  ## Example:
  ##        library(dmc)
  ##        hex_colors <- c("#000000", "#0000FF")
  ##        dmc_color_names <- dmc_helper2(hex_colors)
  ##        > dmc_color_names
  ##        [1] "Black"      "Royal Blue"
  
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc\") and then try again.")
  }
  dmc_colors2 <- character()
  for(i in col) {
    dmc_colors2 <- append(dmc_colors2, dmc(i, visualize = FALSE)$name )
  }
  return(dmc_colors2)
}


tidied_with_color <- function(k_clust){
  
  ## tided_with_color(k_clust) is combined with the two other helper functions
  ## above to add three columns to the output of tidy(k_clust). tidy(kclust) outputs
  ## the cluster information for k means and this function would add three more columns
  ## in addition to this output which is "col", "dmc_col", and "dmc_col_name". They are
  ## the DMC color for each cluster. This helper function works with the other two above
  ## to help make computations easier on process_image function.
  ## 
  ## input:
  ## - kclust: The output after running k means clustering on data with RGB values.
  ##(ie. after using kmeans())
  ## 
  ## output:
  ## - The tibble from tidy(kclust) but with three additional columns, namely "dmc_col", "col"
  ## and "dmc_col_name". They are the nearest DMC color information.
  ## 
  ## Example:
  ##        kclust <- kmeans(data, centers=4, nstart=4)
  ##        tibble_with_dmc_col_info <- tidied_with_color(kclust)
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  
  centres <- tidy(k_clust) %>% mutate(col = rgb(R,G,B))
  centres <- centres %>% mutate(dmc_col = dmc_helper(col))
  centres <- centres %>% mutate(dmc_col_name = dmc_helper2(col))
  return(centres)
}

## Main Functions

process_image <- function(image_file_name, k_list)
{
  ## process_image(image_file_name, k_list) first loads the jpeg/png image 
  ## and computes the (R,G,B) values necessary to do a k-means clustering.
  ## It then computes the k-means clustering for different k values in k_list 
  ## and summarizes this k-means process with a tibble. Each tibble row is 
  ## information for the k-means clustering with k centroids where k is just 
  ## a value from k_list. The information provided for each k cluster will be 
  ## enough to compute the scree plot, color strip, and make the pattern later on. 
  ## It is all the cluster information we need.
  ## 
  ## input:
  ## - image_file_name: must be a JPEG or png image. Insert in quotes the name of it.
  ## - k_list: a numeric vector containing a bunch of integers. Those integers are
  ## the desired number of cluster centers to do a k-means clustering on.
  ## 
  ## output:
  ## - A tibble of information where each row has information on the k-means clustering
  ## on k number of cluster centers. The column will be information about each cluster,
  ## about each point, and about the clustering as a whole. Information for each cluster
  ## will also contain their RGB color and nearest DMC color.
  ##
  ## Example:
  ##        number_of_cluster_center <- c(2,3,4,5,6,7,8,9,10)
  ##        cluster_info <- process_image("Marilyn_Monroe.jpg", number_of_cluster_center)
  
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  
  image <- imager::load.image(image_file_name)
  tidy_image <- as.data.frame(image, wide="c") %>% rename(R=c.1,G=c.2,B=c.3)
  drop_xy <- select(tidy_image, c(-x,-y))
  kclusts_info <- tibble(k=k_list) %>% mutate(
            kclust = map(k, ~kmeans(x=drop_xy, centers=.x, nstart=5)),
            tidied = map(kclust, tidied_with_color),
            glanced = map(kclust, glance),
            augmented = map(kclust, augment, tidy_image)
  )
  
  return(kclusts_info)
}


scree_plot <- function(cluster_info)
{
  ## scree_plot(cluster_info) produces and plots a scree plot with the k-means
  ## clustering information derived from process_image function. "process_image"
  ## outputs information about the clustering as a whole so information like 
  ## within sum of square is available to compute the scree plot.
  ##
  ## input:
  ## - cluster_info: A tibble of information for the k-means clustering for different K's.
  ## This input should be the output of function process_image.
  ##
  ## output:
  ## - A scree plot with the y axis being the total within sum of squares and the x
  ## axis is the number of cluster centers.
  ##
  ## Example:
  ##        cluster_info <- process_image("Marilyn_Monroe.jpg", c(2:10))
  ##        scree_plot(cluster_info)
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }

  clusterings <- cluster_info %>% unnest(cols = c(glanced))
  ggplot(clusterings, aes(k, tot.withinss)) + geom_line() + geom_point()
  
}


color_strips <- function(cluster_info)
{
  ## Recall that process_image produces cluster information for the k-means 
  ## clustering of k centers. With the cluster information we got from process_image,
  ## color_strips(cluster_info) will produce color strips with the DMC color closest
  ## to the cluster center color. These colors will be the potential thread color
  ## and background color for our pattern to be produced later.
  ##
  ## Input:
  ## - cluster_info: The tibble of k-means clustering summary produced from process_image.
  ##
  ## Output:
  ## - Bunch of color strips representing the DMC colors for clusters.
  ##
  ## Example:
  ##        cluster_info <- process_image("Marilyn_Monroe.jpg", c(6,7))
  ##        color_strips(cluster_info)
  ##        Note: This will produce one color strip with 6 DMC colors and another 
  ##        color strip with 7 DMC colors.
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  if(!require(scales)) {
    stop("The scales packages must be installed. Run install.packages(\"scales\") and then try again.")
  }
  
  clusters <- cluster_info %>% unnest(cols = c(tidied))
  par(mfrow=c(1,length(cluster_info$k)))
  for (i in cluster_info$k) {
    show_col(filter(clusters, k == i)$dmc_col)
    
  }
  
}


make_pattern <- function(cluster_info, a, x_size, black_white=FALSE, 
                         background_colour = NULL)
{
  ## Using the cluster information from process_image, make_pattern(...) will 
  ## plot the cross stitch pattern given the chosen cluster size. We already 
  ## know the DMC color/thread color associated with the clusters and the cluster
  ## each point is associated to. With this information, the function will first
  ## reduce the resolution using change_resolution() provided, and then plot the 
  ## cross stitch pattern. The pattern is allowed to be black/white and this 
  ## function can plot the cross stitch pattern ignoring the background of the 
  ## image. (Not providing a cross stitch pattern for the background since it's
  ## not needed).
  ## 
  ## Input:
  ##
  ## - cluster_info: The output of process image.
  ##
  ## - a:            The chosen cluster size. Must be a cluster size from output 
  ##                 of process_image.
  ##
  ## - x_size:       The approximate total number of possible stitches in the 
  ##                 horizontal direction.
  ##
  ## - black_white:   (logical) Print the pattern in black and white (TRUE) or 
  ##                 color (FALSE,default).
  ##
  ##
  ## - background_colour: The color of the background, which should not be stitched 
  ##                      in the pattern. (Default is to not have a background colour)
  ##                      The color of background provided should be in hex code.
  ##                      For example, "#000000" for the color black.
  ##
  ## Output:
  ## - A cross stitch pattern that can be followed with a legend to show thread color
  ##   and a guide grid to easily follow the pattern.
  ##
  ## Example:
  ##        cluster_info <- process_image("Marilyn_Monroe.jpg", c(2,10))
  ##        make_pattern(cluster_info, 7, 60, background_colour = "#FF5773")
  ##        make_pattern(cluster_info, 6, 65, black_white = TRUE)
  
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }

  chosen_clustering <- filter(cluster_info, k == a)
  tidy_dat <- chosen_clustering[1,5][[1]][[1]] %>% rename(cluster = .cluster)
  tidy_dat <- select(tidy_dat, c(-R,-G,-B))
  tidy_dat <- as.data.frame(tidy_dat, wide = "c")
  
  agg_image <- change_resolution(tidy_dat, x_size)
  cluster_centre <- chosen_clustering[1,3][[1]][[1]]
  
  data <- merge.data.frame(agg_image, cluster_centre)
  data <- select(data, c(-R,-G,-B,-size, -withinss,-col))
  
  if (!is.null(background_colour) == TRUE){
    background_dmc_hex <- background_colour
    data <- filter(data, dmc_col != background_dmc_hex)
    cluster_centre <- filter(cluster_centre, dmc_col != background_dmc_hex)}
  
  if (black_white == TRUE){
    
    black <- rep("black", each = length(cluster_centre$cluster))
    
    data %>% ggplot(aes(x,y)) + geom_point(aes(col=factor(cluster),shape=factor(cluster))) +
      scale_color_manual(values = black, label = cluster_centre$dmc_col_name) +
      scale_shape_manual(values=cluster_centre$cluster, label=cluster_centre$dmc_col_name) +
      scale_y_reverse() + theme_bw() + theme(axis.title = element_blank(), 
      axis.text = element_blank(), axis.ticks = element_blank(), legend.title = element_blank(), 
      legend.position="bottom", panel.grid.major = element_line(colour = "black", size = 1.2), 
      legend.key = element_rect(fill = "white", colour = "black"), panel.border = element_blank())
    
  } else {
    
    data %>% ggplot(aes(x,y)) + geom_point(aes(col=factor(cluster),shape=factor(cluster))) +
      scale_color_manual(values=cluster_centre$dmc_col, label=cluster_centre$dmc_col_name) +
      scale_shape_manual(values=cluster_centre$cluster, label=cluster_centre$dmc_col_name) +
      scale_y_reverse() + theme_bw() + theme(axis.title = element_blank(), 
      axis.text = element_blank(), axis.ticks = element_blank(), legend.title = element_blank(), 
      legend.position="bottom", panel.grid.major = element_line(colour = "black", size = 1.2), 
      legend.key = element_rect(fill = "white", colour = "black"), panel.border = element_blank())
    
  }
  
}


# Function Provided

change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}






