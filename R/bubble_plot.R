#' Visualisation of co-ocurrence of indicators in bubble chart
#'
#' co ocurrence chart of two variables 
#'
#' @param database corresponds to the database where the information is stored
#' @param v1 corresponds to a indicator that will be plotted against the second variable. 
#' This value should be defined as.factor when plotting
#' @param v2 corresponds to the second indicator that will be plotted.
#' This value should be defined as.factor when plotting 
#' @param v3 It is a numerical value that will be used to fill in the size and the color of the bubbles in the chart
#' 
#' 
#' 
#' @return plot
#' @export
#'
#' @examples  
#' 
#' data("ind_scenarios")
#' bubble_plot(database = ind_scenarios,v1= "NCP", v2= "rewilding", v3= "sum_counteracting")
#' 
#' bubble_plot(database = ind_scenarios,v1= "rewilding", v2= "NCP", v3= "sum_counteracting")
#' 
#' @importFrom ggplot2 ggplot geom_point scale_colour_gradientn theme aes element_blank element_blank element_rect geom_bar theme_bw element_text .data
#' @importFrom checkmate assertDataFrame assert_character assertNumeric
#' 

bubble_plot <- function(database,v1, v2, v3 )  {
  # assert input
  assertDataFrame(database)
  assert_character(v1)
  assert_character(v2)
  assert_character(v3)
  
  #define legend size and color
  palm2 <-c("#FFE3B3","#47C0A7","#085389")
  legend_size <- c(1,3,5,8,10,13)
  #plot
  c <- ggplot(database, aes(.data[[v1]], .data[[v2]]))
  c2 <- c +  geom_point(aes(colour = .data[[v3]], size = .data[[v3]])) +
    scale_colour_gradientn(colours =  palm2 , guide = "legend")
  c3<-c2 + theme(legend.position="bottom") + 
    theme(legend.key=element_blank(), 
          axis.title.x = element_text(colour = "black", face = "bold", size = 12),
          axis.title.y = element_text(colour = "black", face = "bold", size = 12),
          axis.text.x = element_text(colour = "black", size = 10, angle = 60, vjust=.85, hjust=0.85), 
          axis.text.y = element_text(colour = "black", size = 10), 
          legend.text = element_text(size = 10, face ="bold", colour ="black"), 
          legend.title = element_text(size = 12, face = "bold"), 
          panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
          legend.position = "right") 
  
  return(c3)
}