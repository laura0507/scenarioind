#' Visualization of group of indicators in scenarios
#' 
#' basic histogram to visualize how many times an indicator was found in the database
#' 
#' @param x corresponds to the database where the information is stored
#' @param t is the variable in the database that will be used to plot the values,
#'           when defining variable write t = "name_variable"
#' 
#' 
#' @return plot
#' @export
#'
#' @examples  
#' 
#' data("ind_scenarios")
#' count_ind(x=ind_scenarios, t="NCP")
#' 
#' 
#' 
#' @importFrom dplyr count group_by %>% n
#' @importFrom ggplot2 ggplot .data
#' @importFrom checkmate assertDataFrame assert_character
#' 

count_ind<- function(x,t){
  
  #assert input
  assertDataFrame(x)
  assert_character(t)
  #count individuals
  db <- x %>%
    group_by(.data[[t]]) %>%
    count()
  
  #plot barchsrt
  ggplot(data= db) + geom_bar(aes(x= .data[[t]], y=n), stat="identity")+
    theme_bw()+
    theme(axis.text = element_text(angle = 90))
  return(db)
}