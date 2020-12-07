#' Visualization of group of indicators in scenarios
#' 
#' @param x corresponds to the database where the information is stored
#' @param t is the variable in the database that will be used to plot the values,
#'           when defining variable write t = "name_variable"
#' 
#' 
#' @return 
#' @export
#'
#' @example 
#' 
#' data("ind_scenarios")
#' count_ind(x=ind_scenarios, t="NCP")
#' 
#' @importFrom dplyr count group_by
#' 

count_ind<- function(x,t){
  
  db <- x %>%
    group_by(.data[[t]]) %>%
    count()
  ggplot(data= db) + geom_bar(aes(x= .data[[t]], y=n), stat="identity")+
    theme_bw()+
    theme(axis.text = element_text(angle = 90))
  
}