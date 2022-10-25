#'@title Barplot from the variables obtained in the dvalue 
#'@name barDom
#'
#'@description Generates a barplot from the variables obtained in the dvalue
#'             function (dominance value, social hierarchy and social rank)
#'
#'@param dvalue Dominance value
#'@param variable the column with social hierarchy or social rank information.
#'
#'@details It is a function that plots the social hierarchy or social rank information of a group 
#'         in a barplot. The function uses the options provided by ggplot2.
#'          
#'
#'@return histogram of social dominance
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'            
#'@importFrom data.table data.table
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 geom_bar
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 element_rect
#'@importFrom ggplot2 element_line
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom stats na.omit
#'
#'@examples "There is no example"
#'NULL
#'
#'@export
barDom <- function(dvalue,variable){
  ggplot(dvalue, aes(variable)) +
    geom_bar(width=0.6)+
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),
          axis.line.x = element_line(colour = "black", 
                                     size=0.6),
          axis.line.y = element_line(colour = "black", size=0.6))+
    scale_y_continuous(expand = c(0, 0))
}


#'@title Boxplot 
#'@name bpDom
#'
#'@description Function to obtain the boxplot of social hierarchy categories
#'             from dvalue function.
#'
#'@param y the column with animal information.
#'@param x the column with social hierarchy or social rank information.
#'
#'
#'@details It is a simple function that plots the social hierarchy or social rank information of a group 
#'         in a boxplot. The function uses the options provided by ggplot2.
#'          
#'
#'@return boxplot of social hierarchy or social rank
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'            
#'@importFrom data.table data.table
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 stat_boxplot
#'@importFrom ggplot2 geom_boxplot
#'@importFrom ggplot2 theme
#'@importFrom ggplot2 element_rect
#'@importFrom ggplot2 element_line
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 xlab
#'@importFrom ggplot2 ylab
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom stats na.omit
#'
#'
#'@examples "There is no example"
#'
#'NULL
#'
#'@export
bpDom <- function(y,x){
  bp <- merge(y,x, by = "animal_id")
  ggplot(bp, aes(y = bp[,2], x = bp[,3])) +
    stat_boxplot(geom ='errorbar', width = 0.3, size = 0.6) +
    geom_boxplot(width = 0.5, fill = "lightgrey", size = 0.6) +
    theme(panel.background = element_rect(fill = 'white'),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),
          axis.line.x = element_line(colour = "black", 
                                     size=0.6),
          axis.line.y = element_line(colour = "black", 
                                     size=0.6),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    
    xlab("x") +
    ylab("y") +
    scale_y_continuous(expand = c(0, 0))
}


