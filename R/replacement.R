#'@title Identification of replacements between two animals
#'@name replacement
#'
#'@description Function to identify replacements between actor and reactor from
#'             electronic bins data.
#'
#'@param x dataset with electronic bins information.
#'@param sec optimal interval (in seconds) between two different animals sequentially visited the
#'           same bin (feeder or drinker) to identify a replacement; 
#'
#'@details replacement is only applied for dataset with columns named as follows: equip_id (bin identification),
#'         animal_id (animal identification), IN (date - dd/mm/yyyy - and time - hh:mm:ss - when the animal entry in the bin),
#'         OUT (date - dd/mm/yyyy - and time - hh:mm:ss - when the animal left the bin).
#'
#'@return Replacement between two animals
#'
#'@importFrom utils tail
#'@importFrom utils head
#'@importFrom dplyr lag
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@examples 
#'x <- data.frame(equip_id = as.numeric("0001"), 
#'                animal_id = c(1,2,6,3,5,4,2,1,3,5,8,1,6,9,4,3,2,1,5,1))
#'
#'x$IN <-  c("01/08/2017 00:03:42","01/08/2017 00:05:26","01/08/2017 00:07:04","01/08/2017 00:08:15",
#'           "01/08/2017 00:10:35","01/08/2017 00:15:07","01/08/2017 00:18:13","01/08/2017 00:21:48",
#'           "01/08/2017 00:23:55","01/08/2017 00:30:14","01/08/2017 00:35:00","01/08/2017 00:38:11",
#'           "01/08/2017 00:39:05","01/08/2017 00:40:20","01/08/2017 00:42:08","01/08/2017 00:46:00",
#'           "01/08/2017 00:48:12","01/08/2017 00:49:40","01/08/2017 00:50:57","01/08/2017 00:52:36")
#'
#'x$OUT <-c("01/08/2017 00:05:24","01/08/2017 00:06:56","01/08/2017 00:08:12","01/08/2017 00:10:32",
#'          "01/08/2017 00:15:04","01/08/2017 00:18:10","01/08/2017 00:21:41","01/08/2017 00:23:53",
#'          "01/08/2017 00:30:10","01/08/2017 00:34:56","01/08/2017 00:37:32","01/08/2017 00:39:03",
#'          "01/08/2017 00:40:10","01/08/2017 00:41:51","01/08/2017 00:45:56","01/08/2017 00:48:10",
#'          "01/08/2017 00:49:36","01/08/2017 00:50:33","01/08/2017 00:52:32","01/08/2017 00:55:34")
#'
#'replace <- replacement(x,14)
#'
#'print(replace)
#'
#'@export
replacement <- function(x, sec){
  x$IN <- as.POSIXlt(x$IN, format="%d/%m/%Y %H:%M:%S")
  x$OUT <- as.POSIXlt(x$OUT, format="%d/%m/%Y %H:%M:%S")
  x <- x[
    order( x$OUT),
  ]
  x <- x[
    order( x$equip_id),
  ]
  x$dif <- c(NA, tail(x$IN , -1) - head(x$OUT, -1))
  x <- subset(x, x$dif>= 0)
  x$actor <- ifelse(x$dif <= sec & x$dif >= 1, x$animal_id, 0 )
  x$reactor <- ifelse(x$actor == x$animal_id, lag(x$animal_id), 0)
  x$reactor <- ifelse(x$actor == x$animal_id, lag(x$animal_id), 0)
  x$status <- ifelse(x$actor != x$reactor, "OK", 0)
  replacement <- subset(x, x$status == "OK")
  replacement <-  replacement[,c("actor","reactor")]
}


#'@title Frequency of replacements by bin
#'@name repByBin
#'
#'@description Function to identify frequency of replacements by bin from
#'             electronic bin data.
#'             
#'
#'@param x dataset with electronic bins information.
#'@param sec optimal interval (in seconds) between two different animals sequentially visited the
#'           same bin (feeder or drinker) to identify a replacement; 
#'
#'@details repByBin is only applied for dataset with columns named as follows: equip_id (bin identification),
#'         animal_id (animal identification), IN (date - dd/mm/yyyy - and time - hh:mm:ss - when the animal entry in the bin),
#'         OUT (date - dd/mm/yyyy - and time - hh:mm:ss - when the animal left the bin).
#'
#'@return Frequency of replacements by bin
#'
#'@importFrom utils tail
#'@importFrom utils head
#'@importFrom dplyr lag
#'@importFrom dplyr mutate
#'@importFrom stats aggregate
#'@importFrom magrittr %>%
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@examples 
#'x <- data.frame(equip_id = as.numeric("0001"), 
#'                animal_id = c(1,2,6,3,5,4,2,1,3,5,8,1,6,9,4,3,2,1,5,1))
#'
#'x$IN <-  c("01/08/2017 00:03:42","01/08/2017 00:05:26","01/08/2017 00:07:04","01/08/2017 00:08:15",
#'           "01/08/2017 00:10:35","01/08/2017 00:15:07","01/08/2017 00:18:13","01/08/2017 00:21:48",
#'           "01/08/2017 00:23:55","01/08/2017 00:30:14","01/08/2017 00:35:00","01/08/2017 00:38:11",
#'           "01/08/2017 00:39:05","01/08/2017 00:40:20","01/08/2017 00:42:08","01/08/2017 00:46:00",
#'           "01/08/2017 00:48:12","01/08/2017 00:49:40","01/08/2017 00:50:57","01/08/2017 00:52:36")
#'
#'x$OUT <-c("01/08/2017 00:05:24","01/08/2017 00:06:56","01/08/2017 00:08:12","01/08/2017 00:10:32",
#'          "01/08/2017 00:15:04","01/08/2017 00:18:10","01/08/2017 00:21:41","01/08/2017 00:23:53",
#'          "01/08/2017 00:30:10","01/08/2017 00:34:56","01/08/2017 00:37:32","01/08/2017 00:39:03",
#'          "01/08/2017 00:40:10","01/08/2017 00:41:51","01/08/2017 00:45:56","01/08/2017 00:48:10",
#'          "01/08/2017 00:49:36","01/08/2017 00:50:33","01/08/2017 00:52:32","01/08/2017 00:55:34")
#'
#'bins <- repByBin(x,10)
#'
#'print(bins)
#'
#'@export
repByBin <- function(x, sec){
  x$IN <- as.POSIXlt(x$IN, format="%d/%m/%Y %H:%M:%S")
  x$OUT <- as.POSIXlt(x$OUT, format="%d/%m/%Y %H:%M:%S")
  x <- x[
    order( x$OUT),
  ]
  x <- x[
    order( x$equip_id),
  ]
  x$dif <- c(NA, tail(x$IN , -1) - head(x$OUT, -1))
  x <- subset(x, x$dif>= 0)
  x$actor <- ifelse(x$dif <= sec & x$dif >= 1, 1, 0 )
  x <- subset(x, actor != 0)
  y <- aggregate(x$actor,list(x$equip_id), sum)
  names(y)[c(1,2)] <- c('equip_id', 'replacements')
  y <- y %>% dplyr::mutate("%" = replacements/sum(replacements)*100)
}

#'@title Frequency of an animal was actor
#'@name freqActor
#'
#'@description Function to identify frequency that one animal was actor regarding the herd at bins.
#'
#'@param x dataset with replacements information.
#'
#'@details freqActor is only applied for dataset with columns ordained and named as follows: actor and reactor.
#'
#'@return Frequency of an animal was an actor
#'
#'@importFrom dplyr mutate
#'@importFrom magrittr %>%
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@examples 
#'x <- data.frame(actor = c(6,3,5,4,2,1,3,5,8,6,9,3,2,1,1),
#'                reactor = c(2,6,3,5,4,2,1,3,5,1,6,4,3,2,5))                
#'                                    
#'freqA <- freqActor(x)                                                
#'                                                            
#'print(freqA)
#'
#'@export
freqActor <- function(x){
  y <- x %>% 
    group_by(actor) %>%
    summarise(freq_actor = n())
  names(y)[c(1,2)] <- c('animal_id', 'freq_actor')
  y <- y %>% mutate("%" = freq_actor/sum(freq_actor)*100)
  return(y)
}

#'@title Frequency of an animal was reactor
#'@name freqReactor
#'
#'@description Function to identify frequency that one animal was reactor regarding the herd at bins.
#'
#'@param x dataset with replacements information.
#'
#'@details freqReactor is only applied for dataset with columns ordained and named as follows: actor and reactor.
#'
#'@return Frequency of an animal was an reactor
#'
#'@importFrom dplyr mutate
#'@importFrom magrittr %>%
#'
#'@author Julia P. S. Valente, Matheus Deniz, Karolini T. de Sousa.
#'
#'@examples 
#'x <- data.frame(actor = c(6,3,5,4,2,1,3,5,8,6,9,3,2,1,1),
#'                reactor = c(2,6,3,5,4,2,1,3,5,1,6,4,3,2,5))                
#'                                    
#'freqR <- freqReactor(x)                                                
#'                                                            
#'print(freqR)
#'
#'@export
freqReactor <- function(x){
  y <- x %>% 
    group_by(reactor) %>%
    summarise(freq_reactor = n())
  names(y)[c(1,2)] <- c('animal_id', 'freq_reactor')
  y <- y %>% mutate("%" = freq_reactor/sum(freq_reactor)*100)
  return(y)
}
