#' Feeding event data from Nellore cattle
#'
#' A database obtained from feed efficiency test of beef cattle
#' to illustrate the functions of the socialh package.
#'
#' @format A data frame with 90211 rows and 7 variables:
#'  \describe{
#'   \item{equip_id}{equipament identification}
#'   \item{animal_id}{animal identification}
#'   \item{IN}{date and time (dd/mm/yyyy and hour:minutes:seconds) when the animal entered at the electronic bin}
#'   \item{OUT}{date and time (dd/mm/yyyy and hour:minutes:seconds) when the animal left the electronic bin}
#'   \item{duration (s)}{ duration of the feeding event in seconds}
#'   \item{consumption (g)}{amount of food consumed during the visit to the bin in grams}
#'   \item{pen}{pen identification}
#' }
#' @source <https://www.kaggle.com/datasets/juliavalente/data-from-visits-to-the-trough-of-nellore-cattle>
#' @examples
#' data(feeding_event_data)
"feeding_event_data"