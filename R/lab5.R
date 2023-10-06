#' Access and analyse the data in the Kolada dataset
#' 
#' This RC class function can access to the Kolada dataset which kpi's id is N00945 via Kolada API. Check here: http://api.kolada.se/v2/kpi/N00945.
#' Ihe data is percentage of temporary parental benefit (in terms of net days) taken out by men from 1996 to 2022.
#' This function just gets these five cities' data: Stockholm, Gothenburg, Lund, Uppsala, and Linkoping.
#' 
#' @field city The city you want to query. You could only choose from Stockholm, Gothenburg, Lund, Uppsala, and Linkoping.
#' @field city_and_id Cities and their ids in Kolada dataset. Check here: https://api.kolada.se/v2/municipality.
#' @field one_city_data Data frame of the city you queried.
#' @field all_city_data Data frame of all the five cities.
#' 
#' @importFrom httr GET 
#' @importFrom jsonlite fromJSON
#' @importFrom methods new
#' 
#' @exportClass kolada
#' @export kolada

kolada <- setRefClass('kolada',
                      fields = list(
                        city='character',
                        city_and_id='vector',
                        one_city_data='data.frame',
                        all_city_data='data.frame'
                      ),
                      methods = list(
                        initialize = function(city){
                          city <<- city
                          city_and_id <<- c(Stockholm='0180',Gothenburg='1480',Lund='1281', 
                                            Uppsala='0380',Linkoping='0580')
                          if(!city %in% names(city_and_id)){
                            stop('Only the following cities are available: Stockholm, Gothenburg, Lund, Uppsala, and Linkoping')
                          }
                          stopifnot(is.character(city))
                          
                          kolada_url <- paste0('http://api.kolada.se/v2/data/kpi/N00945/municipality/',city_and_id[[city]])
                          response <- httr::GET(url = kolada_url)
                          data_list <- jsonlite::fromJSON(rawToChar(response$content))
                          data_period <- data_list$values['period']
                          
                          values_list <- list()
                          for (a in 1:length(row(data_list$values['values']))) {
                            value <- data_list$values['values'][[1]][[a]][['value']]
                            values_list[[a]] <- value
                          }
                          data_value <- data.frame(value = unlist(values_list))
                          
                          names(data_value) <- city
                          one_city_data <<- cbind(data_period, data_value)
                        },
                        
                        getonecitydata = function(){
                          return(one_city_data)
                        },
                        
                        getallcitydata = function(){
                          all_city_data <<- kolada$new(names(city_and_id)[1])$getonecitydata()
                          for (i in 2:length(city_and_id)){
                            all_city_data <<- cbind(all_city_data,kolada$new(names(city_and_id)[i])$getonecitydata()[2])
                          }
                          return(all_city_data)
                        },
                        
                        meandata = function(){
                          mevalue <- mean(one_city_data[,2], na.rm = T)
                          cat(paste0('The average value of percentage in ',city,' is ',round(mevalue,1),'%.'))
                        }
                      )
)

