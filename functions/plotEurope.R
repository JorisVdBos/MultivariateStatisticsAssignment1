# Probeersel voor mooie europese map :) http://egallic.fr/en/european-map-using-r/
if(!require(grid)) {
  install.packages("grid") 
  library(grid)
}
if(!require(rworldmap)) {
  install.packages("rworldmap") 
  library(rworldmap)
}
worldMap <- getMap()
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

getCountriesCoords <- function(countryNames){
  countries <- which(worldMap$NAME %in% c(europeanUnion, countryNames))
  countriesCoords <- lapply(countries, function(i){
    df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    df$region =as.character(worldMap$NAME[i])
    colnames(df) <- list("long", "lat", "region")
    return(df)
  })
  countriesCoords <- do.call("rbind", countriesCoords)
  colnames(countriesCoords)[which(colnames(countriesCoords) == "region")] <- "country_name"
  countriesCoords$index <- 1:length(countriesCoords$long)
  
  return(countriesCoords)
}

plotEurope <- function(countriesCoords, scores, 
                       RC = 1, 
                       totalRC = 2,
                       extraTitle = NULL){
  plotData <- merge(countriesCoords, scores[, c("country_name", paste0("RC", RC))], by ="country_name", all.x = TRUE)
  ggplot(data = plotData[order(plotData$index),]) + 
    geom_polygon(aes_string(x = "long", y = "lat", group = "country_name", fill =  paste0("RC", RC)),
                 colour = "black", size = 0.1) +
    coord_map(xlim = c(-13, 55),  ylim = c(28, 73)) + 
    theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
      #panel.background = element_rect(fill = NA, colour = NA),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(), axis.title = element_blank(),
      #rect = element_blank(),
      plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) +
    scale_fill_gradient(name = "", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50") +
    labs(title = paste0("Score of rotated PC", RC, " of ", totalRC, " by country", ifelse(!is.null(extraTitle), paste0(":\n    ", extraTitle), "")),
         fill = paste0("Value of rotated PC", RC))
}