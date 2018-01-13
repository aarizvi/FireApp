fireIncidentPlot <- function(data, years, category, mmap){
        topMA.new <- data
        topMA.new$year <- as.character(topMA.new$year)
        topMA.new.new <- topMA.new %>% filter(year==years)
        topMA.new.new$year <- as.factor(topMA.new.new$year)
        
        if(category=="AREA_ORIG"){
                g <- mmap
                g <- g + geom_point(data=topMA.new.new, aes(x=lon, y=lat, colour=factor(AREA_ORIG))) + ggtitle(years)
                g       
        } else if(category=="HEAT_SOURC"){
                g <- mmap
                g <- g + geom_point(data=topMA.new.new, aes(x=lon, y=lat, colour=factor(HEAT_SOURC))) + ggtitle(years)
                g   
        } else {
                g <- mmap
                g <- g + geom_point(data=topMA.new.new, aes(x=lon, y=lat, colour=factor(CAUSE_IGN))) + ggtitle(years)
                g
        }
        
        

}

