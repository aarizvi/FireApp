natBarPlot <- function(data, years, category){
        merged.all <- data
        merged.all$year <- as.character(merged.all$year)
        
        merged.all.new <- merged.all %>% filter(year==years)
        merged.all.new$year <- as.factor(merged.all.new$year)
        
        if(category=="AREA_ORIG"){
                p <- ggplot(merged.all.new, aes(AREA_ORIG)) 
                p <- p + geom_bar() 
                p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 20))
        } else if(category=="HEAT_SOURC"){
                p <- ggplot(merged.all.new, aes(HEAT_SOURC))
                p <- p + geom_bar() 
                p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size= 20))

        } else {
                p <- ggplot(merged.all.new, aes(CAUSE_IGN))
                p <- p + geom_bar()
                p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size=20))
        }

        
}

