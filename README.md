---
title: "Fire Incident App"
output: github_document
---

# Introduction
We were given a dataset that investigated fire incidences in the United States. The data was from the years 2006-2015. We only looked at a subset of the data from the years 2012-2015. Our focus was specifically on residental fire incidences. We first explored the data and manually curated the dataset such that only informative variables were kept. The variables that we decided to keep upon cleaning were `AREA_ORIG`, `CAUSE_IGN`, and `HEAT_SOURC`. The columns can be further described as the following:  
*  `AREA_ORIG` = Area of Fire Origin  
*  `CAUSE_IGN` = Cause of ignition  
*  `HEAT_SOURC` = Location in which the fire began

We created a `shinyApp` that created an interactive display, allowing the user to examine at the count of incidences per variable of interest from the years 2012-2015. The `shinyApp` also allowed the user to examine the fire department in Massachusetts that had the most incidences from 2012-2015. This fire department was located in Boston and was `FDID 25035`. We were able to retreive the locations of incidents using the `incidentaddress.txt` available for each year. We used `ggmap` package in R to display the coordinates of incidents on a map.  

### Research objectives:  
1. To quantify the number of incidents across the United States and further stratify the incidents by cause.  
2. To investigate dispersion of fires in Boston, MA from year to year and visually displaying these data.

# Cleaning/tidying data
We first wanted to clean and tidy the data. The data came in two types of formats, `.DBF` which required the package `foreign`, or `.txt` files separated by `^`. We wrote a helper function (`readFireIncidents`) to read in these data. This function was written after exploring the raw data. The columns that are selected here are mostly due to the compeleteness of observations per variable. The other variables that are not included, had a high amount of `NA` values. 

```{r, eval=F}
# helper function to read in files
readFireIncident <- function(file.directory, year){
        if(str_detect(file.directory, ".dbf")==TRUE){
                # read in data
                x <- read.dbf(file.directory, as.is=F)
                # make data into tibble data frame so we don't accidentally print all of the output
                x <- tbl_df(x)
                # choose variables of interest that could draw inference from data
                x <- x %>% dplyr::select(STATE, FDID, INC_DATE, INC_NO, NOT_RES, CAUSE_IGN, HEAT_SOURC, AREA_ORIG) %>%
                        filter(NOT_RES=="N") %>% # filter on non-residental
                        mutate(unknownY=ifelse(CAUSE_IGN=="U", 1, 0), year=year) # mutate cause of ignition to binary variable, 1=unknown, 0=reason described)
                x <- x %>% mutate(date=mdy(x$INC_DATE)) # mutate date into proper data format (requires lubridate package)

        }else{
                x <- read.table(file.directory, sep="^", header=T, stringsAsFactors = F)
                x <- tbl_df(x)
                x <- x %>% dplyr::select(STATE, FDID, INC_DATE, INC_NO, NOT_RES, CAUSE_IGN, HEAT_SOURC, AREA_ORIG) %>%
                        filter(NOT_RES=="N") %>%
                        mutate(unknownY=ifelse(CAUSE_IGN=="U", 1, 0), year=year)
                x <- x %>% mutate(date=mdy(x$INC_DATE))
        }
        return(x)
}
```

### Merge incident addresses with fire incident data

We also wrote a helper function (`incAddress`) to parse out the addresses from `incidentaddress.txt`, manipulate the columns into one column called `ADDRESS`. This `ADDRESS` column was then used with the function `ggmap::geocode` to retrieve global coordinates.  
```{r, eval=F}
# helper function to load in incident address data
incAddress <- function(file){
        if(str_detect(file, ".dbf")==TRUE){
                # read in file
                x <- read.dbf(file, as.is=F)
                # make data frame into tibble data frame so we don't accidentally print the entire data set
                x <- tbl_df(x)
                # exclude columns we are not interested in
                x <- x %>% dplyr::select(-EXP_NO, -LOC_TYPE, -NUM_MILE, -STREETSUF, -APT_NO, -ZIP4, -X_STREET, -INC_DATE)
                # customize address data such that we can use geocache to retrieve coordinators 
                x$STREETTYPE <- paste0(x$STREETTYPE, ",")
                x$CITY <- paste0(x$CITY, ",")
                x <- x %>% unite(ADDRESS, c(STREET_PRE, STREETNAME, STREETTYPE, CITY, STATE_ID, ZIP5), sep=" ")
                # remove white space and replace all NAs with blank string
                x$ADDRESS <- trimws(str_replace_all(x$ADDRESS, fixed("NA"), ""))
                #remove addresses that started with , 
                x$ADDRESS[startsWith(x$ADDRESS, ", ")] <- str_replace(x$ADDRESS[startsWith(x$ADDRESS, ", ")], ", ", "")
                

        }else{
                # runs similar to first part of if else loop
                x <- read.table(file, sep="^", header=T, stringsAsFactors = F)
                x <- tbl_df(x)
                x <- x %>% dplyr::select(-EXP_NO, -LOC_TYPE, -NUM_MILE, -STREETSUF, -APT_NO, -ZIP4, -X_STREET, -INC_DATE)
                x$STREETTYPE <- paste0(x$STREETTYPE, ",")
                x$CITY <- paste0(x$CITY, ",")
                x <- x %>% unite(ADDRESS, c(STREET_PRE, STREETNAME, STREETTYPE, CITY, STATE_ID, ZIP5), sep=" ")
                x$ADDRESS <- trimws(str_replace_all(x$ADDRESS, fixed("NA"), ""))
                x$ADDRESS[startsWith(x$ADDRESS, ", ")] <- str_replace(x$ADDRESS[startsWith(x$ADDRESS, ", ")], ", ", "")
        }
        return(x)
}

# create merge function to join fire incidence data with incident address data
merge_incAddFI <- function(fiData, incAddData){
        x <- fiData %>% left_join(incAddData, c("STATE", "FDID", "INC_NO"))
        return(x)
}
```

# Exploratory Data Analysis

We decided to look at the frequency of incidents per fire department, and subsequently reduce the dimesionality of the dataset by selecting the fire department with most incidents.  As seen in **Table 1**, FDID 25035 had the most incidents at 2314 incidents between 2012-2015. Concurrently, `ggmap::geocache()` only permits 2500 coordinates to be retrieved daily, which subsequently lead us to deciding to only plot out the FDID 25035 for our [ShinyApp](https://yduggal.shinyapps.io/finalProject/). We will get more into the ShinyApp in a succeeding subsection. 

```{r, echo=F, cache=T, message=F, eval=F}
fi12 <- readFireIncident("2012/NFIRS_2012_052714/fireincident.txt", "2012")
fi13 <- readFireIncident("2013/NFIRS_2013_121514/fireincident.txt", "2013")
fi14 <- readFireIncident("2014/NFIRS_2014_030216/fireincident.txt", "2014")
fi15 <- readFireIncident("2015/NFIRS_FIRES_2015_20170215/fireincident.txt", "2015")

merged.all <- rbind(fi12, fi13, fi14, fi15)

# helper function to parse states
stateParser <- function(fireIncident.data, state){
        x <- fireIncident.data %>% filter(STATE==state)
        return(x)
}
fi12.ma <- stateParser(fi12, "MA")
fi13.ma <- stateParser(fi13, "MA")
fi14.ma <- stateParser(fi14, "MA")
fi15.ma <- stateParser(fi15, "MA")

# load in the incident address data for 2012-2015
incAdd12 <- incAddress("2012/NFIRS_2012_052714/incidentaddress.txt")
incAdd13 <- incAddress("2013/NFIRS_2013_121514/incidentaddress.txt")
incAdd14 <- incAddress("2014/NFIRS_2014_030216/incidentaddress.txt")
incAdd15 <- incAddress("2015/NFIRS_FIRES_2015_20170215/incidentaddress.txt")


merged.12 <- merge_incAddFI(fi12.ma, incAdd12)
merged.13 <- merge_incAddFI(fi13.ma, incAdd13)
merged.14 <- merge_incAddFI(fi14.ma, incAdd14)
merged.15 <- merge_incAddFI(fi15.ma, incAdd15)

merged.all.ma <- rbind(merged.12, merged.13, merged.14, merged.15)

fdid.freq <- data.frame(table(merged.all.ma$FDID))
colnames(fdid.freq) <- c("FDID", "Freq")
fdid.top <- fdid.freq %>% arrange(desc(Freq)) %>% head
knitr::kable(fdid.top, caption="Table 1. Number of Residental Fire Incidents per Fire Department in Massachusetts.")
```

### Retrieval of coordinates
Coordinates (latitude and longitude) are retrievable using the `ggmap` package and the function `geocode`. The required input for the `geocode` function is a proper address that has a street, city, state, etc. We used addresses in that we manipulated and merged in the preceding subsection to retrieve coordinates.
```{r,eval=F}
topMA <- merged.all.ma %>% filter(FDID=="25035")
topMA <- cbind(topMA, geocode(topMA$ADDRESS))
```

```{r,echo=F, cache=T, eval=F}
topMA <- read.table("MA_fireincidences.txt", sep="\t", header=T)
```

However, before we move forwards with plotting the `ggmap`, we first must discretize the observations in each of our categories of interest (`AREA_ORG`, `HEAT_SOURC`, and `CAUSE_IGN`). Each of these categories have a large number of subcategories that increases the dimensionality such that intepretating trends is noisier. As such, discretization allows us to generalize the observations a bit more into larger bins and therefore we will be able to properly analyze and visualize these data. 

### Discretizing categories
After exploring the data, we wrote helper functions to discretize the data (because we needed to this for both the national dataset (`merged.all`) and the top fire department in MA dataset (`topMA`). The helper functions are called `ign.fun`, `area.orig.fun`, and `heatsource.fun`. These functions curate the variables of interest by replacing the descriptive subcategories with broader categories provided in the codebook retrieved with the raw dataset. These functions can be found in the code `fp.R` or the Appendix.  

```{r, echo=F, eval=F}
ign.fun <- function(x) {
        ign.desc <- c("0"="Cause, other",
                      "1"="Intentional",
                      "2"="Unintentional",
                      "3"="Failure of equipment of heat source",
                      "4"="Act of nature",
                      "5"="Cause under investigation",
                      "U"="Cause undetermined after investgation")
        x$CAUSE_IGN <- as.factor(as.character(ign.desc[as.character(x$CAUSE_IGN)]))
        return(x)
}

area.orig.fun <- function(x){
        x$AREA_ORIG <- as.character(x$AREA_ORIG) 
        x[grepl("^0.*", x$AREA_ORIG),]$AREA_ORIG <- "Means of Egress"
        x[grepl("^1.*", x$AREA_ORIG),]$AREA_ORIG <- "Assembly, Sales Areas (Groups of People)"
        x[grepl("^2.*", x$AREA_ORIG),]$AREA_ORIG <- "Function Area"
        x[grepl("^3.*", x$AREA_ORIG),]$AREA_ORIG <- "Technical processing areas, other"
        x[grepl("^4.*", x$AREA_ORIG),]$AREA_ORIG <- "Storage Areas"
        x[grepl("^5.*", x$AREA_ORIG),]$AREA_ORIG <- "Service Areas"
        x[grepl("^6.*", x$AREA_ORIG),]$AREA_ORIG <- "Service, Equpiment Areas"
        x[grepl("^7.*", x$AREA_ORIG),]$AREA_ORIG <- "Structural Areas"
        x[grepl("^8.*", x$AREA_ORIG),]$AREA_ORIG <- "Transportation, Vehicle Areas"
        x[grepl("^9.*", x$AREA_ORIG),]$AREA_ORIG <- "Other Area of Origin"
        x[grepl("^U.*", x$AREA_ORIG),]$AREA_ORIG <- "Undetermined"
        x[grepl("^u.*", x$AREA_ORIG),]$AREA_ORIG <- "Undetermined"
        x$AREA_ORIG <- as.factor(x$AREA_ORIG)
        return(x)
}

# HEAT SOURCE:
heatsource.fun <- function(x){
        x$HEAT_SOURC <- as.character(x$HEAT_SOURC) 
        x[grepl("^0.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Heat source: other"
        x[grepl("^1.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Operating Equipment"
        x[grepl("^4.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Hot or Smoldering Object"
        x[grepl("^5.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Explosives, Fireworks"
        x[grepl("^6.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Other Open Flame or Smoking Materials"
        x[grepl("^7.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Chemical, Natural Heat Sources"
        x[grepl("^8.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Heat Spread from Another Fire"
        x[grepl("^9.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Multiple heat sources including multiple ignitions"
        x[grepl("^U.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Undetermined"
        x[grepl("^u.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Undetermined"
        x$HEAT_SOURC <- as.factor(x$HEAT_SOURC)
        return(x)
}
```

We apply these functions to both datasets. The datasets are now ready to be plotted. Some example plots with be shown in the next section and the interactive plot will be implemented in a ShinyApp.

```{r, echo=F, cache=T, eval=F}
topMA.new <- readRDS("finalProject/topMA.new.rds")
topMA.new <- topMA.new %>% select(-FACT_IGN_1, -ACRES_BURN, -LESS_1ACRE)
merged.all <- readRDS("finalProject/merged.all.rds")
```

The final datasets are the following:

### United States from 2012-2015 Dataset

```{r, echo=F, eval=F}
knitr::kable(head(merged.all), caption="Glimpse of Clean US Dataset")
```

### Fire Department 25035 in Boston, MA

```{r, echo=F, eval=F}
knitr::kable(head(topMA.new), caption="Glimpse of Clean FDID 25035 Data")
```



# Results
We were interested in residental fire incidences in the United States from 2012-2015. After cleaning the data we were left with categorical variables `AREA_ORIG`, `HEAT_SOURC`, and `CAUSE_IGN`. Each observation in the data was a unique fire incident, even if the incident number was repeated, as a different observation was observed in at least one of the variables of interest, so for this analysis, each of these incidences were considered unique. 

### Investigating the number of incidents by causal variable across the United States per year. 

```{r, echo=F, cache=T, eval=F}

table.area.orig <- merged.all %>% 
        group_by(year, AREA_ORIG) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        spread(year, n, fill=0) %>%
        rowwise() %>%
        mutate(mean=mean(c(`2012`,`2013`,`2014`,`2015`)))

knitr::kable(table.area.orig, caption="Table 2. Summary of Fire Incident by Areas of Origin in United States from 2012-2015")



table.ign <- merged.all %>% 
        group_by(year, CAUSE_IGN) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        spread(year, n, fill=0) %>%
        rowwise() %>%
        mutate(mean=mean(c(`2012`,`2013`,`2014`,`2015`)))

knitr::kable(table.ign, caption="Table 3. Summary of Fire Incidents by Cause of Ignition in United States from 2012-2015")


table.heat <- merged.all %>% 
        group_by(year, HEAT_SOURC) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        spread(year, n, fill=0) %>%
        rowwise() %>%
        mutate(mean=mean(c(`2012`,`2013`,`2014`,`2015`)))
knitr::kable(table.heat, caption="Table 4. Summary of Fire Incident by Heat Source in United States from 2012-2015")
```

We plot these data in our here and interactively in our shiny app to visually display these results. Shown here are just two of the 12 possible plots.


####  Figure 1. US Fire Incidences in 2012 by Heat Source
```{r, echo=F, fig.width=10, fig.height=10, eval=F}
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
natBarPlot(merged.all, "2012", "HEAT_SOURCE")
```


#### Figure 2. US Fire Incidences in 2013 by Area of Origin 
```{r, fig.caption="test", fig.width=10, fig.height=10, echo=F}
natBarPlot(merged.all, "2013", "AREA_ORIG")
```


### Visualizing Fire Department 25035 by causal variable using GPS coordinates
Here we used the package `ggmap` to visualize the address GPS coordinates that we retrieved in a preceding subsection. Just as an example of the plot that we can create, we will show just the year 2015 fire incidents by area of origin. Please see the shinyApp for an interactive exploration of these GPS specific incidences. 


#### Figure 3. Fire Department 25035 Fire Incidences by year 
```{r, echo=F, eval=F}
mmap <- readRDS("finalProject/mmap.rds")

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

fireIncidentPlot(topMA.new, "2015", "AREA_ORIG", mmap)
```




# Implementation in ShinyApps
We implemented the bar graphs of fire incidents in the US and the ggmap of fire incidences for FDID 25035 in Boston. We created a two panel shinyApp interactive display. We wrote plotting functions `fireIncidentPlot` and `natBarPlot` to plot these functions in the shinyApp. The shinyApp is designed for exploratory purposes. The shinyApp could be expanded to compute statistical analyses or any other features of interest. 

# Conclusion
We sought to investigate the number of fire incidents in the US from 2012-2015. These fire incidents were reported with different characteristics described the incident. After data exploration and cleaning, we reduced the dataset to contain just the variables descibed area of origin, heat source, and cause of ignition. All of these variables were categorical and described by numerous subcategories in which we discretized to broader categories using the provided codebook. We then counted the number of incidents by each of these descriptive variables by each year. If more data (e.g. the 2006-2011 data) were added to this analysis, more inference could be drawn. Due to time constraints and memory concerns, only a subset of these data were investgated. 

We also sought to interactively display the GPS coordinates of incidents in Boston Fire Department 25035, which was the fire department with the most amount of incidents from 2012-2015. We retrieved the coordinates by merging the incident address data with the fire incident data and subsequently manipulating the data such that interptable addresses could be input to the `ggmap` package. 



# Appendix

### Helper functions to discretize variables of interest
```{r, eval=F}
ign.fun <- function(x) {
        ign.desc <- c("0"="Cause, other",
                      "1"="Intentional",
                      "2"="Unintentional",
                      "3"="Failure of equipment of heat source",
                      "4"="Act of nature",
                      "5"="Cause under investigation",
                      "U"="Cause undetermined after investgation")
        x$CAUSE_IGN <- as.factor(as.character(ign.desc[as.character(x$CAUSE_IGN)]))
        return(x)
}

area.orig.fun <- function(x){
        x$AREA_ORIG <- as.character(x$AREA_ORIG) 
        x[grepl("^0.*", x$AREA_ORIG),]$AREA_ORIG <- "Means of Egress"
        x[grepl("^1.*", x$AREA_ORIG),]$AREA_ORIG <- "Assembly, Sales Areas (Groups of People)"
        x[grepl("^2.*", x$AREA_ORIG),]$AREA_ORIG <- "Function Area"
        x[grepl("^3.*", x$AREA_ORIG),]$AREA_ORIG <- "Technical processing areas, other"
        x[grepl("^4.*", x$AREA_ORIG),]$AREA_ORIG <- "Storage Areas"
        x[grepl("^5.*", x$AREA_ORIG),]$AREA_ORIG <- "Service Areas"
        x[grepl("^6.*", x$AREA_ORIG),]$AREA_ORIG <- "Service, Equpiment Areas"
        x[grepl("^7.*", x$AREA_ORIG),]$AREA_ORIG <- "Structural Areas"
        x[grepl("^8.*", x$AREA_ORIG),]$AREA_ORIG <- "Transportation, Vehicle Areas"
        x[grepl("^9.*", x$AREA_ORIG),]$AREA_ORIG <- "Other Area of Origin"
        x[grepl("^U.*", x$AREA_ORIG),]$AREA_ORIG <- "Undetermined"
        x[grepl("^u.*", x$AREA_ORIG),]$AREA_ORIG <- "Undetermined"
        x$AREA_ORIG <- as.factor(x$AREA_ORIG)
        return(x)
}

# HEAT SOURCE:
heatsource.fun <- function(x){
        x$HEAT_SOURC <- as.character(x$HEAT_SOURC) 
        
        x[grepl("^0.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Heat source: other"
        x[grepl("^1.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Operating Equipment"
        x[grepl("^4.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Hot or Smoldering Object"
        x[grepl("^5.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Explosives, Fireworks"
        x[grepl("^6.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Other Open Flame or Smoking Materials"
        x[grepl("^7.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Chemical, Natural Heat Sources"
        x[grepl("^8.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Heat Spread from Another Fire"
        x[grepl("^9.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Multiple heat sources including multiple ignitions"
        x[grepl("^U.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Undetermined"
        x[grepl("^u.*", x$HEAT_SOURC),]$HEAT_SOURC <- "Undetermined"
        
        x$HEAT_SOURC <- as.factor(x$HEAT_SOURC)
        return(x)
}
```
