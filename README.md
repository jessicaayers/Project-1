Vignette Project
================
Jessica Ayers
2023-06-22

# Packages Needed

The packages needed for analysis of the Open Movie Database API are:

- `httr`
- `tidyverse`
- `jsonlite`
- `GGally`

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(GGally)
```

# Functions

Options for s include blue, red and green Options for y include 2000,
2010 and 2020 There are 9 total combinations of possible modifications.

``` r
contactFunction <- function(s, y){

if((s == "blue") & (y == 2000)) {

blueData <- GET("http://www.omdbapi.com/?s=blue&y=2000&apikey=ca153ce0")
blueParsed <- fromJSON(rawToChar(blueData$content))

bluedf <- data.frame(blueParsed$Search$Title, blueParsed$Search$Year, blueParsed$Search$imdbID, blueParsed$Search$Type)
blue <- bluedf %>%
  as_tibble() %>%
  rename(Title = blueParsed.Search.Title, Year = blueParsed.Search.Year, imdbID = blueParsed.Search.imdbID, Type = blueParsed.Search.Type)

return(blue)
}

else if((s == "blue") & (y == 2010)){
  blueData <- GET("http://www.omdbapi.com/?s=blue&y=2010&apikey=ca153ce0")
  blueParsed <- fromJSON(rawToChar(blueData$content))

  bluedf <- data.frame(blueParsed$Search$Title, blueParsed$Search$Year, blueParsed$Search$imdbID, blueParsed$Search$Type)
  blue <- bluedf %>%
    as_tibble() %>%
    rename(Title = blueParsed.Search.Title, Year = blueParsed.Search.Year, imdbID = blueParsed.Search.imdbID, Type = blueParsed.Search.Type) 

return(blue)
}

else if((s == "blue") & (y == 2020)){
  blueData <- GET("http://www.omdbapi.com/?s=blue&y=2020&apikey=ca153ce0")
blueParsed <- fromJSON(rawToChar(blueData$content))

bluedf <- data.frame(blueParsed$Search$Title, blueParsed$Search$Year, blueParsed$Search$imdbID, blueParsed$Search$Type)
blue <- bluedf %>%
  as_tibble() %>%
  rename(Title = blueParsed.Search.Title, Year = blueParsed.Search.Year, imdbID = blueParsed.Search.imdbID, Type = blueParsed.Search.Type)

return(blue)
}

else if ((s == "red") & (y == 2000)){
  redData <- GET("http://www.omdbapi.com/?s=red&y=2000&apikey=ca153ce0")
redParsed <- fromJSON(rawToChar(redData$content))

reddf <- data.frame(redParsed$Search$Title, redParsed$Search$Year, redParsed$Search$imdbID, redParsed$Search$Type)
red <- reddf %>%
  as_tibble() %>%
  rename(Title = redParsed.Search.Title, Year = redParsed.Search.Year, imdbID = redParsed.Search.imdbID, Type = redParsed.Search.Type)

return(red)
}

else if ((s == "red") & (y == 2010)){
  redData <- GET("http://www.omdbapi.com/?s=red&y=2010&apikey=ca153ce0")
redParsed <- fromJSON(rawToChar(redData$content))

reddf <- data.frame(redParsed$Search$Title, redParsed$Search$Year, redParsed$Search$imdbID, redParsed$Search$Type)
red <- reddf %>%
  as_tibble() %>%
  rename(Title = redParsed.Search.Title, Year = redParsed.Search.Year, imdbID = redParsed.Search.imdbID, Type = redParsed.Search.Type) 

return(red)
}

else if((s == "red") & (y == 2020)){
  redData <- GET("http://www.omdbapi.com/?s=red&y=2020&apikey=ca153ce0")
redParsed <- fromJSON(rawToChar(redData$content))

reddf <- data.frame(redParsed$Search$Title, redParsed$Search$Year, redParsed$Search$imdbID, redParsed$Search$Type)
red <- reddf %>%
  as_tibble() %>%
  rename(Title = redParsed.Search.Title, Year = redParsed.Search.Year, imdbID = redParsed.Search.imdbID, Type = redParsed.Search.Type) 

return(red)
}

else if((s == "green") & (y == 2000)){
  greenData <- GET("http://www.omdbapi.com/?s=green&y=2000&apikey=ca153ce0")
greenParsed <- fromJSON(rawToChar(greenData$content))

greendf <- data.frame(greenParsed$Search$Title, greenParsed$Search$Year, greenParsed$Search$imdbID, greenParsed$Search$Type)
green <- greendf %>%
  as_tibble() %>%
  rename(Title = greenParsed.Search.Title, Year = greenParsed.Search.Year, imdbID = greenParsed.Search.imdbID, Type = greenParsed.Search.Type) 

return(green)
}

else if((s== "green") & (y == 2010)){
  greenData <- GET("http://www.omdbapi.com/?s=green&y=2010&apikey=ca153ce0")
greenParsed <- fromJSON(rawToChar(greenData$content))

greendf <- data.frame(greenParsed$Search$Title, greenParsed$Search$Year, greenParsed$Search$imdbID, greenParsed$Search$Type)
green <- greendf %>%
  as_tibble() %>%
  rename(Title = greenParsed.Search.Title, Year = greenParsed.Search.Year, imdbID = greenParsed.Search.imdbID, Type = greenParsed.Search.Type) 

return(green)
}

else if((s == "green") & (y == 2020)){
  greenData <- GET("http://www.omdbapi.com/?s=green&y=2020&apikey=ca153ce0")
greenParsed <- fromJSON(rawToChar(greenData$content))

greendf <- data.frame(greenParsed$Search$Title, greenParsed$Search$Year, greenParsed$Search$imdbID, greenParsed$Search$Type)
green <- greendf %>%
  as_tibble() %>%
  rename(Title = greenParsed.Search.Title, Year = greenParsed.Search.Year, imdbID = greenParsed.Search.imdbID, Type = greenParsed.Search.Type) 

return(green)
}

else(stop("invalid s or y argument"))

}
```

# Section 3

``` r
redData1 <- contactFunction(s = "red", y = 2000)
redData2 <- contactFunction(s = "red", y = 2010)
redData3 <- contactFunction(s = "red", y = 2020)
half <- full_join(redData1, redData2)
edaData <- full_join(half, redData3)
edaData
```

    ## # A tibble: 30 × 4
    ##    Title                                  Year      imdbID    Type  
    ##    <chr>                                  <chr>     <chr>     <chr> 
    ##  1 Red Planet                             2000      tt0199753 movie 
    ##  2 Clifford the Big Red Dog               2000–2003 tt0233041 series
    ##  3 Command & Conquer: Red Alert 2         2000      tt0252338 game  
    ##  4 Agent Red                              2000      tt0218080 movie 
    ##  5 Red Dirt                               2000      tt0160749 movie 
    ##  6 Red Hot Chili Peppers: Californication 2000      tt6720730 movie 
    ##  7 Red Letters                            2000      tt0217758 movie 
    ##  8 Red Room 2                             2000      tt0371945 movie 
    ##  9 Rocket's Red Glare                     2000      tt0219279 movie 
    ## 10 Red Hot Chili Peppers: Otherside       2000      tt6720738 movie 
    ## # ℹ 20 more rows

``` r
#Create new variable
#initialize Sequel Variable
edaData$Sequel <- ""
for(i in 1:30){
  if(grepl("2", edaData$Title[i]) == "TRUE"){
    edaData$Sequel[i] = "Yes"
  }
  else {
    edaData$Sequel[i] = "No"
  }
}
edaData
```

    ## # A tibble: 30 × 5
    ##    Title                                  Year      imdbID    Type   Sequel
    ##    <chr>                                  <chr>     <chr>     <chr>  <chr> 
    ##  1 Red Planet                             2000      tt0199753 movie  No    
    ##  2 Clifford the Big Red Dog               2000–2003 tt0233041 series No    
    ##  3 Command & Conquer: Red Alert 2         2000      tt0252338 game   Yes   
    ##  4 Agent Red                              2000      tt0218080 movie  No    
    ##  5 Red Dirt                               2000      tt0160749 movie  No    
    ##  6 Red Hot Chili Peppers: Californication 2000      tt6720730 movie  No    
    ##  7 Red Letters                            2000      tt0217758 movie  No    
    ##  8 Red Room 2                             2000      tt0371945 movie  Yes   
    ##  9 Rocket's Red Glare                     2000      tt0219279 movie  No    
    ## 10 Red Hot Chili Peppers: Otherside       2000      tt6720738 movie  No    
    ## # ℹ 20 more rows

``` r
#initialize length of title variable
edaData$lengthOfTitle <- 0
for(i in 1:30){
  edaData$lengthOfTitle[i] <- nchar(edaData$Title[i])
}
edaData
```

    ## # A tibble: 30 × 6
    ##    Title                                  Year      imdbID  Type  Sequel lengthOfTitle
    ##    <chr>                                  <chr>     <chr>   <chr> <chr>          <dbl>
    ##  1 Red Planet                             2000      tt0199… movie No                10
    ##  2 Clifford the Big Red Dog               2000–2003 tt0233… seri… No                24
    ##  3 Command & Conquer: Red Alert 2         2000      tt0252… game  Yes               30
    ##  4 Agent Red                              2000      tt0218… movie No                 9
    ##  5 Red Dirt                               2000      tt0160… movie No                 8
    ##  6 Red Hot Chili Peppers: Californication 2000      tt6720… movie No                38
    ##  7 Red Letters                            2000      tt0217… movie No                11
    ##  8 Red Room 2                             2000      tt0371… movie Yes               10
    ##  9 Rocket's Red Glare                     2000      tt0219… movie No                18
    ## 10 Red Hot Chili Peppers: Otherside       2000      tt6720… movie No                32
    ## # ℹ 20 more rows

``` r
#contingency tables
table(edaData$Year)
```

    ## 
    ##      2000 2000–2003      2010      2020 2020–2021 2020–2022 
    ##         9         1        10         8         1         1

``` r
table(edaData$Type)
```

    ## 
    ##   game  movie series 
    ##      3     24      3

``` r
table(edaData$Type, edaData$Year)
```

    ##         
    ##          2000 2000–2003 2010 2020 2020–2021 2020–2022
    ##   game      1         0    2    0         0         0
    ##   movie     8         0    8    8         0         0
    ##   series    0         1    0    0         1         1

``` r
table(edaData$Type, edaData$Year, edaData$Sequel)
```

    ## , ,  = No
    ## 
    ##         
    ##          2000 2000–2003 2010 2020 2020–2021 2020–2022
    ##   game      0         0    2    0         0         0
    ##   movie     7         0    8    8         0         0
    ##   series    0         1    0    0         1         1
    ## 
    ## , ,  = Yes
    ## 
    ##         
    ##          2000 2000–2003 2010 2020 2020–2021 2020–2022
    ##   game      1         0    0    0         0         0
    ##   movie     1         0    0    0         0         0
    ##   series    0         0    0    0         0         0

``` r
#numerical summaries for quantitative variables at each setting of the categorical variables
mean(edaData$lengthOfTitle)
```

    ## [1] 17.33333

``` r
median(edaData$lengthOfTitle)
```

    ## [1] 14.5

``` r
sd(edaData$lengthOfTitle)
```

    ## [1] 9.46257

``` r
var(edaData$lengthOfTitle)
```

    ## [1] 89.54023

``` r
IQR(edaData$lengthOfTitle)
```

    ## [1] 14

``` r
#each setting of year first
edaData %>% group_by(Year) %>%
summarise(avg = mean(lengthOfTitle), med = median(lengthOfTitle), var = var(lengthOfTitle))
```

    ## # A tibble: 6 × 4
    ##   Year        avg   med   var
    ##   <chr>     <dbl> <dbl> <dbl>
    ## 1 2000       18.4  11   137. 
    ## 2 2000–2003  24    24    NA  
    ## 3 2010       17.2  17.5 104. 
    ## 4 2020       17.1  15    59.0
    ## 5 2020–2021   9     9    NA  
    ## 6 2020–2022  12    12    NA

``` r
#each setting of type
edaData %>% group_by(Type) %>%
summarise(avg = mean(lengthOfTitle), med = median(lengthOfTitle), var = var(lengthOfTitle))
```

    ## # A tibble: 3 × 4
    ##   Type     avg   med   var
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 game    28.7    30  82.3
    ## 2 movie   16.2    13  81.5
    ## 3 series  15      12  63

``` r
#each setting of Sequel
edaData %>% group_by(Sequel) %>%
summarise(avg = mean(lengthOfTitle), med = median(lengthOfTitle), var = var(lengthOfTitle))
```

    ## # A tibble: 2 × 4
    ##   Sequel   avg   med   var
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 No      17.1  14.5  88.2
    ## 2 Yes     20    20   200

``` r
#bar plot
g <- ggplot(data = edaData, aes(x = Year))
g + geom_bar(aes(fill = Type)) + 
  labs(title = "Bar Plot of Years of Movies, Series, and Games") +
  coord_flip()
```

![](/Users/jessayers/Documents/ST%20558/TOPIC%202/Project-1/unnamed-chunk-8-1.png)<!-- -->

``` r
#histogram
g <- ggplot(edaData, aes(x = lengthOfTitle))
g + geom_histogram(color = "black", fill = "white",
size = 2, binwidth = 3) + 
  labs(title = "Histogram of Length of Title", x = "Length of Title", y = "Count")
```

![](/Users/jessayers/Documents/ST%20558/TOPIC%202/Project-1/unnamed-chunk-9-1.png)<!-- -->

``` r
#Boxplot
g <- ggplot(edaData, aes(x = Type, y = lengthOfTitle))
g + geom_boxplot(fill = "red")
```

![](/Users/jessayers/Documents/ST%20558/TOPIC%202/Project-1/unnamed-chunk-10-1.png)<!-- -->

``` r
g <- ggplot(edaData, aes(x = Year, y = lengthOfTitle))
g + geom_boxplot(fill = "red") + 
stat_summary(fun.y = median, geom = "line",
lwd = 1.5, aes(group = Type, col = Type))
```

![](/Users/jessayers/Documents/ST%20558/TOPIC%202/Project-1/unnamed-chunk-11-1.png)<!-- -->

``` r
#scatterplot
g <- ggplot(edaData, aes(x = lengthOfTitle, y = imdbID))
g + geom_point() +
  facet_grid(~edaData$Type) + 
  geom_text(aes(label = Year))
```

![](/Users/jessayers/Documents/ST%20558/TOPIC%202/Project-1/unnamed-chunk-12-1.png)<!-- -->

``` r
g <- ggplot(edaData, aes(x = Sequel, y = lengthOfTitle, color = Year))
g + geom_line(lwd = 4)
```

![](/Users/jessayers/Documents/ST%20558/TOPIC%202/Project-1/unnamed-chunk-13-1.png)<!-- -->
