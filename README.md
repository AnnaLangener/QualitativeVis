# Visualization of the Categories/ Codebook

Part of the project “Assessing Daily Life Activities: Comparing
Predefined Categories with the Qualitative Analysis of Open-Ended
Responses in Experience Sampling Methodology (ESM)” Marie Stadel, Anna
Langener, Laura Bringmann

Sankey Diagram is based on:
<https://gist.github.com/d3noob/013054e8d7807dff76247b81b0e29030>

In this repository we provide example code to visualize the categories/
codebook as a Sankey Diagramm:

\[ADD LINK\]

<img src="Example_Picture.png" width="567" />

## How to get data into the right structure for visualization?

### 1) Required data files to create the visualization

**File 1** To visualize the data, we need two files. The first file
should contain an overview of all unique codes and their corresponding
level. The columns should be named “Code” and “Level”.

``` r
Overview_Codes <- read.csv("/Users/annalangener/projects/QualitativeVis/Overview_Codes.csv")
head(Overview_Codes)
```

    ##                        Code Level
    ## 1                      Food     4
    ## 2 cooking/ preparing a meal     3
    ## 3         Personal grooming     4
    ## 4             getting ready     3
    ## 5     getting ready for bed     3
    ## 6                   Resting     4

**File 2: Option A**

The second data file contains how the different codes are related to
each other. It contains a column called “source” and a column called
“target”. For example, our first “source” code is *Food*, which has the
subcode (“target”) *cooking/ preparing a meal*.

``` r
Source_Target <- read.csv("/Users/annalangener/projects/QualitativeVis/Source_Target.csv")
head(Source_Target)
```

    ##                      source                             target
    ## 1                      Food          cooking/ preparing a meal
    ## 2 cooking/ preparing a meal       making/ having a tea/ coffee
    ## 3                      Food                             baking
    ## 4                      Food                      ordering food
    ## 5                      Food going out for fast food restaurant
    ## 6                      Food              going to a restaurant

**File 2: Option B**

Another option is to have a file that contains one column per level.
However, this file must also be transformed into a dataframe containing
only two columns, called “source” and “target”.

``` r
Source_Target_2 <- read.csv("Tree_Example.csv")
head(Source_Target_2)
```

    ##   Category                   Level.3 Level.2                            Level.1
    ## 1     Food cooking/ preparing a meal    <NA>       making/ having a tea/ coffee
    ## 2     Food                      <NA>    <NA>                             baking
    ## 3     Food                      <NA>    <NA>                      ordering food
    ## 4     Food                      <NA>    <NA> going out for fast food restaurant
    ## 5     Food                      <NA>    <NA>              going to a restaurant
    ## 6     Food                      <NA>    <NA>                    going to a cafe

Here we transform this file to the needed dataframe;

``` r
library(tidyverse)
# This step is needed if we have not codes on all leveles
Source_Target_2$Level.3[is.na(Source_Target_2$Level.3)] <- Source_Target_2$Category[is.na(Source_Target_2$Level.3)]

Source_Target_2$Level.2[is.na(Source_Target_2$Level.2)] <- Source_Target_2$Level.3[is.na(Source_Target_2$Level.2)]

# ... if more levels are present

#adapted from https://rpubs.com/droach/CPP526-codethrough
Source_Target_Category <- Source_Target_2[,c(1,2)] %>%
  mutate(row = row_number()) %>%                                       
  pivot_longer(cols = -row, names_to = "column", values_to = "source") %>%
  mutate(column = match(column, names(.))) %>%   
  group_by(row) %>% 
  mutate(target = lead(source, order_by = column)) %>% 
  filter(!is.na(target)) %>%                           
  ungroup() 

Source_Target_Level3 <- Source_Target_2[,c(2,3)] %>%
  mutate(row = row_number()) %>%                                       
  pivot_longer(cols = -row, names_to = "column", values_to = "source") %>%
  mutate(column = match(column, names(.))) %>%   
  group_by(row) %>% 
  mutate(target = lead(source, order_by = column)) %>% 
  filter(!is.na(target)) %>%                           
  ungroup() 

Source_Target_Level2 <- Source_Target_2[,c(3,4)] %>%
  mutate(row = row_number()) %>%                                       
  pivot_longer(cols = -row, names_to = "column", values_to = "source") %>%
  mutate(column = match(column, names(.))) %>%   
  group_by(row) %>% 
  mutate(target = lead(source, order_by = column)) %>% 
  filter(!is.na(target)) %>%                           
  ungroup() 

#... if more levels are present

Source_Target_2 <- rbind(Source_Target_Category,Source_Target_Level3,Source_Target_Level2)[,c(3,4)]
Source_Target_2 <- Source_Target_2[!duplicated(Source_Target_2),]

Source_Target <- Source_Target_2[Source_Target_2$source != Source_Target_2$target,]
head(Source_Target)
```

    ## # A tibble: 6 × 2
    ##   source            target                                     
    ##   <chr>             <chr>                                      
    ## 1 Food              cooking/ preparing a meal                  
    ## 2 Personal grooming getting ready                              
    ## 3 Personal grooming getting ready for bed                      
    ## 4 City              going to the city                          
    ## 5 Nature            going outside                              
    ## 6 University        going/ being at university/ faculty/library

### 2) Merge both files

The next step is to merge the two data files. We create two new columns
that contain the level of the “source” and “target” code. We need this
for the data visualization to have the codes in the right place (all
level 4 codes on the left and so on).

``` r
library(dplyr)

source = data.frame(level_source = Overview_Codes$Level,Code = Overview_Codes$Code )
target = data.frame(level_target = Overview_Codes$Level,Code = Overview_Codes$Code )

Source_Target <- left_join(Source_Target, source, by = c("source" = "Code"))
Source_Target <- left_join(Source_Target, target, by = c("target" = "Code"))

head(Source_Target)
```

    ## # A tibble: 6 × 4
    ##   source            target                             level_source level_target
    ##   <chr>             <chr>                                     <int>        <int>
    ## 1 Food              cooking/ preparing a meal                     4            3
    ## 2 Personal grooming getting ready                                 4            3
    ## 3 Personal grooming getting ready for bed                         4            3
    ## 4 City              going to the city                             4            3
    ## 5 Nature            going outside                                 4            3
    ## 6 University        going/ being at university/ facul…            4            3

### 4) Add the “value” column

To visualize the data correctly, we need to add another column called
“value”. This column shows the thickness of the lines between the codes.

``` r
target_counts <- Source_Target %>%
  group_by(source) %>%
  summarize(value = n())

Source_Target <- left_join(Source_Target, target_counts, by = c("target" = "source"))

Source_Target$value[is.na(Source_Target$value)] = 1
```

Most of our value entries are now correctly. For some codes (that have
multiple subcodes), we need to make some additional small adjustments.

``` r
# Fix the value for multiple levels
checkcol <- Source_Target$source[Source_Target$value != 1]

for (i in 1:length(checkcol)) {
  targets <- Source_Target$target[Source_Target$source == checkcol[i]]
  for (j in 1:length(targets)) {
    tragets_lowerlevel <-
      Source_Target$target[Source_Target$source %in% targets[j]]
    for (k in 1:length(tragets_lowerlevel)) {
      value <-
        nrow(Source_Target[Source_Target$source %in% tragets_lowerlevel[k], ]) + 1
      if (value > 1) {
        Source_Target$value[Source_Target$source == checkcol[i] &
                              Source_Target$targe == targets[j]] <- value
      }
    }
  }
}
```

    ## Warning: Unknown or uninitialised column: `targe`.
    ## Unknown or uninitialised column: `targe`.
    ## Unknown or uninitialised column: `targe`.
    ## Unknown or uninitialised column: `targe`.

``` r
# Correct transport case (two sources go to one target)
target_counts <- Source_Target %>%
  group_by(target) %>%
  summarize(value = n())

index <- left_join(Source_Target, target_counts, by = c("target" = "target"))
index <- index[index$value.y != 1,]
index <- index[index$value.x != 1,]

index_source <- index$source
index_source <- Source_Target[Source_Target$target %in% index_source,]

index <- index[duplicated(index$target),]

for(i in 1:nrow(index)){
Source_Target$value[ Source_Target$target == index$target[i]] =
  Source_Target$value[ Source_Target$target == index$target[i]]/index$value.y[i]
}

# fix other mistake
for(i in 1:nrow(index_source)){
Source_Target$value[Source_Target$target %in% index_source] = Source_Target$value[index_source$target[i] == Source_Target$source]
}
```

### 5) Save the file

Finally, we need to save our file. This file will be used for the data
visualization.

``` r
write.csv(Source_Target,"/Users/annalangener/projects/QualitativeVis/TestFormatVis_new.csv")

head(Source_Target)
```

    ## # A tibble: 6 × 5
    ##   source            target                       level_source level_target value
    ##   <chr>             <chr>                               <int>        <int> <dbl>
    ## 1 Food              cooking/ preparing a meal               4            3     1
    ## 2 Personal grooming getting ready                           4            3     9
    ## 3 Personal grooming getting ready for bed                   4            3     5
    ## 4 City              going to the city                       4            3     2
    ## 5 Nature            going outside                           4            3     4
    ## 6 University        going/ being at university/…            4            3     3

### 6) Manual adjustment

If the visualization has some overlapping codes, it may help to move the
order of the codes around in the data file.
