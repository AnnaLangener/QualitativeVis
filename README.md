# Visualization of the Categories/ Codebook

This repository is part of the project “Assessing Daily Life Activities:
Comparing Predefined Categories with the Qualitative Analysis of
Open-Ended Responses in Experience Sampling Methodology (ESM)” Marie
Stadel, Anna Langener, Laura Bringmann

In this repository we provide example code to visualize the categories/
codebook as a Sankey Diagramm: See
<https://annalangener.github.io/QualitativeVis/> or
<https://annalangener.github.io/QualitativeVis/Skimina.html>.

<img src="Example_Picture.png" width="567"/>

The Sankey Diagram is based on:
<https://gist.github.com/d3noob/013054e8d7807dff76247b81b0e29030>

## How to get data into the right structure for visualization?

### 1) Required data files to create the visualization

To visualize the data, we need two files. The first file should contain
an overview of all unique codes and their corresponding level. The
columns should be named “Code” and “Level”. The second data file should
contain two columns called source and target, which indicates the
relationship between the codes (i.e., how codes are nested)

**Option A**

*File 1*

If we have an overview file already ready, it should look like this (one
column that indicates the unique codes and one column that indicates the
level);

``` r
Overview_Codes <- read.csv("/Users/annalangener/projects/QualitativeVis/Overview_Codes.csv")
head(Overview_Codes)
```

```         
##                        Code Level
## 1                      Food     4
## 2 cooking/ preparing a meal     3
## 3         Personal grooming     4
## 4             getting ready     3
## 5     getting ready for bed     3
## 6                   Resting     4
```

*File 2*

The second data file contains how the different codes are related to
each other. It contains a column called “source” and a column called
“target”. For example, our first “source” code is *Food*, which has the
subcode (“target”) *cooking/ preparing a meal* (see also the picture
above or the sankey diagram to see how codes are related to each other).

``` r
Source_Target <- read.csv("/Users/annalangener/projects/QualitativeVis/Source_Target.csv")
head(Source_Target)
```

```         
##                      source                             target
## 1                      Food          cooking/ preparing a meal
## 2 cooking/ preparing a meal       making/ having a tea/ coffee
## 3                      Food                             baking
## 4                      Food                      ordering food
## 5                      Food going out for fast food restaurant
## 6                      Food              going to a restaurant
```

**Option B**

Another option is to have a file that contains one column per level.
However, this file must also be transformed into an overview dataframe
and into a dataframe containing only two columns, called “source” and
“target”.

``` r
Source_Target_2 <- read.csv("Skimina.csv")
head(Source_Target_2)
```

```         
##        Category                    Level.3            Level.2
## 1 Entertainment           Creative hobbies     Creative hobby
## 2 Entertainment Indoor leisure and resting            Reading
## 3 Entertainment Indoor leisure and resting               News
## 4 Entertainment Indoor leisure and resting              Hobby
## 5 Entertainment            Outdoor leisure              Hobby
## 6 Entertainment Indoor leisure and resting Listening to music
##                                                        Level.1
## 1 Creative hobby (e.g., singing, dancing, playing instruments)
## 2                                                      Reading
## 3                                     Watching or reading news
## 4                        Getting new information on ones hobby
## 5                                        Hobby (e.g., fishing)
## 6                                                         <NA>
```

Here we create the overview dataframe;

``` r
library(tidyverse)
```

```         
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.2     ✔ purrr   1.0.1
## ✔ tibble  3.2.1     ✔ dplyr   1.1.2
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.2
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

``` r
Source_Target_wide <- Source_Target_2

# Add a row index as ID column
Source_Target_wide$ID <- seq_len(nrow(Source_Target_wide))

# Convert from wide to long format using pivot_longer
Overview_Codes <- pivot_longer(
  data = Source_Target_wide,
  cols = -ID,  # Specify columns to remain as identifier variables
  names_to = "Level",  # Name of the new column for variable names
  values_to = "Code"  # Name of the new column for variable values
)

Overview_Codes <- Overview_Codes[-1]
Overview_Codes$Level[Overview_Codes$Level == "Category"] <- 4
Overview_Codes$Level[Overview_Codes$Level == "Level.3"] <- 3
Overview_Codes$Level[Overview_Codes$Level == "Level.2"] <- 2
Overview_Codes$Level[Overview_Codes$Level == "Level.1"] <- 1

Overview_Codes <- Overview_Codes[!duplicated(Overview_Codes$Code),]

# Output the long format data frame
print(Overview_Codes)
```

```         
## # A tibble: 154 × 2
##    Level Code                                                        
##    <chr> <chr>                                                       
##  1 4     Entertainment                                               
##  2 3     Creative hobbies                                            
##  3 2     Creative hobby                                              
##  4 1     Creative hobby (e.g., singing, dancing, playing instruments)
##  5 3     Indoor leisure and resting                                  
##  6 2     Reading                                                     
##  7 2     News                                                        
##  8 1     Watching or reading news                                    
##  9 2     Hobby                                                       
## 10 1     Getting new information on ones hobby                       
## # ℹ 144 more rows
```

Here we transform this file to the needed dataframe containing only
source and target;

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
rm(Source_Target_Category,Source_Target_Level3,Source_Target_Level2)
Source_Target_2 <- Source_Target_2[!duplicated(Source_Target_2),]

Source_Target <- Source_Target_2[Source_Target_2$source != Source_Target_2$target,]
head(Source_Target)
```

```         
## # A tibble: 6 × 2
##   source        target                    
##   <chr>         <chr>                     
## 1 Entertainment Creative hobbies          
## 2 Entertainment Indoor leisure and resting
## 3 Entertainment Outdoor leisure           
## 4 Entertainment Physical activity         
## 5 Entertainment Repairing                 
## 6 Household     Errands
```

### 2) Merge both files

The next step is to merge the two data files. We create two new columns
that contain the level of the “source” and “target” code. We need this
for the data visualization to have the codes in the right place.

``` r
library(dplyr)

source = data.frame(level_source = Overview_Codes$Level,Code = Overview_Codes$Code )
target = data.frame(level_target = Overview_Codes$Level,Code = Overview_Codes$Code )

Source_Target <- left_join(Source_Target, source, by = c("source" = "Code"))
Source_Target <- left_join(Source_Target, target, by = c("target" = "Code"))

head(Source_Target)
```

```         
## # A tibble: 6 × 4
##   source        target                     level_source level_target
##   <chr>         <chr>                      <chr>        <chr>       
## 1 Entertainment Creative hobbies           4            3           
## 2 Entertainment Indoor leisure and resting 4            3           
## 3 Entertainment Outdoor leisure            4            3           
## 4 Entertainment Physical activity          4            3           
## 5 Entertainment Repairing                  4            3           
## 6 Household     Errands                    4            3
```

### 4) Add the “value” column

To visualize the data correctly, we need to add another column called
“value”. This column shows the thickness of the "lines"/"connections"
between the codes. For example, the value from "personal grooming" to
"getting ready for bed" needs to be five, because "getting ready for
bed" has five subcodes. The easiest and safest way is to manually adjust
the value column in the csv file. Below is some code to calculate an
initial number for the value column. However, this number may be
incorrect if many codes are "nested" in each other. So it may be need to
be adjusted by hand. The easiest way to adjust it by hand is to open the
visualization and correct the numbers in the value column of the csv
file that are incorrect.

``` r
target_counts <- Source_Target %>%
  group_by(source) %>%
  summarize(value = n())

Source_Target <- left_join(Source_Target, target_counts, by = c("target" = "source"))

Source_Target$value[is.na(Source_Target$value)] = 1
```

Most of our value entries are now correctly. For some codes (that have
multiple subcodes), we need to make some additional small adjustments.
Either with the code below or by hand (I recommend doing this by hand).

``` r
# Fix the value for multiple levels
checkcol <- unique(Source_Target$source[Source_Target$value != 1 & Source_Target$level_source != 4 ])
# checkcol <- unique(Source_Target$source[Source_Target$value != 1])

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
                              Source_Target$target == targets[j]] <- value
      }
    }
  }
}

# Correct transport/ hobby case (two sources go to one target)
target_counts <- Source_Target %>%
  group_by(target) %>%
  summarize(value = n())


#### NEXT LINES ONLY FOR SKIMINIA ####
# Correct value for level 4 to 3 codes

checkcol <- unique(Source_Target$source[Source_Target$value != 1 & Source_Target$level_source == 4])


correction <- target_counts$target[target_counts$value > 1]

for (i in 1:length(checkcol)) {
  targets <- Source_Target$target[Source_Target$source == checkcol[i]]
  for (j in 1:length(targets)) {
    tragets_lowerlevel <-
      Source_Target$target[Source_Target$source %in% targets[j]]
    value <- length(tragets_lowerlevel)

     for (k in 1:length(tragets_lowerlevel)) {
       if(nrow(Source_Target[Source_Target$source %in% tragets_lowerlevel[k], ]) > 1){
      value <-
        nrow(Source_Target[Source_Target$source %in% tragets_lowerlevel[k], ]) + value - 1
      if(tragets_lowerlevel[k] %in% correction){
        value = value -1
      }
       }
    Source_Target$value[Source_Target$source == checkcol[i] &
                              Source_Target$target == targets[j]] <- value
  }
  }
}
################## UNTIL HERE #############


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
df <- Source_Target

write.csv(Source_Target,"/Users/annalangener/projects/QualitativeVis/Skimina_new.csv")

#write.csv(Source_Target,"/Users/annalangener/projects/QualitativeVis/StadelLangener_new.csv")

head(Source_Target)
```

```         
## # A tibble: 6 × 5
##   source        target                     level_source level_target value
##   <chr>         <chr>                      <chr>        <chr>        <dbl>
## 1 Entertainment Creative hobbies           4            3                1
## 2 Entertainment Indoor leisure and resting 4            3               10
## 3 Entertainment Outdoor leisure            4            3                5
## 4 Entertainment Physical activity          4            3                1
## 5 Entertainment Repairing                  4            3                1
## 6 Household     Errands                    4            3                2
```

### 6) Manual adjustment

In general, it is recommended to sort the nodes in the correct order as
you want them to appear in your Sankey diagram. For the Skiminia
visualization, we set the .nodeSort to null in the html file, so it will
take the order from the csv file. If the visualization has some
overlapping codes, it may help to move the order of the codes around in
the data file. We have found the best order by trial and error.
**Note:** it is important that the level 4 codes/categories appear first
in the csv file. For example, “food” must appear before
“cooking/preparing a meal” or the coloring will not work.

### 7) Useful adjustment in the html file

It may be useful to look for the following lines of code and adjust them
in the html file.

The variables width (here 1150) and height (here 2650) can be changed
when nodes overlap.

Similarly, nodeWidth (here 20) and nodePadding (here 8) can be changed
for a nicer visualization. You can also try to delte the nodeSort(null)
attribute.

```         
var margin = { top: 10, right: 10, bottom: 10, left: 10 },
  width = 1150 - margin.left - margin.right, # Try another width
  height = 2650 - margin.top - margin.bottom; #try another height

var sankey = d3.sankey()
  .nodeWidth(20) # Node size
  .nodePadding(8)  # Thickness of lines
  .size([width, height])
  .nodeSort(null); # Remove this if you don't want the fixed order
```

### Questions?

Write an email to a.m.langener[at]rug.nl or langener95[at]gmail.com
