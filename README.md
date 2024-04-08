# Visualization of the Categories/ Codebook

Part of the project “Assessing Daily Life Activities: Comparing
Predefined Categories with the Qualitative Analysis of Open-Ended
Responses in Experience Sampling Methodology (ESM)” Marie Stadel, Anna
Langener, Laura Bringmann

Sankey Diagram is based on:
<https://gist.github.com/d3noob/013054e8d7807dff76247b81b0e29030>

In this repository we provide example code to visualize the categories/
codebook as a Sankey Diagramm:

<img src="Example_Picture.png" width="567" />

## How to get data into the right structure for visualization?

### 1) Required data files to create the visualization

To visualize the data, we need two files. The first file should contain
an overview of all unique codes and their corresponding level. The
columns should be named “Code” and “Level”.

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

Source_Target
```

    ##                                           source
    ## 1                                           Food
    ## 2                      cooking/ preparing a meal
    ## 3                                           Food
    ## 4                                           Food
    ## 5                                           Food
    ## 6                                           Food
    ## 7                                           Food
    ## 8                                           Food
    ## 9                                           Food
    ## 10                             Personal grooming
    ## 11                                 getting ready
    ## 12                                 getting ready
    ## 13                                 getting ready
    ## 14                                 getting ready
    ## 15                                 getting ready
    ## 16                                 getting ready
    ## 17                                 getting ready
    ## 18                                 getting ready
    ## 19                                 getting ready
    ## 20                             Personal grooming
    ## 21                         getting ready for bed
    ## 22                         getting ready for bed
    ## 23                         getting ready for bed
    ## 24                         getting ready for bed
    ## 25                         getting ready for bed
    ## 26                             Personal grooming
    ## 27                             Personal grooming
    ## 28                             Personal grooming
    ## 29                             Personal grooming
    ## 30                             Personal grooming
    ## 31                                       Resting
    ## 32                                       Resting
    ## 33                                       Resting
    ## 34                                       Resting
    ## 35                                       Resting
    ## 36                                         Hobby
    ## 37                                         Hobby
    ## 38                                 playing games
    ## 39                                 playing games
    ## 40                                 playing games
    ## 41                                 playing games
    ## 42                                         Hobby
    ## 43                                         Hobby
    ## 44                                         Hobby
    ## 45                                         Hobby
    ## 46                    working on a creative task
    ## 47                    working on a creative task
    ## 48                    working on a creative task
    ## 49                    working on a creative task
    ## 50                    working on a creative task
    ## 51                    working on a creative task
    ## 52                    working on a creative task
    ## 53                    working on a creative task
    ## 54                    working on a creative task
    ## 55                    working on a creative task
    ## 56                    working on a creative task
    ## 57                    working on a creative task
    ## 58                    working on a creative task
    ## 59                    working on a creative task
    ## 60                    working on a creative task
    ## 61                                          City
    ## 62                             going to the city
    ## 63                             going to the city
    ## 64                             going to the city
    ## 65                                        Nature
    ## 66                                 going outside
    ## 67                                 going outside
    ## 68                                 going outside
    ## 69                                 going outside
    ## 70                                 going outside
    ## 71                                    University
    ## 72                                    University
    ## 73                                    University
    ## 74                                    University
    ## 75   going/ being at university/ faculty/library
    ## 76   going/ being at university/ faculty/library
    ## 77                 having an uni-related meeting
    ## 78                 having an uni-related meeting
    ## 79                 having an uni-related meeting
    ## 80                                          Work
    ## 81                                          Work
    ## 82           going to/ being at work/ the office
    ## 83                                          Work
    ## 84                          work-related meeting
    ## 85                          work-related meeting
    ## 86                          work-related meeting
    ## 87                                          Work
    ## 88                                          Work
    ## 89                                          Work
    ## 90                                          Work
    ## 91                                          Work
    ## 92                                          Work
    ## 93                                      Sporting
    ## 94                                      Sporting
    ## 95                                      Sporting
    ## 96                               doing a workout
    ## 97                               doing a workout
    ## 98                               doing a workout
    ## 99                               doing a workout
    ## 100                              doing a workout
    ## 101                              doing a workout
    ## 102                              doing a workout
    ## 103                              doing a workout
    ## 104                              doing a workout
    ## 105                              doing a workout
    ## 106                              doing a workout
    ## 107                              doing a workout
    ## 108                              doing a workout
    ## 109                              doing a workout
    ## 110                              doing a workout
    ## 111                                       Chores
    ## 112                                small errands
    ## 113                                       Chores
    ## 114                                       Chores
    ## 115                                       Chores
    ## 116                                       Chores
    ## 117                                       Chores
    ## 118                                       Chores
    ## 119                                       Chores
    ## 120                                       Social
    ## 121     hanging out/ being/ meeting with someone
    ## 122     hanging out/ being/ meeting with someone
    ## 123     hanging out/ being/ meeting with someone
    ## 124     hanging out/ being/ meeting with someone
    ## 125                                       Social
    ## 126                  helping/ supporting someone
    ## 127                                       Social
    ## 128                                Substance use
    ## 129                                Substance use
    ## 130                                Substance use
    ## 131                                        Party
    ## 132                                        Party
    ## 133                                        Party
    ## 134                                        Party
    ## 135                                        Party
    ## 136 preparing for a holiday/ event (celebration)
    ## 137 preparing for a holiday/ event (celebration)
    ## 138 preparing for a holiday/ event (celebration)
    ## 139                                       Buying
    ## 140                               going shopping
    ## 141                               going shopping
    ## 142                               going shopping
    ## 143                                       Buying
    ## 144                              online shopping
    ## 145                               going shopping
    ## 146                               going shopping
    ## 147                               going shopping
    ## 148                              online shopping
    ## 149                              online shopping
    ## 150                              online shopping
    ## 151                                      Digital
    ## 152                                      Digital
    ## 153                                      Digital
    ## 154                                      Digital
    ## 155                                      Digital
    ## 156                                      Digital
    ## 157                       browsing on the laptop
    ## 158                                      Digital
    ## 159                        browsing on the phone
    ## 160                       browsing on the laptop
    ## 161                       browsing on the laptop
    ## 162                       browsing on the laptop
    ## 163                        browsing on the phone
    ## 164                        browsing on the phone
    ## 165                        browsing on the phone
    ## 166                                     Religion
    ## 167                         religious activities
    ## 168                                       Travel
    ## 169                                    traveling
    ## 170                                    traveling
    ## 171                                    Transport
    ## 172                                    traveling
    ## 173                                    traveling
    ## 174                                    traveling
    ## 175                                    traveling
    ## 176                                    traveling
    ## 177                                    traveling
    ## 178                                       Health
    ## 179                         doctor's appointment
    ## 180                         doctor's appointment
    ## 181                         doctor's appointment
    ## 182                         doctor's appointment
    ## 183                         doctor's appointment
    ## 184                                       Health
    ## 185                             not feeling well
    ## 186                           being sick/injured
    ## 187                           being sick/injured
    ## 188                           being sick/injured
    ## 189                             not feeling well
    ## 190                                        Other
    ## 191                                        Other
    ## 192                                        Other
    ##                                           target level_source level_target
    ## 1                      cooking/ preparing a meal            4            3
    ## 2                   making/ having a tea/ coffee            3            1
    ## 3                                         baking            4            1
    ## 4                                  ordering food            4            1
    ## 5             going out for fast food restaurant            4            1
    ## 6                          going to a restaurant            4            1
    ## 7                                going to a cafe            4            1
    ## 8                                       snacking            4            1
    ## 9                                  eating a meal            4            1
    ## 10                                 getting ready            4            3
    ## 11                                     showering            3            1
    ## 12                          washing/ drying hair            3            1
    ## 13                  putting on/ changing clothes            3            1
    ## 14                              brushed my teeth            3            1
    ## 15                                washed my face            3            1
    ## 16                        putting on a face mask            3            1
    ## 17                                 doing make up            3            1
    ## 18                                painting nails            3            1
    ## 19                                    dying hair            3            1
    ## 20                         getting ready for bed            4            3
    ## 21                                     showering            3            1
    ## 22                          washing/ drying hair            3            1
    ## 23                  putting on/ changing clothes            3            1
    ## 24                              brushed my teeth            3            1
    ## 25                                washed my face            3            1
    ## 26                            getting a piercing            4            1
    ## 27                             getting a massage            4            1
    ## 28                          going to hairdresser            4            1
    ## 29                         going to beauty salon            4            1
    ## 30                           going to nail salon            4            1
    ## 31                                  taking a nap            4            1
    ## 32                                taking a break            4            1
    ## 33                   relaxing/ chilling/ resting            4            1
    ## 34                     laying/ sitting somewhere            4            1
    ## 35                                       nothing            4            1
    ## 36                            playing videogames            4            2
    ## 37                                 playing games            4            2
    ## 38                                        sudoku            2            1
    ## 39                                jigsaw puzzles            2            1
    ## 40                                  playing pool            2            1
    ## 41                                   lasergaming            2            1
    ## 42                                pet activities            4            2
    ## 43                                watering plant            4            1
    ## 44                               repotting plant            4            1
    ## 45                    working on a creative task            4            3
    ## 46         looking at old picture/ photos/videos            3            1
    ## 47                         crafting a photo wall            3            1
    ## 48                        assembling a miniature            3            1
    ## 49                                      painting            3            1
    ## 50                                       drawing            3            1
    ## 51                             making a bracelet            3            1
    ## 52                                       crochet            3            1
    ## 53                                      knitting            3            1
    ## 54                                       reading            3            1
    ## 55                  writing poem for Sinterklaas            3            1
    ## 56                                       singing            3            2
    ## 57                         playing an instrument            3            2
    ## 58        acting / practicing (theatre / comedy)            3            2
    ## 59                         photo/ video shooting            3            2
    ## 60                                    journaling            3            2
    ## 61                             going to the city            4            3
    ## 62                          going to the theatre            3            1
    ## 63                            going to the forum            3            1
    ## 64                  watching movie in the cinema            3            1
    ## 65                                 going outside            4            3
    ## 66                               being at a lake            3            1
    ## 67                            going to the ocean            3            1
    ## 68                               being at a park            3            1
    ## 69                   visiting a botanical garden            3            1
    ## 70                             being in a forest            3            1
    ## 71                                      studying            4            2
    ## 72   going/ being at university/ faculty/library            4            3
    ## 73            working on a university assignment            4            1
    ## 74                receiving test results/ grades            4            1
    ## 75        student association/ committee meeting            3            2
    ## 76                 having an uni-related meeting            3            2
    ## 77                                taking an exam            2            1
    ## 78                           attending a lecture            2            1
    ## 79                         attending a practical            2            1
    ## 80                    charity work/ Volunteering            4            3
    ## 81           going to/ being at work/ the office            4            3
    ## 82                          work-related meeting            3            2
    ## 83                          work-related meeting            4            2
    ## 84            assisting in an operation/ surgery            2            1
    ## 85                          patient consultation            2            1
    ## 86                          had my job interview            2            1
    ## 87                     participating in research            4            1
    ## 88                                   babysitting            4            1
    ## 89                     teaching language classes            4            1
    ## 90                                      tutoring            4            1
    ## 91                         giving riding lessons            4            1
    ## 92                       teaching a sports class            4            1
    ## 93                       teaching a sports class            4            1
    ## 94                              going to the gym            4            3
    ## 95                               doing a workout            4            3
    ## 96                                          yoga            3            1
    ## 97                                       pilates            3            1
    ## 98                                    stretching            3            1
    ## 99                                       archery            3            1
    ## 100                           playing basketball            3            1
    ## 101                             playing football            3            1
    ## 102                         playing table tennis            3            1
    ## 103                               playing tennis            3            1
    ## 104                                      dancing            3            1
    ## 105                                     swimming            3            1
    ## 106                              going for a run            3            1
    ## 107                               inline skating            3            1
    ## 108                                   kickboxing            3            1
    ## 109                                          MMA            3            1
    ## 110                                  ice skating            3            1
    ## 111                                small errands            4            3
    ## 112               picking up/ bringing away mail            3            2
    ## 113                        handywork/ renovation            4            1
    ## 114                                   tidying up            4            1
    ## 115                                     cleaning            4            1
    ## 116                              doing groceries            4            1
    ## 117                                doing laundry            4            1
    ## 118                  packing/ unpacking suitcase            4            1
    ## 119                         planning/ organising            4            3
    ## 120     hanging out/ being/ meeting with someone            4            3
    ## 121                                      talking            3            1
    ## 122                               having a fight            3            1
    ## 123                     being intimate/ cuddling            3            1
    ## 124                               fooling around            3            1
    ## 125                  helping/ supporting someone            4            2
    ## 126            picking someone up/ bringing away            2            1
    ## 127           visiting family/ friends overnight            4            3
    ## 128                      smoking (not specified)            4            1
    ## 129                   smoking weed/ taking drugs            4            1
    ## 130          having an (alcoholic) drink/ borrel            4            1
    ## 131                       playing drinking games            4            1
    ## 132                           going out/partying            4            2
    ## 133                           going to a concert            4            2
    ## 134                          going to a festival            4            2
    ## 135 preparing for a holiday/ event (celebration)            4            3
    ## 136                        christmas celebration            3            1
    ## 137                    other holiday celebration            3            1
    ## 138                         birthday celebration            3            1
    ## 139                               going shopping            4            3
    ## 140                    going to a hardware store            3            1
    ## 141                         going to a bookstore            3            1
    ## 142                                 buying gifts            3            1
    ## 143                              online shopping            4            3
    ## 144                                 buying gifts            3            1
    ## 145                            shopping clothing            3            1
    ## 146                               buying jewelry            3            1
    ## 147               shopping home goods/ furniture            3            1
    ## 148                            shopping clothing            3            1
    ## 149                               buying jewelry            3            1
    ## 150               shopping home goods/ furniture            3            1
    ## 151                    listening to music/ radio            4            1
    ## 152                       listening to a podcast            4            1
    ## 153                          watching sport (tv)            4            1
    ## 154            watching videos/ TV shows/ Movies            4            1
    ## 155                       watching/ reading news            4            1
    ## 156                       browsing on the laptop            4            3
    ## 157                             social media use            3            2
    ## 158                        browsing on the phone            4            3
    ## 159                             social media use            3            2
    ## 160          texting/ chatting/checking messages            3            1
    ## 161                                     emailing            3            1
    ## 162                              (video) calling            3            1
    ## 163          texting/ chatting/checking messages            3            1
    ## 164                                     emailing            3            1
    ## 165                              (video) calling            3            1
    ## 166                         religious activities            4            3
    ## 167                              going to church            3            1
    ## 168                                    traveling            4            3
    ## 169                          city trip/ vacation            3            2
    ## 170                                  sightseeing            3            2
    ## 171                                    traveling            4            3
    ## 172                              taking a flight            3            1
    ## 173    taking/ waiting for public transportation            3            1
    ## 174                                      walking            3            1
    ## 175                              biking/ cycling            3            1
    ## 176                             traveling by car            3            1
    ## 177                                   going home            3            1
    ## 178                         doctor's appointment            4            3
    ## 179                              therapy session            3            1
    ## 180                                physiotherapy            3            1
    ## 181                        going to the pharmacy            3            1
    ## 182                        going to the hospital            3            1
    ## 183                          dentist appointment            3            1
    ## 184                             not feeling well            4            3
    ## 185                           being sick/injured            3            2
    ## 186                            having a headache            2            1
    ## 187                          taking a covid test            2            1
    ## 188                               being hungover            2            1
    ## 189                                       crying            3            1
    ## 190                              procrastinating            4            1
    ## 191                                   meditating            4            1
    ## 192                                     thinking            4            1

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

    ##                      source                             target level_source
    ## 1                      Food          cooking/ preparing a meal            4
    ## 2 cooking/ preparing a meal       making/ having a tea/ coffee            3
    ## 3                      Food                             baking            4
    ## 4                      Food                      ordering food            4
    ## 5                      Food going out for fast food restaurant            4
    ## 6                      Food              going to a restaurant            4
    ##   level_target value
    ## 1            3     1
    ## 2            1     1
    ## 3            1     1
    ## 4            1     1
    ## 5            1     1
    ## 6            1     1

### 6) Manual adjustment

If the visualization has some overlapping codes, it may help to move the
order of the codes around in the data file.
