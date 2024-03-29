##################################
# The starting packages & data
##################################

library("tidyverse")


# GET THE DATA
# Remember you will need to take the fires_ch07_start.rds file I have in the lesson # 7 folder and put it in your working directory. 

fires <- readRDS("fires_ch07_start.rds")
##################################
# How to add and modify columns
##################################
# display the fires tibble you will notice that there is a Fire Name, Year etc. 
# Also notice you will you have two dates the date the fire was discovered and the date it was contained. 

fires

# create a new column for the month
# notice that you can use the mutate and format functions together to extract the month from the discovery date,
fires <- fires %>% mutate(Month = format(DiscoveryDate, "%m"))
fires
# create a new column for the number of days burning using mutate which will subtract the discovery date from the contain date. Why do you add a 1 to the mutate function? Because in this reporting a fire that is discovered and contained on the same day is considered a 1 day fire.
fires <- fires %>% mutate(DaysBurning = ContainDate - DiscoveryDate + 1)
fires
# convert the month column to factor type and days burning to integer, why? because we will later use days burning to calculate by month. 
fires <- fires %>% mutate(Month = as.factor(Month),
                          DaysBurning = as.integer(DaysBurning))
fires
# view the new columns
fires %>% select(DiscoveryDate, ContainDate, Month, DaysBurning)
##### Working with other functions that will help you prepare data to
# create some sample variables
first_name <- "Bob"
last_name <- 'Smith'
age <- 40

# combine the variables into strings
str_c(first_name, " ", last_name)             # returns "Bob Smith"
str_c(last_name, first_name, sep = ", ")      # returns "Smith, Bob"
str_c(first_name, " is ", age, " years old.") # returns "Bob is 40 years old."

# how to check if a string contains a substring create a variable named state 
state <- "Maine CD-1"
# check to see if a string contains another string.Which is why it's called sub-string!!!! 
str_detect(state, "CD-")                      # returns TRUE
# remember the pattern has to match, including capitalization
str_detect(state, "cd-")                      # returns FALSE
# str_detect function is case sensitive by adding (?i) into the string pattern you are searching for you can check for the character match, regardless of case, 
str_detect(state, "(?i)cd-")                  # returns TRUE

# how to convert a string to title case create a string in all caps
fire_name <- "AUGUST COMPLEX"
# convert that string to a title meaning only the first letter of each word is capitalized. 
str_to_title(fire_name)                       # returns "August Complex"

# how to pipe a string into multiple functions, column names are usually named with underscores, or camel case or something else not commonly found in a report heading to share with other people. You can use piping to prepare column name all in one command. In this example we are replacing the underscore with a space then we are changing from all caps to title type formatting finally we are replacing the spacing with "nothing" = ""
col_name <- "FIRE_DISCOVERY_DATE"
col_name %>% str_replace_all("_"," ") %>% 
  str_to_title() %>%
  str_replace_all(" ", "")                    # returns "FireDiscoveryDate"


# set the names of the fires columns (not shown in chapter) by creating 
names(fires) <- c("FIRE_NAME","YEAR","STATE","SIZE","DISCOVERY_DATE",
                  "CONTAIN_DATE","MONTH","DAYS_BURNING")


# convert the names of the columns to camel case
# (not shown in chapter)
names(fires) <- names(fires) %>% 
  str_replace_all("_"," ") %>% 
  str_to_title() %>%
  str_replace_all(" ", "")
fires

# view three columns
fires %>% select(FireName, Size, DaysBurning)

# create a new column for the full fire name and populate it
fires <- fires %>% mutate(
  FireName = str_to_title(FireName),
  FullName = str_c("The ", FireName, " Fire (", Year, ")"))
fires
# create a column for the average acres burned per day and populate it. Average is calculated by dividing the Size of the fire, by days it burned. in this example we are also rounding the average to 1 digital place
fires <- fires %>% mutate(AcresPerDay = round(Size / DaysBurning, 1))

fires %>% select(FireName, FullName, AcresPerDay)


##################################
# How to group and bin data
##################################

# how to work with statistical functions
#same as manually calculating an average by dividing, you do have the option to not calculate n/a values and only count cells with data
mean(fires$Size) 
#the middle value not used as often in business reporting
median(fires$Size)
# totals all values 
sum(fires$Size)
# the smallest value
min(fires$Size)
# the largest value
max(fires$Size)
# the standard deviation
sd(fires$Size)

mean(fires$ContainDate)                # returns NA because NA values are present
mean(fires$ContainDate, na.rm = TRUE)  # removes NA values before calculating
# totals the number of NA values in a column
sum(is.na(fires$ContainDate))   # count of NA values
sum(!is.na(fires$ContainDate))  # count of other values that aren't NA

# use summarize to get the mean size
fires %>% summarize(mean(Size))

# use summarize with multiple functions
fires %>% summarize(MeanSize = mean(Size),
                    MedianSize = median(Size),
                    Count = n())

# the three examples above aren't too useful until the data is grouped in someone. Give me the mean test score not for all the tests for the whole class, instead give me the mean test score by test by student 
# group the data by state
fires %>% group_by(State)

# group the data by state, then year
fires %>% group_by(State, Year)

# group the data by state and get summary data for each state
fires %>% group_by(State) %>% 
  summarize(MeanSize = mean(Size),
            MedianSize = median(Size),
            Count = n())

# group the data by state and year 
# and get summary data for each state and year combination
fires %>% group_by(State, Year) %>% 
  summarize(MeanSize = mean(Size),
            MedianSize = median(Size),
            Count = n())

# ungroup the data
fires %>% ungroup()

# saving the groups to the tibble and then removing them
# (not shown in chapter)
fires <- fires %>% group_by(State)
fires
fires <- fires %>% ungroup()
fires

# how to apply a function to a table using a column as an index (group)
fires$DaysBurning %>% 
  tapply(INDEX = fires$State, FUN = mean, na.rm = TRUE)

# same results as above, but a tibble instead of a named vector
fires %>% group_by(State) %>% 
  summarize(MeanDays = mean(DaysBurning, na.rm = TRUE))
# either method works the same way but it matters depending on the reporting that you want to do to the data

# different methods of ranking data. Depending on the data set and the analysis needed you might want to rank the rows in it by a column. In this case you are grouping the fires data by state, summarize by getting the avg(mean) days fires burned in each state, you're ranking the results by mean date desc(smallest to largest), your setting any ties between two states to the same ranked value by using the dense_rank function  
fires %>% group_by(State) %>% 
  summarize(MeanDays = mean(DaysBurning, na.rm = TRUE)) %>% 
  mutate(MeanDays = as.integer(MeanDays),
         Rank = rank(desc(MeanDays)),
         RankMin = rank(desc(MeanDays), ties.method = "min"),
         RowNumber = row_number(desc(MeanDays)),
         DenseRank = dense_rank(desc(MeanDays))) %>% 
  arrange(Rank) %>% 
  head(11)

# create the fire_sizes data set (not shown in chapter)
fire_sizes <- fires %>% filter(Year %in% 1992:1994) %>% 
  group_by(State, Year) %>% 
  summarize(Size = sum(Size)) %>% ungroup()
fire_sizes

# calculate the cumulative sum, or running total for all rows
fire_sizes %>% mutate(CumulativeSize = cumsum(Size))

# calculate the cumulative sum for each group
fire_sizes %>% group_by(State) %>% 
  mutate(CumulativeSize = cumsum(Size))

# create a new column named Quarter and use it to label each row with a bin
fires <- fires %>% mutate(Quarter = cut(as.integer(Month), 
                                        breaks = c(0,3,6,9,12),
                                        labels = c("Q1","Q2","Q3","Q4")))

# display the Month column and newly created Quarter column
fires %>% select(Month, Quarter)

# create a new column named SizeGroup and use it to assign a bin to each row
fires <- fires %>% mutate(SizeGroup = ntile(Size, n = 5))
fires %>% select(Size, SizeGroup)
fires
# the number of values in each bin
table(fires$SizeGroup)



##################################
# How to apply functions and lambda expressions
##################################

# how to define a function that operates on rows
get_contain_date <- function(row) {           # defines a row as its parameter
  if (is.na(row[["ContainDate"]])) {          # double bracket syntax for columns
    return(row[["DiscoveryDate"]])            # uses return() to return values
  } else {
    return(row[["ContainDate"]])
  }
}

# how to test the function on a single row
fires %>% head(1) %>% get_contain_date()

# how to apply the function to all rows
fires <- fires %>% mutate(                     # uses apply() to apply to all rows
  ContainDate = apply(X = fires, MARGIN = 1, FUN = get_contain_date))

# convert the column back to the date type
fires <- fires %>% mutate(ContainDate = as.Date(ContainDate))

# how to view the date columns
fires %>% select(DiscoveryDate, ContainDate)

# how to create a function that operates on columns
count_na <- function(col) {
  count <- sum(is.na(col))
  return(count)
}

# test the function on one column
count_na(fires$FireName)

# apply the function to all columns
apply(X = fires, MARGIN = 2, FUN = count_na)


# apply a lambda expression to columns
apply(X = fires, MARGIN = 2, FUN = function(col) sum(is.na(col)))

# apply a lambda expression to rows
fires <- fires %>% mutate(
  ContainDate = apply(X = fires, MARGIN = 1, FUN = function(row) 
    ifelse(is.na(row[["ContainDate"]]), 
           row[["DiscoveryDate"]], 
           row[["ContainDate"]])))

# another way to write this code (not shown in chapter)
fires <- fires %>% mutate(
  ContainDate = ifelse(is.na(ContainDate), 
                       as.character(DiscoveryDate), 
                       as.character(ContainDate)),
  ContainDate = as.Date(ContainDate))


##################################
# How to reshape tibbles
##################################

# get the original fire data (not shown in chapter)
fires <- readRDS("../../data/fires_ch07_start.rds")
fires

# get a list of state names (not shown in chapter)
state_names <- read_csv("../../data/state_names.csv")
state_names

# inner join the tibbles by State
inner_join(fires, state_names, by = "State")

# two small tibbles to use for demonstrating joins
state_names_min <- state_names %>% filter(State %in% c("CA","AK"))
state_names_min

fires_min <- fires %>% slice(1, 3) %>% select(FireName, Year, Size, State)
fires_min

# four types of join performed on the small tibbles
inner_join(fires_min, state_names_min, by = "State")
left_join(fires_min, state_names_min, by = "State")
right_join(fires_min, state_names_min, by = "State")
full_join(fires_min, state_names_min, by = "State")

# a tibble containing one new fire
new_fires <- as_tibble(data.frame(
  FireName = "MURACH", Year = 2022, State = "CA", Size = 1, 
  DiscoveryDate = "2022-01-01", ContainDate = "2022-01-01"))

new_fires

identical(names(fires), names(new_fires))

fires <- fires %>% rbind(new_fires)
tail(fires)
