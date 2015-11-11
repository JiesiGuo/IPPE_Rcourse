

# load packages and an example dataset
library(dplyr)
pisa<-read.csv("PISA12-AUS-motivation.csv")
# `tbl_df` creates a "local data frame"
# Local data frame is simply a wrapper for a data frame that is easier to examine.

# convert to local data frame
pisa <- tbl_df(pisa)
# printing only shows 10 rows and as many columns as can fit on your screen
pisa

### Viewing printing more output

# you can specify that you want to see more rows
print(pisa, n=20)
# slice() certain row numbers
slice(pisa, 1:5)
# specify that you want to see ALL rows (don't run this!)
print(pisa, n = Inf)
# specify that you want to see ALL columns
print(pisa, width = Inf)
# convert to a normal data frame to see all of the columns
data.frame(head(flights))
# show up to 1000 rows and all columns
flights %>% View()

# set option to set numbers of columns and rows you want to see
options(dplyr.width = 20, dplyr.print_min = 20)
# reset options (or just close R)
options(dplyr.width = NULL, dplyr.print_min = 10)

## filter: Keep rows matching criteria

# dplyr approach is simpler to write and read
# Command structure (for all dplyr verbs):
    # first argument is a data frame
    # return value is a data frame
    # nothing is modified in place

# base R approach to view all students in catholic school in NSW
pisa[pisa$Schtype==1 & pisa$State=="NSW", ]# Base R approach forces you to repeat the data frame's name

# dplyr approach
filter(pisa, Schtype==1, State=="NSW") # using comma or ampersand to represent AND condition 


## "Chaining" - the key feature of dplyr

# Can write commands in a natural order by using the `%>%` infix/pipe operator imported from [magrittr]
# take the thing on the lefe hand-side and insert it as the first argument on the right hand-side
# x %>% f(y) equal to f(x,y)
# x %>% f(y) %>% g(z) equal to g(f(x,y),z)

# to view all students in NSW and VIC 
pisa %>% filter (State=="NSW" | State=="VIC") #using pipe for OR condition
filter (pisa, State=="NSW" | State=="VIC")
# you can also use %in% operator
pisa %>% filter (State %in% c("NSW", "VIC"))
### other examples for selecting colomns 
# between() is a concise alternative for determing if numeric values fall in a range
# to view students having math scores from 300 to 500
flights %>% filter(between(PV1MATH, 300, 500))

# is.na() can also be useful when filtering
# filliter ST42Q02 without missing value
flights %>% filter(!is.na(ST42Q02))

## select: Pick columns by name

# base R approach to select school sype, state and math achievement
pisa[, c("Schtype", "State", "PV1MATH")]

# dplyr approach
pisa %>% select (Schtype, State, PV1MATH) # equal to select (pisa, Schtype, State, PV1MATH)
### other examples for selecting columes by name
pisa %>% select(Schtype:PV1MATH)      # From Schtype through PV1MATH.
pisa %>% select(-State, -PV1MATH)     # hide those two variables.
pisa %>% select(contains("MAT"))      # Gets variables containing "MAT"
pisa %>% select(starts_with("PV"))    # Gets all vars staring with "PV".
pisa %>% select(ends_with("MAT"))     # All vars ending with "MAT".
pisa %>% select(matches("^ST\\d{2}"))  # Matches any regular expression.
## arrange: Reorder rows

# base R approach to select State and PV1MATH columns and sort by PV1MATH
pisa[order(pisa$PV1MATH), c("State", "PV1MATH")]

## rename: specific columns
rename(pisa, SES = ESCS) 

# dplyr approach
pisa %>%
    select(State, PV1MATH) %>%
    arrange(PV1MATH) #use `desc` for descending: arrange(desc(PV1MATH))
## mutate: Add new variables

# Create new variables that are functions of existing variables

# base R approach to create a new variable for differece between students' math achivement and science achievement
pisa$diff_ach <- pisa$PV1MATH / pisa$PV1SCIE
pisa[, c("PV1MATH", "PV1SCIE", "diff_ach")]

# dplyr approach (prints the new variable but does not store it)
pisa %>%
    select(PV1MATH, PV1SCIE) %>%
    mutate(diff_ach = PV1MATH - PV1SCIE)

# store the new variable
pisa <- pisa %>% mutate(diff_ach = PV1MATH - PV1SCIE) 

# `transmute` will let you use newly create variables in the same function which is creating the variable in the first place.
pisa %>% 
    select(PV1MATH, PV1SCIE) %>%
    mutate(diff_ach = PV1MATH - PV1SCIE, ratio = diff_ach/PV1MATH, ratio2 = diff_ach/ratio)

## summarise: Reduce variables to values

# summary the mean of math achievement and math self-concept
pisa %>% summarise(mean_math = mean(PV1MATH), mean_msc = mean(SCMAT, na.rm = TRUE))
## Combine `mutate` and `summarise` with `group_by`
#  `mutate` and `summarise` are primarily useful with data that has been grouped by one or more variables
# local data frame is the same as the original one except for grouping State and Schtype
pisa %>% group_by(State,Schtype) 

# n_groups() simply reports the number of groups
pisa %>% group_by(State,Schtype)  %>% n_groups() 

#  view the number of students and schools in each school type and each 
 pisa %>% 
   group_by(State,Schtype) %>%
 	summarise(stu = n(),school=n_distinct(SCHOOLID)) 

# function `n()` counts the number of rows in a group
# function `n_distinct(vector)` counts the number of unique items in that vector
 	
# `summarise_each` allows you to apply the same summary function to multiple columns at once
# view  max and min scorces of each of achievement scores (reading, math, science)
pisa %>%
 	group_by(State,Schtype) %>%
 	summarise_each(funs(max, min), PV1MATH:PV1SCIE)
# if varibles with missing value, summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), PV1MATH:PV1SCIE))
# `mutate_each` allows you to apply the same mutate function to multiple columns at once to create new viriables

# for each SCHOOLID, create the mean of student' math and science achievement for each students
pisa1 <- pisa %>% select (SCHOOLID, PV1MATH:PV1SCIE) # reduce the size of data set for easy presentation in this case
pisa1 %>%
 	group_by(SCHOOLID) %>% 
 	mutate_each(funs(mean_ach = mean(.)), -PV1READ)
#Here a small bug: when only 1 function is applied, dplyr will keep the orignal names

# What you can do to avoid this is specifying names of each variable manually 
pisa1 %>%
	group_by(SCHOOLID) %>% 
	mutate_each(funs(mean), mean_math = PV1MATH, mean_scie = PV1SCIE, -PV1READ) 

# when 2 functions are applied, variable names works fine
# calculate the standardardized and centered scores of each student's read, math and science achievement 
pisa1 %>%
    mutate_each(funs(std = scale(.), cent = scale(., scale = FALSE)), -SCHOOLID) %>%
    print(width = Inf)
## `do` allows you to apply one unnamed argument 
# for each of school type, do simple regression of math achievement on self-concept 
pisa %>% 
  			group_by(Schtype) %>%
  			do(
   			 output = lm(PV1MATH ~ SCMAT, data = .)
 			 )
# store to a new local data frame
models <- pisa %>% 
  			group_by(Schtype) %>%
  			do(
   			 output = lm(PV1MATH ~ SCMAT, data = .)
          )

# create a function to extract path coefficients, standard error, t-value and p-value
coef_df <- function(x) {
  temp <- coef(summary(x))
  colnames(temp) <- c("est", "se", "t", "P")
  data.frame(coef = rownames(temp), temp)
}
models %>% do(coef_df(.$output))

# the easy way to extract simialr output by using `tidy` function in broom package
#install.packages("broom") 
library(broom)
models %>% do(tidy(.$output)) #`tidy` function can be used for different types of regression models

## Joining (merging) data frames

# create two simple data frames
a <- data_frame(color = c("black","blue","red"), num = 4:6)
b <- data_frame(color = c("black","blue","pink"), size = c("S","I","P"))

# the three most common joins
# include observations found in either "a" or "b"
full_join(a, b)

# only include observations found in both "a" and "b" 
inner_join(a, b)

#include all observations found in "a"
left_join(a, b)
```

## Combining data frames
```{r}
# create a simple dataframe
mydata <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE) 
str(mydata)
# when combinding column-wise,the number of rows must match, but row names are ignored
bind_cols(mydata, data.frame(z = 3:1)) 
# when combinding row-wise, both the number and names of columns much match
bind_rows(mydata, data.frame(x = 10, y = "z")) 
* use`rbind_all` to combind multiple data frames like `do.call(rbind, )`
