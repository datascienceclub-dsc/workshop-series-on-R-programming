# Reference ====
#' installation: install.packages("dplyr")
#' Book : https://r4ds.had.co.nz 
#' Website https://dplyr.tidyverse.org

browseVignettes(package = "dplyr") # R package documentations

#' comparison with base operations
#' https://dplyr.tidyverse.org/articles/base.html

# 1. About ====
utils::packageVersion("dplyr") 
packageDescription("dplyr")

# 2. Setup ====

## 2.1 Initialize ====
#' dtplyr, dbplyr, etc. are alternative backends to work with large dat.
library(dplyr)

## 2.2 Data ====

if (FALSE) {
  # Data sets related to flights that departed from NYC in 2013
  install.packages("nycflights13")
  library(nycflights13)
  ?flights # ?airports; ?airlines; ?planes; ?weather
  
  devtools::install_github("rstudio/EDAWR")
  library(EDAWR)
  ?storms # ?pollution; ?cases: ?tb
}

?iris # Edgar Anderson's Iris Data
dd <- iris %>% as_tibble()
dd

# 3. Operations ====
?select # Extract existing variables
?filter # Extract existing observations
?mutate # Derive new variables
?summarise # Changing the unit of analysis --> applying statistics
?arrange # Order rows using column values
?group_by # Group by one or more variables
?across # Apply a function (or functions) across multiple columns

# args(summarize); formals(summarize)

## 3.1 select ====
dd
names(dd) # column names
select(dd, Sepal.Length, Petal.Length) # without pipe 

?dplyr::`%>%` # pipe operation exported from package `magrittr`

dd %>% select(Sepal.Length, Petal.Length) # accessing few columns
dd %>% select(everything()) # all columns
dd %>% select(Species, everything()) # all columns
dd %>% select(-Sepal.Length, -Petal.Length) # select everything by -...
dd %>% select(Sepal.Length:Petal.Length) # selection range
dd %>% select(contains("Length")) # column names contains "chr_string"
dd %>% select(matches("\\.")) # column matching a regular expression
dd %>% select(starts_with("P")) # column name starts with "chr_string"
dd %>% select(ends_with("s")) # column name ends with "chr_string"

# renaming columns
dd %>% select(SL = Sepal.Length, PL = Petal.Length)

dd %>% select(SL = Sepal.Length, PL = Petal.Length, everything())
dd %>% rename(SL = Sepal.Length, PL = Petal.Length) # alternative
# dd %>% rename(SL = 1)

## 3.2 filter ====
#' you can use any following logical tests in R
#' ?Comparison
#' ?base:::Logic

dd %>% nrow
dd %>% filter(Sepal.Length > 5) # single filter
dd %>% filter(Sepal.Length > 5, Species == "versicolor") # multiple filters

## 3.3 transformations ====
#' These are element wise/ row wise operations
#' CAUTION: careful with non-vectorized function
dd %>% ncol

dd %>% 
  rowwise() %>% 
  mutate(ratio = Sepal.Length/Petal.Length) %>% 
  print(width = Inf) # single transformation

dd %>% mutate(ratio = Sepal.Length/Petal.Length,
              SepalLength2 = Sepal.Length^2) %>% 
  print(width = Inf) # multiple transformation

#' some useful transformation functions
#' Notice the difference between mutate() & transmute() ?
#' 
dd %>% transmute(
  Sepal.Length,
  max = pmax(Sepal.Width, Petal.Width), # element wise maximum --> unlike max()
  min = pmin(Sepal.Width, Petal.Width), # element wise minimum --> unlike min()
  isBetween_4_5 = between(Sepal.Length, left = 4, right = 5), # are values between left & right ?
  lead = lead(Sepal.Length), # copy with values one position down
  lag = lag(Sepal.Length), # copy with values one position up
  grp = ntile(Sepal.Length, n = 10)) %>% # group vector into n equal buckets 
  # coũnt(grp)
  print(width = Inf)

#' look for: cume_dist(), dense_rank(), percent_rank(), ...

## 3.4 summarise ====
#' applicable for only non character and factor variables

dd %>% summarise(avg_sepal_length = mean(Sepal.Length))

select_variable <- "Sepal.Length"
dd %>% 
  select(v = all_of(select_variable)) %>% 
  summarise(Variable = dplyr::all_of(select_variable),
            n = n(), # number of observations
            n_unique = n_distinct(v), # number of distinct observations
            min = min(v), # minimum
            avg = mean(v), # average
            med = median(v), # median
            q75 = quantile(v, probs = 0.75), # 3rd quantile
            max = max(v), # maximum
            var = var(v), # variance
            sum = sum(v), # total sum
            first = first(v), # first observation of the vector
            last = last(v), # last observation of the vector
            std = sd(v)) # standard deviations

## 3.5 arrange ====
#' order 

dd %>% print(n = 4)
dd %>% arrange(Sepal.Length) %>% print(n = 4)
dd %>% arrange(desc(Sepal.Length)) %>% print(n = 4, width = Inf)
dd %>% arrange(Species, Sepal.Length) %>% print(n = 4, width = Inf)
dd %>% arrange(desc(Species), Sepal.Length) %>% print(n = 4, width = Inf)

## 3.6 group_by ====
# --> used along with summarise()
dd %>% count(Species)

dd %>% 
  group_by(Species) %>% 
  summarise(mean = mean(Sepal.Length))

dd %>% 
  group_by(Species) %>% 
  summarise_all(mean)

dd %>% 
  mutate(SepalLength5 = Sepal.Length > 5) %>% 
  # print(width = Inf)
  group_by(Species, SepalLength5) %>% # multiple group by elements
  summarise(n = n(),
            avgPL = mean(Petal.Length),
            stdPL = sd(Petal.Length)) %>% 
  ungroup()
# Note : use ungroup() to apply operations at row level again.

## 3.7 across ====

### 3.7.1 across with mutate ====
dd %>% mutate(across(.fns = round)) # error
str(dd)
dd %>% mutate(across(-Species, .fns = round))
dd %>% mutate(across(where(is.factor), .fns = as.integer)) # converting factors
dd %>% mutate(across(starts_with("Petal"), .fns = mean)) # selecting vars

### 3.7.2 across with group_by & mutate ====
# applying functions across
dd %>% 
  group_by(Species) %>% 
  mutate(across(starts_with("Petal"), mean)) %>% 
  ungroup() %>% 
  slice_sample(by = "Species", n = 5)

dd %>% 
  group_by(Species) %>% 
  mutate(across(starts_with("Petal"),
                list(avg = mean, std = sd),
                .names = "{.col}.{.fn}")) %>% # output naming convention
  ungroup() %>% 
  slice_sample(by = "Species", n = 3) %>% 
  print(width = Inf)

### 3.7.3 across with group_by & summarise ====
dd %>% 
  group_by(Species) %>% 
  summarise(across(starts_with("Petal"), mean))

dd %>%
  group_by(Species) %>%
  summarise(across(starts_with("Petal"), 
                   list(avg = mean, std = sd),
                   .names = "{.col}.{.fn}"))

## 3.8 if_any and if_all ====
dd %>% filter(if_any(ends_with("Width"), ~ . > 4))
dd %>% filter(if_all(ends_with("Width"), ~ . > 2))

# 4 The pipe %>% operator ====
# --> narrates the story
dd %>% dim
dd %>% 
  select(Sepal.Length:Petal.Length) %>% # selects variable of interest
  mutate(ratio = Sepal.Length/Petal.Length) %>% # calculates new variables
  filter(Sepal.Length > 5) %>% # some clean up
  arrange(desc(Sepal.Length)) # order data based on user interest

# 5. Joining data ====

(d1 <- tibble(id = 1:5, x = runif(5), y = x * 2))
(d2 <- tibble(id = 3:7, x = runif(5), y = sqrt(x), z = log(y)))

## 5.1 bind_rows ====
bind_rows(d1, d2)
d1 %>% bind_rows(d2) # alternative

## 5.2 bind_cols ====
d1 %>% bind_cols(d2)
d1 %>% bind_cols(d2, .name_repair = make.names)

## 5.3 union ====
d1 %>% union(d2) # error
d1 %>% union(d2 %>% select(-z))

## 5.4 intersect ====
d1 %>% intersect(d2) # error
d1 %>% select(id) %>% intersect(d2 %>% select(id))

## 5.5 setdiff ====
d1 %>% setdiff(d2) # error
d1 %>% setdiff(d2 %>% select(-z))
d2 %>% select(-z) %>% setdiff(d1)

## 5.6 left_join ====
d1 %>% left_join(d2)
d1 %>% left_join(d2, by = "id")
d1 %>% left_join(d2, by = "id", suffix = c(".d1", ".d2"))

## 5.7 right_join ====
d1 %>% right_join(d2, by = "id", suffix = c(".d1", ".d2"))

## 5.8 inner_join ====
d1 %>% inner_join(d2, by = "id", suffix = c(".d1", ".d2"))

## 5.9 semi_join ====
d1 %>% semi_join(d2, by = "id")
d2 %>% semi_join(d1, by = "id")

## 5.10 anti_join ====
d1 %>% anti_join(d2, by = "id")
d2 %>% anti_join(d1, by = "id")

# 6. Dessert (tidyr) ==== 

## 6.1 Pivot ====

# gather 

# spread 


