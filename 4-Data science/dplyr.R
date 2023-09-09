# Reference ====
# For online documentation refer : https://r4ds.had.co.nz 
browseVignettes(package = "dplyr")

# 1. About ====
utils::packageVersion("dplyr") 

# 2. Setup ====

# _2.1 Initialize ====
library(dplyr)

# _2.2 Data ====
if (FALSE) {
  # Data sets related to flights that departed from NYC in 2013
  install.packages("nycflights13")
  library(nycflights13)
  ?flights # ?airports; ?airlines; ?planes; ?weather
  
  devtools::install_github("rstudio/EDAWR")
  library(EDAWR)
  ?storms # ?pollution; ?cases: ?tb
}

df <- iris %>% as_tibble()

# 3. Operations ====
?select # Extract existing variables
?filter # Extract existing observations
?mutate # Derive new variables
?summarise # Changing the unit of analysis --> applying statistics
?arrange
?group_by
?across

# _3.1 select ====
df
names(df)
select(df, Sepal.Length, Petal.Length)

df %>% select(Sepal.Length, Petal.Length) # accessing few columns
df %>% select(everything()) # all columns
df %>% select(-Sepal.Length, -Petal.Length) # select everything by -...
df %>% select(Sepal.Length:Petal.Length) # selection range
df %>% select(contains("Length")) # column names contains "chr_string"
df %>% select(matches("\\.")) # column matching a regular expression
df %>% select(starts_with("P")) # column name starts with "chr_string"
df %>% select(ends_with("s")) # column name ends with "chr_string"

# bonus --> rename()
df %>% rename(SL = Sepal.Length, PL = Petal.Length)

df %>% select(SL = Sepal.Length, PL = Petal.Length)
df %>% select(SL = Sepal.Length, PL = Petal.Length, everything())

# _3.2 filter ====
df %>% nrow
df %>% filter(Sepal.Length > 5) # single filter
df %>% filter(Sepal.Length > 5, Species == "versicolor") # multiple filters

# you can use any following logical tests in R
?Comparison
?base:::Logic

# _3.3 mutate ====
# These are element wise/ row wise operations
df %>% ncol

df %>% mutate(ratio = Sepal.Length/Petal.Length) # single transformation
df %>% mutate(ratio = Sepal.Length/Petal.Length,
              SepalLength2 = Sepal.Length^2) # multiple transformation

# some useful mutate functions
# Assignment : cume_dist(), dense_rank(), percent_rank(), ...
df %>% mutate(
  Max = pmax(Sepal.Width, Petal.Width), # element wise maximum --> unlike max()
  Min = pmin(Sepal.Width, Petal.Width), # element wise minimum --> unlike min()
  Between = between(Sepal.Length, left = 4, right = 5), # are values between left & right ?
  Lead = lead(Sepal.Length), # copy with values one position down
  Lag = lag(Sepal.Length), # copy with values one position up
  SepalLengthGrp = ntile(Sepal.Length, n = 10)) # group vector into n equal buckets 

# Note: difference between mutate() & transmute() ?

# _3.4 summarise ====
# applicable for only non character and factor variables

df %>% summarise(avgSepalLength = mean(Sepal.Length))

select_variable <- "Sepal.Length"

df %>% 
  select(v = all_of(select_variable)) %>% 
  summarise(Variable = all_of(select_variable),
            N = n(), # number of observations
            Distinct = n_distinct(v), # number of distinct observations
            Min = min(v), # minimum
            Mean = mean(v), # average
            Median = median(v), # median
            Quantile75 = quantile(v, probs = 0.75), # 3rd quantile
            Max = max(v), # maximum
            Variance = var(v), # variance
            Sum = sum(v), # total sum
            First = first(v), # first observation of the vector
            Last = last(v), # last observation of the vector
            Std = sd(v)) # standard deviations

# _3.5 arrange ====
df %>% print(n = 4)
df %>% arrange(Sepal.Length) %>% print(n = 4)
df %>% arrange(desc(Sepal.Length)) %>% print(n = 4)
df %>% arrange(Species, Sepal.Length) %>% print(n = 4)
df %>% arrange(desc(Species), Sepal.Length) %>% print(n = 4)

# _3.6 group_by ====
# --> used along with summarise()
df %>% count(Species)

df %>% 
  group_by(Species) %>% 
  summarise(Mean = mean(Sepal.Length))

df %>% 
  group_by(Species) %>% 
  summarise_all(mean)

df %>% 
  mutate(SepalLength5 = Sepal.Length > 5) %>% 
  group_by(Species, SepalLength5) %>% 
  summarise(n = n(),
            avgPL = mean(Petal.Length),
            stdPL = sd(Petal.Length))
# Note : use ungroup() to apply operations at row level again.

# _3.7 across ====

# __3.7.1 across with mutate ====
df %>% mutate(across(.fns = round))
df %>% mutate(across(-Species, .fns = round))
df %>% mutate(across(where(is.factor), .fns = as.integer))
df %>% mutate(across(starts_with("Petal"), .fns = mean))

# __3.7.2 across with group_by & mutate ====
df %>% 
  group_by(Species) %>% 
  mutate(across(starts_with("Petal"), mean))

df %>% 
  group_by(Species) %>% 
  mutate(across(starts_with("Petal"),
                list(avg = mean, std = sd),
                .names = "{.col}.{.fn}")) %>% 
  print(width = Inf)

# __3.7.3 across with group_by & summarise ====
df %>% 
  group_by(Species) %>% 
  summarise(across(starts_with("Petal"), mean))

df %>%
  group_by(Species) %>%
  summarise(across(starts_with("Petal"), 
                   list(avg = mean, std = sd),
                   .names = "{.col}.{.fn}"))

# _3.8 if_any and if_all ====
df %>% filter(if_any(ends_with("Width"), ~ . > 4))
df %>% filter(if_all(ends_with("Width"), ~ . > 2))

# 4 The pipe %>% operator ====
# --> narrates the story
df %>% dim
df %>% 
  select(Sepal.Length:Petal.Length) %>% 
  mutate(ratio = Sepal.Length/Petal.Length) %>% 
  filter(Sepal.Length > 5) %>% 
  arrange(desc(Sepal.Length)) %>% 
  mutate()

# 5. Joining data ====

(df1 <- tibble(id = 1:5, x = runif(5), y = x * 2))
(df2 <- tibble(id = 3:7, x = runif(5), y = sqrt(x), z = log(y)))

# _5.1 bind_rows ====
bind_rows(df1, df2)
df1 %>% bind_rows(df2)

# _5.2 bind_cols ====
df1 %>% bind_cols(df2)
df1 %>% bind_cols(df2, .name_repair = make.names)

# _5.3 union ====
df1 %>% union(df2)
df1 %>% union(df2 %>% select(-z))

# _5.4 intersect ====
df1 %>% intersect(df2)
df1 %>% select(id) %>% intersect(df2 %>% select(id))

# _5.5 setdiff ====
df1 %>% setdiff(df2)
df1 %>% setdiff(df2 %>% select(-z))
df2 %>% select(-z) %>% setdiff(df1)

# _5.6 left_join ====
df1 %>% left_join(df2)
df1 %>% left_join(df2, by = "id")
df1 %>% left_join(df2, by = "id", suffix = c(".df1", ".df2"))

# _5.7 right_join ====
df1 %>% right_join(df2, by = "id", suffix = c(".df1", ".df2"))

# _5.8 inner_join ====
df1 %>% inner_join(df2, by = "id", suffix = c(".df1", ".df2"))

# _5.9 semi_join ====
df1 %>% semi_join(df2, by = "id")
df2 %>% semi_join(df1, by = "id")

# _5.10 anti_join ====
df1 %>% anti_join(df2, by = "id")
df2 %>% anti_join(df1, by = "id")

# 6. Dessert (tidyr) ==== 

# _6.1 gather ====

# _6.2 spread ====


