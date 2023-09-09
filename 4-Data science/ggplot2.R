# Reference ====
# For online documentation refer : https://r4ds.had.co.nz 
browseVignettes(package = "ggplot2")

# 1. About ====
# Grammar of Graphics
utils::packageVersion("ggplot2") 
packageDescription("ggplot2")

# A geom is the geometrical object that a plot uses to represent data.
# For example, bar charts use bar geoms, line charts use line geoms, 
# boxplots use boxplot geoms, and so on. 

# 2. Setup ====

# _2.1 Initialize ====
library(dplyr)
library(ggplot2)

# _2.2 Data ====
?mpg
df <- mpg %>% as_tibble()
df

# 3. Basic plots ====

# _3.1 geom_point ====

df %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  theme_gray()

df %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "blue", size = 2)

# difference between defining parameters inside aesthetics and outside ?
df %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class))

df %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class, size = cyl))

df %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class, size = cyl, shape = fl)) # has 25 built in shapes

df %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class, size = cyl, shape = fl, alpha = hwy))

# _3.2 facets ====
# split your plot into subplots that each display one subset of the data.
df %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~ class)

df %>% 
  ggplot(aes(x = displ, y = hwy)) + 
  geom_point() +
  facet_grid(drv ~ cyl)

# _3.3 geom_smooth ====
df %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

df %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(linetype = drv)) # equivalent to group = drv

df %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(aes(color = drv)) # show.legend = FALSE
  
# do more --> use smooth for sub sample, useful for some analysis
df %>% 
  ggplot(aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) + 
  geom_smooth(data = filter(df, class == "subcompact"), se = T)

# _3.4 geom_boxplot ====
df %>% 
  ggplot(aes(x = manufacturer, y = hwy)) +
  geom_boxplot() +
  coord_flip()

# 4. Statistical transformations ====
# Following plots reveals something subtle about data

# _4.1 geom_bar ====
# args(geom_bar): You can learn which stat a geom uses by inspecting the 
# default value for the stat argument

df %>% 
  ggplot(aes(x = manufacturer)) +
  geom_bar(stat = "count") +
  # stat_count() +
  # geom_text(stat = 'count', aes(label = ..count..), hjust = -0.5) +
  coord_flip()
  
# df %>% count(manufacturer)

tibble(university = c("MG", "Calicut", "Kerala", "CUSAT"),
       noOfStudents = c(120, 140, 90, 150),
       avgMarks = c(40, 50, 60, 70)) %>% 
  ggplot(aes(x = university, y = avgMarks)) +
  geom_bar(stat = "identity")

df %>% 
  count(manufacturer) %>%
  ggplot(aes(x = manufacturer, y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

# _4.2 stat_summary ====
# There are over 20 stats for you to use. Each stat is a function
# https://ggplot2.tidyverse.org/reference/stat_summary.html 
df %>% 
  ggplot(aes(x = manufacturer, y = displ)) +
  stat_summary(
    fun.min = min,
    fun = "median", 
    fun.max = max) +
  stat_summary(fun = "mean", colour = "red", geom = "point", group = 1) +
  coord_flip()
# use geom = "line" to get lines connecting the means.

# 5. Position adjustments ====
df %>% 
  ggplot(aes(x = manufacturer, colour = trans)) +
  geom_bar() +
  coord_flip()

df %>% 
  ggplot(aes(x = manufacturer, fill = trans)) +
  geom_bar() +
  coord_flip()

# _5.1 position = "identity" ====
# will place each object exactly where it falls in the context of the graph. 
# This is not very useful for bars, because it overlaps them

df %>% 
  ggplot(aes(x = manufacturer, fill = trans)) +
  geom_bar(position = "identity", alpha = 0.5) + 
  coord_flip()

df %>% 
  count(manufacturer, trans) %>% 
  tidyr::spread(trans, n) %>% 
  print(width = Inf)

df %>% 
  ggplot(aes(x = manufacturer, colour = trans)) +
  geom_bar(position = "identity", fill = NA) + 
  # geom_text(stat = 'count', aes(label = ..count..), hjust = 2) +
  coord_flip()

# _5.2 position = "fill" ====
# works like stacking, but makes each set of stacked bars the same height. 
# This makes it easier to compare proportions across groups.
df %>% 
  ggplot(aes(x = manufacturer, fill = trans)) +
  geom_bar(position = "fill", alpha = 0.5) + 
  coord_flip()

# _5.3 position = "dodge" ====
# places overlapping objects directly beside one another. This makes it easier 
# to compare individual values.
df %>% 
  ggplot(aes(x = manufacturer, fill = trans)) +
  geom_bar(position = "dodge") + 
  coord_flip()

# 6. Coordinate systems ====

# _6.1 coord_flip ====
# Already explained 

# _6.2 coord_polar ====
df %>% 
  ggplot(aes(x = manufacturer)) +
  geom_bar() +
  coord_polar()

# _6.3 coord_quickmap ====
# This is useful if you’re plotting spatial data with ggplot2

map_data("nz") %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# 7. Themes and templates ====
df %>% 
  ggplot(aes(x = manufacturer, fill = trans)) +
  geom_bar(position = "fill") + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))

# 8. Layered grammar ====

# The grammar of graphics is based on the insight that you can uniquely 
# describe any plot as a combination of a dataset, a geom, a set of mappings, 
# a stat, a position adjustment, a coordinate system, and a faceting scheme

