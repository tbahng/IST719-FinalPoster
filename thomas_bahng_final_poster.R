#########################################################
# Thomas bahng
# IST 719 Final Poster Code
# Title: Gender Roles and Popularity in Human History
#########################################################
library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(treemap)
library(gdata)
library(ggmap)
library(plotrix)
library(maps)
library(arules)
library(arulesViz)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggpubr)
library(viridis)

# read data
# download from https://www.kaggle.com/mit/pantheon-project
df <- read.csv('database.csv', stringsAsFactors = FALSE)

# structure of data
str(df)

# dataset size calculation notes
number_of_columns <- ncol(df)
number_of_rows <- nrow(df)
score <- (number_of_columns * 4) * (number_of_rows / 100)
print(score)

#########################################################
# Cleaning
#########################################################
# convert birth_year to numeric
numeric_birth_year <- as.numeric(df$birth_year) # try conversion
idx <- which(is.na(numeric_birth_year)) # indices of incorrect values
df$birth_year[idx] # inspect incorrect values
#View(df[idx,])
df$birth_year <- numeric_birth_year

# assign unknown or missing values to unspecified: continent
df$continent[df$continent == 'Unknown' | df$continent == ''] <- 'Unspecified'

# assign unknown or missing values to unspecified: country
df$country[df$country == 'Unknown' | df$country == ''] <- 'Unspecified'

#########################################################
# color options
#########################################################
my.colors <- c('#372759','#8491D9','#F2F2F2','#8C8C8C','#590B0B')
show_col(my.colors)
# https://en.wikipedia.org/wiki/Main_Page
wiki.colors <- c('#F2F2F2','#A4A5A6','#737373','#404040','#0D0D0D')
show_col(wiki.colors)
# https://pantheon.world/explore/viz?viz=map&show=occupations&years=-3501,2015&place=dnk
panth.colors <- c('#1A2873','#66B1F2','#F2EDA7','#F2F2F2','#A66226')
show_col(panth.colors)
# tree
tree.colors <- c('#D973AB','#AE84D9','#03A678','#97A624','#A68D14')
show_col(tree.colors)
# gender colors
gender.cols <- c('male' = '#AE84D9', 'female' = '#03A678')
single.dim.col <- "#590B0B"
show_col(gender.cols)
show_col(single.dim.col)
# exemplar colors
exemplar.colors <- c('#F2293A','#595859','#1763A6','#3CA64C','#A63333')
show_col(exemplar.colors)

#########################################################
# Histogram of Historical Popularity Index
#########################################################
par(mar = c(5, 5, 4, 2) + 0.1)
hist(
  df$historical_popularity_index
  , col = single.dim.col
  , las = 2
  , main = "Distribution of Historical Popularity Index"
  , xlab = "Historical Popularity Index"
  , border = 'white'
)
par(mar = c(5, 4, 4, 2) + 0.1)
summary(df$historical_popularity_index)


# overall distribution of popularity
p1 <- ggplot(df, aes(historical_popularity_index)) +
  geom_density(fill = single.dim.col)

# distribution of HPI by gender
mu <- ddply(df, 'sex', summarise, grp.mean = mean(historical_popularity_index))
p2 <- ggplot(df, aes(historical_popularity_index, fill = sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c(gender.cols[['female']],gender.cols[['male']])) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = sex),
             linetype="dashed") +
  scale_color_manual(values = c(gender.cols[['female']],gender.cols[['male']])) +
  theme(legend.position="top") +
  theme_bw()
p2

#########################################################
# Histogram of Article Languages
#########################################################
par(mar = c(5, 5, 4, 2) + 0.1)
hist(
  df$article_languages
  , col = single.dim.col
  , las = 2
  , main = "Distribution of Article Languages"
  , xlab = "Article Languages"
  , border = 'white'
)
par(mar = c(5, 4, 4, 2) + 0.1)
summary(df$article_languages)

# distribution of Article Languages by gender
mu <- ddply(df, 'sex', summarise, grp.mean = mean(article_languages))
p <- ggplot(df, aes(article_languages, fill = sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("#A68D14","#D973AB")) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = sex),
             linetype="dashed") +
  scale_color_manual(values = c("#A68D14","#D973AB")) +
  theme(legend.position="top")
p

#########################################################
# Histogram of Total Page Views
#########################################################
par(mar = c(5, 5, 4, 2) + 0.1)
hist(
  df$page_views
  , col = single.dim.col
  , las = 2
  , main = "Distribution of Total Page Views"
  , xlab = "Page Views"
  , border = 'white'
)
par(mar = c(5, 4, 4, 2) + 0.1)
summary(df$page_views)

# distribution of Total Page Views by gender
mu <- ddply(df, 'sex', summarise, grp.mean = mean(page_views))
p <- ggplot(df, aes(page_views, fill = sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("#A68D14","#D973AB")) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = sex),
             linetype="dashed") +
  scale_color_manual(values = c("#A68D14","#D973AB")) +
  theme(legend.position="top")
p

#########################################################
# Histogram of Average Page Views
#########################################################
par(mar = c(5, 5, 4, 2) + 0.1)
hist(
  df$average_views
  , col = single.dim.col
  , las = 2
  , main = "Distribution of Average Page Views"
  , xlab = "Page Views"
  , border = 'white'
)
par(mar = c(5, 4, 4, 2) + 0.1)
summary(df$average_views)

# distribution of Average Page Views by gender
mu <- ddply(df, 'sex', summarise, grp.mean = mean(average_views))
p <- ggplot(df, aes(average_views, fill = sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("#A68D14","#D973AB")) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = sex),
             linetype="dashed") +
  scale_color_manual(values = c("#A68D14","#D973AB")) +
  theme(legend.position="top")
p

#########################################################
# Histogram of Birth Year
#########################################################
par(mar = c(5, 5, 4, 2) + 0.1)
hist(
  df$birth_year
  , col = single.dim.col
  , las = 2
  , xlab = "Birth Year"
  , border = 'white'
  , main = "Distribution of Birth Year"
)
par(mar = c(5, 4, 4, 2) + 0.1)
summary(df$birth_year)

# distribution of birth year by gender
mu <- ddply(df, 'sex', summarise, grp.mean = mean(birth_year, na.rm = TRUE))
p <- ggplot(df, aes(birth_year, fill = sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c(gender.cols[['female']],gender.cols[['male']])) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = sex),
             linetype="dashed") +
  scale_color_manual(values = c(gender.cols[['female']],gender.cols[['male']])) +
  theme(legend.position="top") +
  xlim(1500,2005) +
  theme_bw()
p

#########################################################
# Barplot of Sex
#########################################################
par(mar = c(5, 5, 4, 2) + 0.1)
dat <- sort(table(df$sex))
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = c(gender.cols[['female']],gender.cols[['male']])
  , main = "Distribution of Sex"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat

#########################################################
# Barplot of Top 10 Countries
#########################################################
par(mar = c(5, 7.5, 4, 2) + 0.1)
dat <- tail(sort(table(df$country)), 10)
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = single.dim.col
  , main = "Distribution of Top 10 Countries"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat

# by gender
dat <- ddply(df, .(country, sex), summarise, value = length(article_id))
dat <- dat[order(dat$sex, -dat$value),]

p1 <- ggplot(head(dat[dat$sex=='Male',],10), aes(reorder(x=country, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['male']) + xlab('Country') + ylab("Count") +
  coord_flip() + ggtitle("Top 10 Countries: Male")
p2 <- ggplot(head(dat[dat$sex=='Female',],10), aes(reorder(x=country, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['female']) + xlab('Country') + ylab("Count") +
  coord_flip() + ggtitle("Top 10 Countries: Female")
grid.arrange(p1,p2)

#########################################################
# Barplot of Continents
#########################################################
par(mar = c(5, 7.5, 4, 2) + 0.1)
dat <- sort(table(df$continent))
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = single.dim.col
  , main = "Distribution of Continents"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat

# by gender
dat <- ddply(df, .(continent, sex), summarise, value = length(article_id))
dat <- dat[order(dat$sex, -dat$value),]

p1 <- ggplot(head(dat[dat$sex=='Male',],10), aes(reorder(x=continent, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['male']) + xlab('Continent') + ylab("Count") +
  coord_flip() + ggtitle("Continents: Male")
p2 <- ggplot(head(dat[dat$sex=='Female',],10), aes(reorder(x=continent, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['female']) + xlab('Continent') + ylab("Count") +
  coord_flip() + ggtitle("Continents: Female")
grid.arrange(p1,p2)

#########################################################
# Barplot of Top 10 Occupations
#########################################################
par(mar = c(5, 7.5, 4, 2) + 0.1)
dat <- tail(sort(table(df$occupation)), 10)
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = single.dim.col
  , main = "Distribution of Top 10 Occupations"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat

# by gender
dat <- ddply(df, .(occupation, sex), summarise, value = length(article_id))
dat <- dat[order(dat$sex, -dat$value),]

p1 <- ggplot(head(dat[dat$sex=='Male',],10), aes(reorder(x=occupation, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['male']) + xlab('Occupation') + ylab("Count") +
  coord_flip() + ggtitle("Top 10 Occupations: Male")
p2 <- ggplot(head(dat[dat$sex=='Female',],10), aes(reorder(x=occupation, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['female']) + xlab('Occupation') + ylab("Count") +
  coord_flip() + ggtitle("Top 10 Occupations: Female")
grid.arrange(p1,p2)

#########################################################
# Barplot of Top 10 Industries
#########################################################
par(mar = c(5, 7.5, 4, 2) + 0.1)
dat <- tail(sort(table(df$industry)), 10)
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = single.dim.col
  , main = "Distribution of Top 10 Industries"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat

# by gender
dat <- ddply(df, .(industry, sex), summarise, value = length(article_id))
dat <- dat[order(dat$sex, -dat$value),]

p1 <- ggplot(head(dat[dat$sex=='Male',],10), aes(reorder(x=industry, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['male']) + xlab('Industry') + ylab("Count") +
  coord_flip() + ggtitle("Top 10 Industries: Male")
p2 <- ggplot(head(dat[dat$sex=='Female',],10), aes(reorder(x=industry, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['female']) + xlab('Industry') + ylab("Count") +
  coord_flip() + ggtitle("Top 10 Industries: Female")
grid.arrange(p1,p2)

#########################################################
# Barplot of Domain
#########################################################
par(mar = c(5, 9.4, 4, 2) + 0.1)
dat <- sort(table(df$domain))
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = single.dim.col
  , main = "Distribution of Domain"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat

# by gender
dat <- ddply(df, .(domain, sex), summarise, value = length(article_id))
dat <- dat[order(dat$sex, -dat$value),]

p1 <- ggplot(head(dat[dat$sex=='Male',],10), aes(reorder(x=domain, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['male']) + xlab('Domain') + ylab("Count") +
  coord_flip() + ggtitle("Domains: Male")
p2 <- ggplot(head(dat[dat$sex=='Female',],10), aes(reorder(x=domain, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['female']) + xlab('Domain') + ylab("Count") +
  coord_flip() + ggtitle("Domains: Female")
grid.arrange(p1,p2)

#########################################################
# Top 15 Historical Figures by HPI
#########################################################
par(mar = c(5, 9.6, 4, 2) + 0.1)
dat <- tail(sort(tapply(df$historical_popularity_index, df$full_name, sum)), 15)
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = single.dim.col
  , main = "Top 15 Historical Figures by HPI"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat
most_popular <- names(dat)

# by gender
dat <- ddply(df, .(full_name, sex), summarise, value = mean(historical_popularity_index))
dat <- dat[order(dat$sex, -dat$value),]

p1 <- ggplot(head(dat[dat$sex=='Male',],10), aes(reorder(x=full_name, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['male']) + xlab('Name') + ylab("Historical Popularity Index") +
  coord_flip() + ggtitle("Names: Male")
p2 <- ggplot(head(dat[dat$sex=='Female',],10), aes(reorder(x=full_name, value), y = value)) +
  geom_bar(stat = 'identity', fill = gender.cols['female']) + xlab('Name') + ylab("Historical Popularity Index") +
  coord_flip() + ggtitle("Names: Female")
grid.arrange(p1,p2)

# male
## Aristotle (-384)
## Plato (-427)
## Jesus Christ (-4)
## Socrates (-469)
## Alexander the Great (-356)

# female
## Cleopatra VII of Egypt (-69)
## Nefertiti (-1370)
## Jeanne d'Arc (1412)
## Mary (-18)
## Sappho (-625)
top.names = c(
  'Aristotle','Plato','Jesus Christ','Socrates','Alexander the Great',
  'Cleopatra VII of Egypt','Nefertiti',"Jeanne d'Arc","Mary","Sappho"
)
df[df$full_name %in% top.names,c('full_name','birth_year')]
#########################################################
# treemap of popularity by sex and occupation
#########################################################

pdf_file <- 'treemap_hpi_by_occupation_sex.pdf'
cairo_pdf(bg="grey98", pdf_file, width=11.69, height=7.5)
par(omi=c(0.55,0.25,1.15,0.75), las=1)
plot.new()

group <- df$sex
subgroup <- df$occupation
value <- df$historical_popularity_index
dat <- data.frame(group, subgroup, value)

treemap(
  dat
  , index = c("group", "subgroup")
  , vSize = "value"
  , type = "index"
  , title = ""
  #, inflate.labels = TRUE
  , fontsize.labels = c(36,12)
  , aspRatio = 1.9
)
mtext("Occupations by Gender", side=3, line=3.8, adj=0, cex=2.2, outer = TRUE) # title
mtext("(Multidimensional) Size: Historical Popularity Index", side=3, line=2.3, adj=0, cex=1.5, outer = TRUE) # subtitle
mtext("Source: pantheon-project | Thomas Bahng", side = 1, line = 1, adj = 1, cex = 0.8, family = 'serif', outer = TRUE) # source
dev.off()

#########################################################
# When did the most memorable people live?
#########################################################

# color range
low <- '#FAB1B2'
high <- '#590B0B'
col.keep <- c('birth_year','historical_popularity_index')
dat <- df[order(df$birth_year), col.keep]
dat <- dat[complete.cases(dat),]
dat <- aggregate(dat$historical_popularity_index, by = list(dat$birth_year), mean)
colnames(dat) <- c('birth_year','mean_hpi')
ggplot(dat, aes(x = birth_year, y = factor(0), fill = mean_hpi)) +
  geom_tile() + ylab(NULL) +
  xlim(min(dat$birth_year), max(dat$birth_year)) +
  scale_fill_gradient(low = low, high = high) +
  theme(legend.position="bottom")

# by gender
col.keep <- c('birth_year','historical_popularity_index')
dat <- df[df$sex == 'Male', col.keep]
dat <- dat[order(dat$birth_year), ]
dat <- dat[complete.cases(dat),]
dat <- aggregate(dat$historical_popularity_index, by = list(dat$birth_year), mean)
colnames(dat) <- c('birth_year','mean_hpi')
p1 <- ggplot(dat, aes(x = birth_year, y = factor(0), fill = mean_hpi)) +
  geom_tile() + ylab(NULL) +
  xlim(min(dat$birth_year), max(dat$birth_year)) +
  scale_fill_gradient(low = 'white', high = gender.cols['male']) +
  theme(legend.position="right") +
  ggtitle("Timeline of Famous Males")

col.keep <- c('birth_year','historical_popularity_index')
dat <- df[df$sex == 'Female', col.keep]
dat <- dat[order(dat$birth_year), ]
dat <- dat[complete.cases(dat),]
dat <- aggregate(dat$historical_popularity_index, by = list(dat$birth_year), mean)
colnames(dat) <- c('birth_year','mean_hpi')
p2 <- ggplot(dat, aes(x = birth_year, y = factor(0), fill = mean_hpi)) +
  geom_tile() + ylab(NULL) +
  xlim(min(dat$birth_year), max(dat$birth_year)) +
  scale_fill_gradient(low = 'white', high = gender.cols['female']) +
  theme(legend.position="right") +
  ggtitle("Timeline of Famous Females")

grid.arrange(p1, p2, nrow = 2)

#########################################################
# How has the demographic of the pantheon changed over time?
# geographic footprint
#########################################################

col.keep <- c('sex','birth_year','latitude','longitude','historical_popularity_index')
dat <- df[, col.keep]
dat <- dat[complete.cases(dat),]
dat$time_range <- ifelse(dat$birth_year < 0, "1) year < 0",
                         ifelse(dat$birth_year < 1800, "2) 0 to 1799",
                                ifelse(dat$birth_year < 1900, "3) 1800 < 1899",
                                       "4) 1900 - present")))
seg <- unique(dat$time_range)
m.col <- alpha("#590B0B", 0.4) # color of male points
f.col <- alpha("#372759", 0.4) # color of female points
my.col <- ifelse(dat$sex == "Male", m.col, f.col) # color of points
map.col <- '#8C8C8C'

par(mfrow = c(2,2), mar = c(0,0,0,0), oma = c(0,0,0,0))
# world map: 1) year < 0
wm <- dat[dat$time_range == seg[1],]
map(
  'world', fill = TRUE, col = map.col, border = 'white', 
  main = paste("Time Range:", wm$time_range[1])
)
points(
  wm$longitude
  , wm$latitude
  , col = my.col
  , pch = 16
)

# world map: "2) 0 to 1799"
wm <- dat[dat$time_range == seg[2],]
map('world', fill = TRUE, col = map.col, border = 'white')
points(
  wm$longitude
  , wm$latitude
  , col = my.col
  , pch = 16
  , main = paste("Time Range:", wm$time_range[1])
)

# world map: "3) 1800 < 1899"
wm <- dat[dat$time_range == seg[3],]
map('world', fill = TRUE, col = map.col, border = 'white')
points(
  wm$longitude
  , wm$latitude
  , col = my.col
  , pch = 16
  , main = paste("Time Range:", wm$time_range[1])
)

# world map: "4) 1900 - present"
wm <- dat[dat$time_range == seg[4],]
map('world', fill = TRUE, col = map.col, border = 'white')
points(
  wm$longitude
  , wm$latitude
  , col = my.col
  , pch = 16
  , main = paste("Time Range:", wm$time_range[1])
)
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

#########################################################
# How has the demographic of the pantheon changed over time?
# trend of observations by gender
#########################################################
dat <- aggregate(df$article_id, by = list(df$birth_year, df$sex), length)
colnames(dat) <- c('birth_year','sex','count')
dat.m <- dat[dat$sex == 'Male',]
dat.m$cumcount <- cumsum(dat.m$count)
dat.f <- dat[dat$sex == 'Female',]
dat.f$cumcount <- cumsum(dat.f$count)
dat <- rbind(dat.m, dat.f)
dat$logcum <- log(dat$cumcount)
ggplot(dat, aes(x = birth_year, y = logcum, col = sex)) + 
  geom_line(size = 1.5) +
  theme_bw() +
  scale_colour_manual(values = c("#A68D14","#D973AB")) +
  xlab("Birth Year") + ylab("Log Cumulative Growth") +
  geom_smooth(method = 'loess', linetype="dashed") +
  ggtitle("Growth Over Time by Gender")

#########################################################
# How does gender, age, occupation, industry and domain influence historical popularity?
#########################################################

keep.cols <- c(
  'sex','birth_year','occupation','industry',
  'domain','historical_popularity_index'
)
dat <- df[, keep.cols]
colnames(dat)[6] <- 'hpi'
dat <- dat %>% 
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_if(is.numeric, funs(discretize))
#########################################################
# association network graph
#########################################################
# all years
rules <- apriori(
  dat, parameter = list(
    supp = 0.001, conf = 0.8, maxlen = 4
  ),
  appearance = list(default = 'lhs', rhs = "hpi=[24.62,31.99]")
)
summary(rules)
inspect(rules)
plot(rules, method = 'graph', interactive = TRUE)

# in the last 200 years
dat <- df[df$birth_year >= 1800, keep.cols]
colnames(dat)[6] <- 'hpi'
dat <- dat %>% 
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_if(is.numeric, funs(discretize))
rules <- apriori(
  dat, parameter = list(
    supp = 0.0002, conf = 0.9, maxlen = 4
  ),
  appearance = list(default = 'lhs', rhs = "hpi=[23.68,30.58]")
)
summary(rules)
inspect(rules)
plot(rules, method = 'graph', interactive = TRUE)


#########################################################
# How does gender, age, and domain influence historical popularity?
# Decision Tree
#########################################################

# all years
keep.cols <- c(
  'sex'
  , 'birth_year'
  , 'domain'
  , 'historical_popularity_index'
)
dat <- df[, keep.cols]
colnames(dat)[4] <- 'hpi'
dat <- dat %>%
  mutate_if(is.character, funs(as.factor))
dat$birth_year <- ifelse(
  dat$birth_year >= 1800, "Post 1800s", "Pre 1800s"
) %>% as.factor
dat$hpi <- discretize(dat$hpi, categories = 5, method = 'frequency')

fit <- rpart(hpi ~ ., data = dat, method = 'class')
rpart.plot(fit, main = "Decision Tree", cex.main = 1.5)

# in the last 200 years
dat <- df[df$birth_year >= 1800, keep.cols]
colnames(dat)[4] <- 'hpi'
dat <- dat %>% 
  mutate_if(is.character, funs(as.factor)) %>%
  mutate_if(is.numeric, funs(discretize(., categories = 5)))

fit <- rpart(hpi ~ ., data = dat, method = 'class')
rpart.plot(fit, main = "Decision Tree", cex.main = 1.5
           , type = 2
           , extra = 4
           )

#########################################################
# How does gender, age, and domain influence historical popularity?
# Balloon plot
#########################################################

# all years
keep.cols <- c(
  'sex','birth_year',
  'domain','historical_popularity_index'
)
dat <- df[, keep.cols]
colnames(dat)[4] <- 'hpi'
dat <- dat %>% 
  mutate_if(is.character, funs(as.factor))
dat$birth_year <- discretize(dat$birth_year, categories = 5, method = 'frequency')
dat <- dat[complete.cases(dat),] %>% unique
dat <- dat %>% group_by(sex, birth_year, domain) %>% summarise(hpi = mean(hpi))

ggballoonplot(dat
              , x = "domain"
              , y = "birth_year"
              , size = "hpi"
              , fill = "hpi"
              , facet.by = "sex"
              , ggtheme = theme_bw()) +
  scale_fill_viridis()

