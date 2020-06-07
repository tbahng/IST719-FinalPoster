#########################################################
# Thomas bahng
# IST 719 IST719-M401
# Final Poster Code
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

# Data Description
# The data set is an index of historical figures from 
# Wikipedia consisting of 11341 observations and 17 variables. 
# It was developed by the Macro Connections group at the Massachusetts 
# Institute of Technology Media Lab and made available on Kaggle, 
# Pantheon Project: Historical Popularity Index.
# download from https://www.kaggle.com/mit/pantheon-project

# Poster Story
# Recorded human history has existed for over 5000 years, and key
# figures have lived dispersed throughout time and societal roles

# read data
df <- read.csv('data/database.csv', stringsAsFactors = FALSE)

# structure of data
str(df)

# dataset size calculation notes
number_of_columns <- ncol(df)
number_of_rows <- nrow(df)
score <- (number_of_columns * 4) * (number_of_rows / 100)
print(score)
# 7711

#########################################################
# Cleaning
#########################################################
# convert birth_year to numeric
numeric_birth_year <- as.numeric(df$birth_year) # try conversion
idx <- which(is.na(numeric_birth_year)) # indices of incorrect values
df$birth_year[idx] # inspect incorrect values
df$birth_year <- numeric_birth_year

# assign unknown or missing values to unspecified: continent
df$continent[df$continent == 'Unknown' | df$continent == ''] <- 'Unspecified'

# assign unknown or missing values to unspecified: country
df$country[df$country == 'Unknown' | df$country == ''] <- 'Unspecified'

#########################################################
# color options
#########################################################
my.colors <- c('#372759','#8491D9','#F2F2F2','#8C8C8C','#590B0B')

# https://en.wikipedia.org/wiki/Main_Page
wiki.colors <- c('#F2F2F2','#A4A5A6','#737373','#404040','#0D0D0D')

# https://pantheon.world/explore/viz?viz=map&show=occupations&years=-3501,2015&place=dnk
panth.colors <- c('#1A2873','#66B1F2','#F2EDA7','#F2F2F2','#A66226')

# tree map default colors
tree.colors <- c('#D973AB','#AE84D9','#03A678','#97A624','#A68D14')

# gender colors - derived from tree map colors
gender.cols <- c('male' = '#AE84D9', 'female' = '#03A678')
single.dim.col <- "#590B0B"

# exemplar colors - from class
exemplar.colors <- c('#F2293A','#595859','#1763A6','#3CA64C','#A63333')

# not all colors defined are used in project
# show_col(my.colors)
# show_col(wiki.colors)
# show_col(panth.colors)
# show_col(tree.colors)
# show_col(gender.cols)
# show_col(single.dim.col)
# show_col(exemplar.colors)

#########################################################
# Gender Distribution
#########################################################
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
# males are generally more popular than females
# both males and females are slightly left-skewed in terms of popularity but overall evenly
## distributed, which mirrors the population distribution.

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
# most females in the data were born post-1900

# Barplot of Sex
par(mar = c(5, 5, 4, 2) + 0.1)
dat <- sort(table(df$sex))
barplot(
  dat
  , las=2
  , horiz = TRUE
  , col = c(gender.cols[['female']],gender.cols[['male']])
  , main = "Distribution of Gender"
  , xlab = "Number of Observations"
)
par(mar = c(5, 4, 4, 2) + 0.1)
dat
# the number of male individuals vastly outweigh the number of females


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
# some individual names are duplicated, skewing the popularity measure.

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
col.keep <- c('full_name','sex','birth_year','occupation','historical_popularity_index')
df[df$full_name %in% top.names, col.keep]

# Most popular males from 1800 to present
df[df$sex == 'Male' & df$birth_year >= 1800, col.keep] %>%
  .[order(.$historical_popularity_index, decreasing = TRUE),] %>% head()
# Most popular females from 1800 to present
df[df$sex == 'Female' & df$birth_year >= 1800, col.keep] %>%
  .[order(.$historical_popularity_index, decreasing = TRUE),] %>% head()
#########################################################
# treemap of popularity by sex and occupation
#########################################################

#pdf_file <- 'plots/treemap_hpi_by_occupation_sex.pdf'
#cairo_pdf(bg="grey98", pdf_file, width=15.9, height=7.5)
par(omi=c(0.55,0.25,1.15,0.75), las=1)
#par(omi=c(1.1,0.5,2.3,1.5), las=1)
#plot.new()

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
  , fontsize.labels = c(48,24)
  , aspRatio = 2.1
)
#mtext("Occupations by Gender", side=3, line=3.8, adj=0, cex=2.2, outer = TRUE) # title
#mtext("(Multidimensional) Size: Historical Popularity Index", side=3, line=2.3, adj=0, cex=1.5, outer = TRUE) # subtitle
#mtext("Source: pantheon-project | Thomas Bahng", side = 1, line = 1, adj = 1, cex = 0.8, family = 'serif', outer = TRUE) # source
#dev.off()
# a stark contrast can be seen between typical occupations for males and females

#########################################################
# When did the most memorable people live?
#########################################################

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
# How does gender, age, and domain influence historical popularity?
# Decision Tree
#########################################################

# data: gender, birth year, domain, and popularity (discretized)
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

# variable importance
fit$variable.importance
# When predicting historical popularity, domain is considered most important, 
## followed by birth year and gender.
# Individuals in Sports tend to be less popular.
# Individuals born before the 1800s tend to be more popular.
# Gender is not a great predictor of popularity.

#########################################################
# How does gender, age, and domain influence historical popularity?
# Balloon plot
#########################################################

# data: Mean popularity by gender, birth year, and domain
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

# On average, females were historically reknown for their roles in science & technology
# Whereas males were reknown for their roles in the humanities and science.
# There is a negative relationship between popularity and birth year across gender and domain.
