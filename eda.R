
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(ggrepel)

dataset <- read.csv('C:/Users/srika/Documents/R/RPrograms/movie_metadata.csv', fill = TRUE)
dataset


summary(dataset)
head(dataset)
options(repr.plot.width=6, repr.plot.height=4) 

graph1 <- ggplot(dataset, aes(x = duration)) +
  geom_histogram(binwidth = 5, aes(y = ..density.., fill = 'red'))

graph2 <- ggplot(dataset, aes(x = num_voted_users)) +
  geom_histogram(binwidth = 500, aes(y= ..density..),  fill = 'green3')
graph2

graph3 <- ggplot(dataset, aes(x = content_rating, color = content_rating)) +
  geom_boxplot(aes(y = dataset$imdb_score))

graph4 <- ggplot(dataset, aes(x = title_year)) + 
  geom_histogram(aes(y = ..count..), fill = 'blue2')

graph5 <- ggplot(dataset, aes(x = num_critic_for_reviews, color = 'purple')) +
  geom_point(aes(y = dataset$gross))
graph5

graph6 <- ggplot(dataset, aes(x = facenumber_in_poster)) +
  geom_histogram(aes(y = dataset$num_critic_for_reviews))
graph6


densityp1 <- dataset %>%
  ggplot(aes(x=num_voted_users, y = imdb_score)) +
  stat_density_2d(geom = 'tile', aes(fill = ..density..),h = c(5000000, 7.5), contour = FALSE) +
  scale_fill_viridis()
densityp1
  

densityp2 <- dataset %>%
  ggplot(aes(x=duration, y = imdb_score)) +
  stat_density_2d(geom = 'tile', aes(fill = ..density..),h=c(200,7.5), contour = FALSE) +
  scale_fill_viridis()
densityp2

densityp3 <- dataset %>%
  ggplot(aes(x=cast_total_facebook_likes, y = imdb_score)) +
  stat_density_2d(geom = 'tile', aes(fill = ..density..),h=c(500000,7.5), contour = FALSE) +
  scale_fill_viridis()
densityp3

dataset %>%
  filter(!content_rating == '') %>%
  ggplot(aes(x = content_rating, y= imdb_score, fill = content_rating, col = content_rating)) +
  geom_point(size = 0.5) +
  geom_boxplot() +
  stat_summary(fun.data = 'mean_cl_normal', geom = 'crossbar', width = 0.2, color = 'red') +
  theme(legend.position = 'none', axis.text.x = element_text(angle=90)) +
  scale_y_log10()


dataset %>%
  filter(!country == '') %>%
  ggplot(aes(x = country, y= imdb_score, fill = country, color = country)) +
  geom_point(size = 0.5) +
  geom_boxplot() +
  stat_summary(fun.data = 'mean_cl_normal', geom = 'crossbar', width = 0.2, color = 'red') +
  theme(legend.position = 'none', axis.text.x = element_text(angle=90)) +
  scale_y_log10()

dataset %>%
  filter(!content_rating == '') %>%
  select(title_year, content_rating) %>%
  group_by(content_rating, title_year) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(x = title_year, y = cnt, fill = content_rating)) +
  geom_bar(stat = 'identity') +
  labs(title=" Distribution of Movies with content rating")

dataset%>%
  filter(!num_user_for_reviews =='')%>%
  select(title_year,num_user_for_reviews)%>%
  group_by(title_year, num_user_for_reviews)%>%
  ggplot(aes(x=title_year,y=num_user_for_reviews, fill = title_year))+
  geom_bar(stat="identity")+
  labs(title=" Distribution of Movies with number of user reviews")

dataset %>%
  filter(!duration == '') %>%
  select(title_year, duration) %>%
  group_by(title_year, duration) %>%
  ggplot(aes(x = title_year, y = duration, fill = duration)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Duration of movies by year')

dataset %>%
  ggplot(aes(x = title_year, y = facenumber_in_poster, color = cut(facenumber_in_poster, 2))) +
  geom_point() +
  scale_x_continuous(limits = c(1920, 2020)) +
  scale_color_manual(values = c('green', 'red'))

dataset %>%
  filter(!aspect_ratio == '') %>%
  ggplot(aes(x = aspect_ratio, y = imdb_score, color = cut(imdb_score, 2))) +
  geom_point() +
  scale_x_continuous(limits = c(0, 5)) +
  scale_color_manual(values = c('blue', 'orange'))

dataset %>%
  ggplot(aes(x = title_year, y = imdb_score, color = cut(imdb_score, 2))) +
  geom_point() +
  scale_x_continuous(limits = c(1920, 2020)) +
  scale_color_manual(values = c('blue', 'orange'))

graph7 <- ggplot(dataset, aes(x = num_voted_users)) +
  geom_histogram(binwidth = 50000, aes(y = ..density..), fill = 'orange')
graph7


graph8 <- ggplot(dataset, aes(x = num_critic_for_reviews)) +
  geom_histogram(binwidth = 50, aes(y = ..density..), fill = 'purple')
graph8


graph9 <- dataset %>%
  select(movie_title, num_critic_for_reviews, imdb_score) %>%
  arrange(desc(num_critic_for_reviews)) %>%
  ggplot(aes(x = num_critic_for_reviews, y= imdb_score, color = cut(imdb_score, 5))) +
  geom_point() +
  geom_jitter() +
  labs(title = 'number of critic reviews vs imdb score', color = 'imdb score') +
  theme(legend.position = 'bottom')
graph9
graph10 <-dataset%>%
  select(movie_title,num_voted_users,imdb_score)%>%
  arrange(desc(num_voted_users))%>%
  ggplot(aes(x=num_voted_users,y=imdb_score,col=cut(imdb_score,5)))+
  geom_point()+
  geom_jitter()+
  labs(title="number of users voted vs Imdb score",col="imdb score")+
  theme(legend.position = "bottom")
graph10


graph11 <- dataset %>%
  select(movie_facebook_likes, imdb_score) %>%
  arrange(desc(movie_facebook_likes)) %>%
  ggplot(aes(x = movie_facebook_likes, y= imdb_score, color = cut(imdb_score, 5))) +
  geom_point() +
  geom_jitter() +
  labs(title='movie facebook likes vs imdb score', color = 'imdb score') +
  theme(legend.position = 'bottom')
graph11  

lang1<-ggplot(dataset,aes(x=language,fill=language))+
  geom_histogram(stat="count",aes(y=..count../sum(..count..)),binwidth=1)+
  theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0),legend.position="none")+
  labs(y="Percent",title="Percentage of Movie Languages")
lang1

lang2 <- dataset %>%
  filter(!language == 'English') %>%
  ggplot(aes(x = language, fill = language)) +
  geom_histogram(stat = 'count', aes(y=..count../sum(..count..)), binwidth = 1) +
  theme(axis.text.x = element_text(angle=90, hjust=0.5, vjust=0), legend.position = 'none') +
  labs(y = 'Percent', title = 'Percentage of Movie Languages Excluding English')
lang2

dataset %>%
  select(director_name, imdb_score) %>%
  filter((imdb_score >= 9)) %>%
  filter((!director_name == '')) %>%
  arrange((imdb_score)) %>%
  distinct() %>%
  ggplot(aes(x=factor(director_name, levels = unique(director_name)), y = imdb_score, fill = imdb_score)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_continuous(low = 'blue', high = 'red') +
  geom_label(aes(label = director_name), hjust = 1.5) +
  labs(title = "Directors with Movies > 9", x = ' ') +
  theme(axis.text= element_blank(), axis.ticks = element_blank())

dataset %>% select(movie_title,imdb_score)%>% 
  filter(imdb_score>=9)%>%
  arrange((imdb_score))%>%
  distinct()%>%
  ggplot(aes(x=factor(movie_title,levels=movie_title),y=imdb_score,fill=imdb_score))+
  geom_bar(stat="identity")+
  coord_flip()+
  scale_fill_continuous(low="yellow",high="green")+
  geom_label(aes(label=movie_title),hjust=1.5)+
  labs(title="Movies Score >9",x=" ")+
  theme(axis.text=element_blank(),axis.ticks = element_blank())

year_movies <- dataset %>%
  group_by(title_year, country, language) %>%
  summarise(movie_count = n()) %>%
  filter(movie_count >= 5)

ggplot(year_movies, aes(x = title_year, y = movie_count, color = country)) +
  geom_point() +
  geom_line() +
  xlab('years') +
  ylab('Number of Movies') +
  xlim(c(1920, 2017)) +
  theme_classic()


movieYear <- dataset %>%
  select(country, title_year) %>%
  group_by(title_year, country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

movieYear$year <- NULL
movieYear$year <- ifelse(movieYear$title_year < 2000, 'Before 2000', 'After 2000')

movie1 <- movieYear %>%
  filter(!year == "NA" & count > 1) %>%
  ggplot(aes(x = country, y = count, fill = year, group = year)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom') +
  scale_fill_manual(values=c("green4","navyblue"))+
  labs(title="Movie Count per Country")
movie1

dataset %>%
  select(director_name, imdb_score) %>%
  group_by(director_name) %>%
  summarise(movie_count = n(), avg_score = mean(imdb_score)) %>%
  arrange(desc(movie_count)) %>%
  filter(!director_name == '' & movie_count >= 8) %>%
  ggplot(aes(x = factor(director_name, level = director_name), group = 1)) +
  geom_bar(aes(y=movie_count), stat = 'identity', fill = 'green') +
  geom_line(aes(y = avg_score), col = 'red', size = 1.5) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Movie count per director and average score', x='Director')

f1 <- dataset %>%
  ggplot(aes(x = movie_facebook_likes, y = imdb_score)) +
  geom_point(color = 'purple', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size=8)) +
  labs(title = 'Movie FB likes vs Score')
f1

f2 <- dataset %>%
  ggplot(aes(x = actor_1_facebook_likes, y = imdb_score)) +
  geom_point(color = 'red', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Actor 1 likes vs Score')
f2

f3 <- dataset %>%
  ggplot(aes(x = actor_2_facebook_likes, y = imdb_score)) +
  geom_point(color = 'darkred', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs('Actor 2 likes vs Score')
f3

f4 <- dataset %>%
  ggplot(aes(x = actor_3_facebook_likes, y = imdb_score)) +
  geom_point(color = 'maroon', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Actor 3 likes vs Score ')
f4

f5 <- dataset %>%
  ggplot(aes(x = director_facebook_likes, y = imdb_score)) +
  geom_point(color = 'green', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Director likes vs Score')
f5

f6 <- dataset %>%
  ggplot(aes(x = cast_total_facebook_likes, y = imdb_score, color = factor(content_rating))) +
  geom_point(alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Total Cast Facebooks Likes vs Score')
f6

f7 <- dataset %>%
  ggplot(aes(x = num_critic_for_reviews, y = imdb_score)) +
  geom_point(color = 'yellow', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Num critics vs Score')
f7

f8 <- dataset %>%
  ggplot(aes(x = num_voted_users, y = imdb_score)) +
  geom_point(color = 'darkred', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Num Users vs Score')
f8


f9 <- dataset %>%
  ggplot(aes(x = num_user_for_reviews, y = imdb_score)) +
  geom_point(color = 'yellow', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Num review Users vs Score')
f9

f10 <- dataset %>%
  filter(!budget == '') %>%
  ggplot(aes(x = budget, y = imdb_score)) +
  geom_point(color = 'hotpink', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Num Users vs Score')
f10

f11 <- dataset %>%
  ggplot(aes(x = duration, y = imdb_score)) +
  geom_point(color = 'pink', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Duration vs Score')
f11

f12 <- dataset %>%
  ggplot(aes(x = facenumber_in_poster, y = imdb_score)) +
  geom_point(color = 'hotpink', alpha = 0.4) +
  theme(legend.position = 'bottom', plot.title = element_text(size = 8)) +
  labs(title = 'Face Number vs Score')
f12

likes <- dataset %>%
  select(movie_title, movie_facebook_likes, director_facebook_likes, actor_1_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes) %>%
  filter(movie_facebook_likes != 0, director_facebook_likes != 0, actor_1_facebook_likes != 0, actor_2_facebook_likes != 0, actor_3_facebook_likes != 0) %>%
  gather(likes, value, 2:5)

l1 <- likes %>%
  filter(likes == 'movie_facebook_likes' & value > 100000) %>%
  ggplot(aes(x = factor(movie_title), y = value, group = likes, color = likes)) +
  geom_line(size = 2, color = 'purple') + 
  coord_polar() +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 50), legend.position = 'bottom') +
  labs(x = 'Movie', title = 'Most Facebook Likes by Movie', y = '')
l1

l2 <- likes %>%
  filter(likes == 'director_facebook_likes' & value > 20000) %>%
  ggplot(aes(x = factor(movie_title), y = value, group = likes, color = likes)) + 
  geom_line(size = 2, color = 'hotpink') +
  coord_polar() +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = 50), legend.position = 'bottom') +
  labs(x = 'Movie', title = 'Director Facebook likes by Movie', y = '')
l2



l3 <- likes %>%
  filter(likes == 'actor_1_facebook_likes' & value > 35000) %>%
  ggplot(aes(x = factor(movie_title), y = value, group = likes, color = likes)) + 
  geom_line(size = 2, color = 'green') +
  coord_polar() +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = 50), legend.position = 'bottom') +
  labs(x = 'Movie', title = 'Actor 1 Facebook likes by Movie', y = '')
l3



l4 <- likes %>%
  filter(likes == 'actor_2_facebook_likes' & value > 15000) %>%
  ggplot(aes(x = factor(movie_title), y = value, group = likes, color = likes)) + 
  geom_line(size = 2, color = 'blue') +
  coord_polar() +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = 50), legend.position = 'bottom') +
  labs(x = 'Movie', title = 'Actor 2 Facebook likes by Movie', y = '')
l4

k1 <- dataset %>%
  select(director_name, imdb_score) %>%
  group_by(director_name) %>%
  summarise(director_score = sum(imdb_score)) %>%
  arrange(desc(director_score)) %>%
  head(10) %>%
  ggplot(aes(x = factor(director_name, level = director_name), y = director_score)) +
  geom_bar(stat = 'identity', fill = 'navyblue') +
  coord_polar() + 
  labs(x = 'Director', y = 'Imdb score')

k2 <- dataset %>%
  select(actor_1_name, imdb_score) %>%
  group_by(actor_1_name) %>%
  summarise(actor1S = sum(imdb_score)) %>%
  arrange(desc(actor1S)) %>%
  head(10) %>%
  ggplot(aes(x = factor(actor_1_name, level = actor_1_name), y = actor1S)) +
  geom_bar(stat = 'identity', fill = 'purple') +
  coord_polar() +
  labs(x = 'Actor 1', y = 'Imdb Score')
k2

k3 <- dataset %>%
  select(actor_2_name, imdb_score) %>%
  group_by(actor_2_name) %>%
  summarise(actor2S = sum(imdb_score)) %>%
  arrange(desc(actor2S)) %>%
  head(10) %>%
  ggplot(aes(x = factor(actor_2_name, level = actor_2_name), y = actor2S)) +
  geom_bar(stat = 'identity', fill = 'green') +
  coord_polar() +
  labs(x = 'Actor 1', y = 'Imdb Score')
k3

k4 <- dataset %>%
  select(actor_3_name, imdb_score) %>%
  group_by(actor_3_name) %>%
  summarise(actor3S = sum(imdb_score)) %>%
  arrange(desc(actor3S)) %>%
  head(10) %>%
  ggplot(aes(x = factor(actor_3_name, level = actor_3_name), y = actor3S)) +
  geom_bar(stat = 'identity', fill = 'gold') +
  coord_polar() +
  labs(x = 'Actor 3', y = 'Imdb Score')
k4

dataset %>%
  filter(title_year >= 2000 & country == 'Japan') %>%
  select(movie_title, director_name, title_year, budget) %>%
  group_by(movie_title, director_name, title_year) %>%
  ggplot(aes(x = director_name, y = budget, group = title_year, color = title_year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label_repel(aes(label = movie_title), size = 2)
  

dataset %>%
  filter(title_year >= 2000 & country == 'Germany') %>%
  select(movie_title, director_name, title_year, budget) %>%
  group_by(movie_title, director_name, title_year) %>%
  ggplot(aes(x = director_name, y = budget, group = movie_title, color = title_year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label_repel(aes(label = movie_title), size = 2)

dataset %>%
  filter(title_year >= 2000 & country == 'India') %>%
  select(movie_title, director_name, title_year, budget) %>%
  group_by(movie_title, director_name, title_year) %>%
  ggplot(aes(x = director_name, y = budget, group = movie_title, color = title_year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label_repel(aes(label = movie_title), size = 2)

dataset %>%
  filter(title_year >= 2000 & country == 'France') %>%
  select(movie_title, director_name, title_year, budget) %>%
  group_by(movie_title, director_name, title_year) %>%
  ggplot(aes(x = director_name, y = budget, group = movie_title, color = title_year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label_repel(aes(label = movie_title), size = 2)

dataset %>% 
  filter(title_year >= 2000 & country == 'USA' & budget > 200000000) %>%
  select(movie_title, director_name, title_year, budget) %>%
  group_by(movie_title, director_name, title_year) %>%
  ggplot(aes(x = director_name, y = budget, group = movie_title, color = title_year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label_repel(aes(label = movie_title), size = 2) +
  labs(x = 'Director Name', y = "Budget", title = 'Highest Budget USA Movies by Director')

dataset %>% 
  filter(title_year >= 2000 & country == 'USA' & budget < 200000000) %>%
  select(movie_title, director_name, title_year, budget) %>%
  group_by(movie_title, director_name, title_year) %>%
  ggplot(aes(x = director_name, y = budget, group = movie_title, color = title_year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_label_repel(aes(label = movie_title), size = 2) +
  labs(x = 'Director Name', y = "Budget", title = 'Budget USA Movies by Director')

profit <- dataset %>%
  select(title_year, budget, gross, movie_title, director_name, imdb_score) %>%
  mutate(profit = gross - budget, percent = (profit/budget)*100)

profit %>%
  filter(director_name == 'Steven Spielberg') %>%
  ggplot(aes(x = title_year, y = profit, group = 1)) +
  geom_line(color = 'green') +
  geom_text_repel(aes(label = movie_title)) +
  geom_point() +
  geom_line(aes(y = budget))
  
profit %>%
  filter(director_name == 'Joss Whedon') %>%
  ggplot(aes(x = title_year, group = 1,y = profit)) +
  geom_line(col = 'purple') +
  geom_text_repel(aes(label = movie_title)) +
  geom_point() +
  geom_line(aes(y = budget))

profit %>%
  filter(director_name == 'Peter Jackson') %>%
  ggplot(aes(x = title_year, group = 1,y = profit)) +
  geom_line(col = 'purple') +
  geom_text_repel(aes(label = movie_title)) +
  geom_point() +
  geom_line(aes(y = budget))
profit %>% 
  filter(director_name=="James Cameron")%>% 
  ggplot(aes(x=title_year,y=profit,group=1))+
  geom_line(col="navyblue")+
  geom_text_repel(aes(label=movie_title))+
  geom_point()+
  geom_line(aes(y=budget))

profit %>%
  filter(director_name == 'Ridley Scott') %>%
  ggplot(aes(x = title_year, group = 1,y = profit)) +
  geom_line(col = 'purple') +
  geom_text_repel(aes(label = movie_title)) +
  geom_point() +
  geom_line(aes(y = budget))

profit %>% 
  filter(director_name=="Sam Raimi")%>% 
  ggplot(aes(x=title_year,y=profit,group=1))+
  geom_line(col="navyblue")+
  geom_text_repel(aes(label=movie_title))+
  geom_point()+
  geom_line(aes(y=budget))

profit %>% 
  filter(director_name=="Tim Burton")%>% 
  ggplot(aes(x=title_year,y=profit,group=1))+
  geom_line(col="navyblue")+
  geom_text_repel(aes(label=movie_title))+
  geom_point()+
  geom_line(aes(y=budget))

profit %>%
  filter(percent > 90 & percent < 100) %>%
  arrange(desc(percent)) %>%
  ggplot(aes(x = factor(movie_title, level = movie_title), y = percent, group = imdb_score, color = imdb_score)) +
  geom_point(size = 2) +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom') +
  labs(x = 'movie title', title = 'Highest Percent Movie Returns')

net <- dataset %>%
  select(title_year, budget, gross, movie_title, director_name, imdb_score, num_voted_users, num_critic_for_reviews, num_user_for_reviews, movie_facebook_likes, director_facebook_likes, cast_total_facebook_likes, duration, facenumber_in_poster) %>%
  mutate(profit = (gross - budget), percent = round((profit/budget)*100))

net %>%
  select(num_voted_users, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0 & num_voted_users > 0) %>%
  ggplot(aes(x = num_voted_users, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0, 200), low = 'white', high = 'red') +
  labs(col = 'Budget Recovered')

net %>%
  select(num_critic_for_reviews, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0 & num_critic_for_reviews > 0) %>%
  ggplot(aes(x = num_critic_for_reviews, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0,200), low = 'blue', high = 'purple') +
  labs(col = 'Budget Recovered')

net %>%
  select(num_user_for_reviews, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0 & num_user_for_reviews > 0) %>%
  ggplot(aes(x = num_user_for_reviews, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0,200), low = 'green', high = 'blue') +
  labs(col = 'Budget Recovered')

net %>%
  select(movie_facebook_likes, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0 & movie_facebook_likes> 0) %>%
  ggplot(aes(x = movie_facebook_likes, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0,200), low = 'white', high = 'purple') +
  labs(col = 'Budget Recovered')


net %>%
  select(director_facebook_likes, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0 & director_facebook_likes > 0) %>%
  ggplot(aes(x = director_facebook_likes, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0,200), low = 'blue', high = 'purple') +
  labs(col = 'Budget Recovered')

net %>%
  select(cast_total_facebook_likes, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0 & cast_total_facebook_likes > 0) %>%
  ggplot(aes(x = cast_total_facebook_likes, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0,200), low = 'green', high = 'blue') +
  labs(col = 'Budget Recovered')

net %>%
  select(duration, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0 & duration > 0) %>%
  ggplot(aes(x = duration, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0,200), low = 'white', high = 'purple') +
  labs(col = 'Budget Recovered')

net %>%
  select(facenumber_in_poster, imdb_score, percent) %>%
  filter(percent > 0 & percent < 200 & imdb_score > 0) %>%
  ggplot(aes(x = facenumber_in_poster, y = imdb_score, color = percent)) +
  geom_point() +
  scale_color_gradient(limits = c(0,200), low = 'red', high = 'purple') +
  labs(col = 'Budget Recovered')
