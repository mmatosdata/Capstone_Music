
#Libraries
library(tidyverse)
library(stringr)
library(spotifyr)
library(purrr)
library(skimr)
library(caret)
library(DataExplorer)
library(corrplot)
library(factoextra)
library(Boruta)
library(rpart) 
library(rpart.plot) 
library(packcircles)
library(viridis)
library(caretEnsemble)

####################################################
#####DATA LOADING####
###################################################
#loading the data
Hot_100_Audio_Features <- read.csv("~//Hot 100 Audio Features.csv")
genres_v2 <- read.csv("~/genres_v2.csv")


##view the columns available
names(Hot_100_Audio_Features)
names(genres_v2)

##checking which columns are not in both datasets

setdiff(colnames(Hot_100_Audio_Features), colnames(genres_v2))

setdiff( colnames(genres_v2),colnames(Hot_100_Audio_Features))

##checking unique records per dataset

length(unique(Hot_100_Audio_Features$spotify_track_id))
length(unique(genres_v2$id))

##checking column classes
a<- lapply(Hot_100_Audio_Features, class)
b<-lapply(genres_v2, class)
str(Hot_100_Audio_Features)

#exploring NA's
Hot_100_Audio_Features %>% filter(is.na(SongID)== TRUE) %>% count()
Hot_100_Audio_Features %>% filter(is.na(Song)== TRUE) %>% count()
Hot_100_Audio_Features %>% filter(is.na(spotify_track_id)== TRUE) %>% count()
Hot_100_Audio_Features[(Hot_100_Audio_Features$Song==""),] %>% count()
Hot_100_Audio_Features[(Hot_100_Audio_Features$spotify_genre==""),]  %>% count()
Hot_100_Audio_Features[(is.na(Hot_100_Audio_Features$spotify_genre)== TRUE),]  %>% count()
Hot_100_Audio_Features[(Hot_100_Audio_Features$spotify_genre == "[]"),]  %>% count()


genres_v2 %>% filter(is.na(id ==TRUE)) %>% count()
genres_v2 %>% filter(is.na(song_name ==TRUE)) %>% count()
genres_v2[(genres_v2$song_name==""),]  %>% count()
genres_v2[(genres_v2$genre== "[]"),]  %>% count()

###################################################
#####DATA WRANGLING####
###################################################

#removing rows in genres_v2 misisng song name
genres_v2 <- genres_v2[!(genres_v2$song_name==""),] 


#removing rows in Hot_100_Audio_Features  missing genre
Hot_100_Audio_Features <- Hot_100_Audio_Features[!(Hot_100_Audio_Features$spotify_genre==""),] 
Hot_100_Audio_Features <- Hot_100_Audio_Features[(Hot_100_Audio_Features$spotify_genre != "[]"),] 


#renaming columns
genres_v2 <- genres_v2 %>%   rename(., track_id = id )
Hot_100_Audio_Features <- Hot_100_Audio_Features %>%   
  rename(., track_id = spotify_track_id, song_name = Song, genre = spotify_genre,
         duration_ms = spotify_track_duration_ms)

#defining class field

genres_v2$Is_hit <- 0 #means No hit
Hot_100_Audio_Features$Is_hit <- 1  # means Yes, it's a hit


#columns to drop 
#Hot_100_Audio_Features:
exclude_Hot <- c("spotify_track_album", "SongID", "spotify_track_preview_url",
                 "spotify_track_explicit","spotify_track_popularity")

#genres_v2
exclude_genr <- c("type","uri","track_href", "analysis_url", "Unnamed..0" , "title")

#defining columns wanted in specific order

valid_fields <- c("track_id", "song_name", "Is_hit", "genre", "key", "mode"  , "tempo", 
                  "danceability", "energy", "loudness", "speechiness", "acousticness",
                  "instrumentalness", "liveness", "valence","duration_ms",  "time_signature", "Performer")              


#identifying the songs from genres_v2 that are not in Hot_100 (i.e. true no hits)

genres_v2_no_hits <- subset(genres_v2 ,!(track_id%in%Hot_100_Audio_Features$track_id))

####OBTAINING SAMPLE OF PERFORMERS NAMES OF NON-HIT SONGS FOR CONTEXT###

#creating authentication
Sys.setenv(SPOTIFY_CLIENT_ID = Client_Id)  # client id not shared here for security
Sys.setenv(SPOTIFY_CLIENT_SECRET = Client_secret) #client secret not shared here for security

#defining a function to obtain artists names
spotify_filter_artist_id_func <- function(x) {
  c<- get_track(x, market = NULL, authorization = get_spotify_access_token())
  d<- c$artists$id
  e<- get_artists(d, authorization = get_spotify_access_token())
  f<- e$name
  g<-data.frame(song_id= x, artist_id = d, artist_name = f)
  return(g)
}  

#getting samples (limited to 50 songs per run)

spotify_filter_artist_id <- lapply( sample(genres_v2_no_hits$track_id,50), spotify_filter_artist_id_func)

spotify_filter_artist_id <-  bind_rows(spotify_filter_artist_id, .id = "column_label")

spotify_filter_artist_id2 <- lapply( sample(genres_v2_no_hits$track_id,50), spotify_filter_artist_id_func)

spotify_filter_artist_id2 <-  bind_rows(spotify_filter_artist_id, .id = "column_label")


spotify_filter_artist_id3 <- lapply( sample(genres_v2_no_hits$track_id,50), spotify_filter_artist_id_func)

spotify_filter_artist_id3 <-  bind_rows(spotify_filter_artist_id, .id = "column_label")

#merging samples
no_hits_performers_sample <- rbind(spotify_filter_artist_id, spotify_filter_artist_id2, spotify_filter_artist_id3)

###################################################################

#creating "Performers" column in genres_v2_no_hits

genres_v2_no_hits <- merge(genres_v2_no_hits, no_hits_performers_sample[ , c("song_id", "artist_name")], by.x = "track_id", by.y =  "song_id", all.x = TRUE)

genres_v2_no_hits <- genres_v2_no_hits %>% rename(., Performer = artist_name)


#keeping only unique tracks

genres_v2_no_hits<- genres_v2_no_hits %>% distinct(track_id, .keep_all = TRUE)

Hot_100_Audio_Features_uniques <- Hot_100_Audio_Features %>% distinct(track_id, .keep_all = TRUE)

#checking datasets completeness

genres_v2_no_hits[(genres_v2_no_hits$track_id==""),] %>% count()
genres_v2_no_hits %>% filter(is.na(genre)== TRUE) %>% count()
genres_v2_no_hits  %>% filter(genre == "") %>% count()

Hot_100_Audio_Features_uniques[(Hot_100_Audio_Features_uniques$track_id==""),] %>% count()

Hot_100_Audio_Features_uniques %>% filter(is.na(track_id)== TRUE) %>% count()

Hot_100_Audio_Features_uniques %>% filter(is.na(danceability)== TRUE) %>% count()

Hot_100_Audio_Features_uniques %>% filter(is.na(genre)== TRUE) %>% count()

Hot_100_Audio_Features_uniques  %>% filter(genre == "") %>% count()

Hot_100_Audio_Features_uniques  %>% filter(genre == "[]") %>% count()

#excluding rows with no features from hot_100 data

Hot_100_Audio_Features_uniques<- Hot_100_Audio_Features_uniques %>% filter(!is.na(danceability)== TRUE) 


#COMBINING DATASETS and keeping balanced classes (at 16656 each)

#combining datasets
Dataset <- rbind(genres_v2_no_hits %>% select(all_of(valid_fields)),Hot_100_Audio_Features_uniques[1:16656,] %>% select(all_of(valid_fields)))



#cleansing genre variable in Dataset to eliminate special characters and also only take the first genre associated to the song 

Dataset$genre_clean  <- Dataset$genre  %>%   ifelse(str_detect(., ",", negate = FALSE),  str_extract(.,"^(.+?),") %>% str_replace_all(., "[:punct:]", " ") %>% trimws(.),.) %>%
  str_replace_all(., "[:punct:]", " ") %>% trimws(.)


#dataset profiling 
summary(Dataset)


Dataset_skim_summary <- Dataset  %>% skim()


#checking completeness of final dataset
Dataset[(Dataset$key==""),] %>% count()

Dataset %>% filter(is.na(track_id)== TRUE) %>% count()

Dataset %>% filter(genre == "[]") %>% count()

Dataset %>% filter(genre == "") %>% count()

#ensuring classes remain balanced
Dataset %>% group_by(Is_hit) %>% summarise(n = n())

#counting songs per genre
dataset_genre <- Dataset %>% select(genre,genre_clean) %>%   group_by(genre_clean, genre) %>% summarise(n = n())

#deleting tables not longer needed to liberate memory:
rm(list=c("genres_v2", "genres_v2_no_hits", "Hot_100_Audio_Features","Hot_100_Audio_Features_uniques",
          "spotify_filter_artist_id", "spotify_filter_artist_id2", "spotify_filter_artist_id3",
          "a", "b", "valid_fields","exclude_genr", "exclude_Hot", "Dataset_skim_summary"))

###################################################
#####DATA SPLITTING FOR MODELLING####
###################################################

# Split-out validation dataset
set.seed(75, sample.kind="Rounding")

# create a list of 80% of the rows in the cleansed Dataset  for training
validationIndex <- createDataPartition(y = Dataset$Is_hit , times = 1, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- Dataset[-validationIndex,]
# use the remaining 80% of data to training the models
training <- Dataset[validationIndex,]


###################################################
#####DATA EXPLORATION####
###################################################

# Descriptive statistics

#checking names of performers for hit & non-hit songs in consolidated Dataset (before partition)
#hit songs
Dataset %>% filter(Is_hit== "1") %>% group_by(Performer,Is_hit) %>% 
  summarise(number_songs = n()) %>% arrange(desc(number_songs)) %>% head(10)
#non-hit songs
Dataset %>% filter(Is_hit== "0" & !is.na(Performer)) %>% group_by(Performer,Is_hit) %>% 
  summarise(number_songs = n()) %>% arrange(desc(number_songs)) %>% head(10)

#checking Michael Jackson songs
Dataset %>% filter(Performer == "Michael Jackson") %>% group_by(Performer,Is_hit) %>% 
  summarise(number_songs = n())

# dimensions of dataset
dim(training)

#exploring training data
training %>% introduce() 

training %>% plot_intro()

#checking for missing values
training %>% profile_missing()  # dataframe of the missing values % per variable

## variables types
sapply(training, class) %>% as.data.frame(.)

## reviewing the first 3 rows
head(training, n=3)

# summary
summary(training)

#keys
unique(training$key)

#class distribution check
cbind(freq=table(training$Is_hit), percentage=prop.table(table(training$Is_hit))*100)

#exploring validation dataset

#variables
str(validation)
#keys
unique(validation$key)

# class distribution check
cbind(freq=table(validation$Is_hit), percentage=prop.table(table(validation$Is_hit))*100)

###################################################
#####DATA CLEANSING####
###################################################

##Cleansing of variables

#transforming "IS_hit" to factor class
training$Is_hit <- as.factor(training$Is_hit)
validation$Is_hit <- as.factor(validation$Is_hit)

#modifying labels for ease of understanding

#on training dataset
levels(training$Is_hit) <- c("No","Yes")
head(training$Is_hit)

#on validation dataset
levels(validation$Is_hit) <- c("No","Yes")
head(validation$Is_hit)

#converting genres clean column to factors
training$genre_clean_factors <- as.factor(training$genre_clean)

#replicating it in validation dataset
validation$genre_clean_factors <- as.factor(validation$genre_clean)

# Creating index of predictors
features_index <- c(5:17,20)
features_index_num <- c(5:17)


###################################################
#####DATA VISUALIZATION####
###################################################

#checking correlations  
corr_training <-cor(training[features_index_num])

corrplot(corr_training, method="number",type= "full",insig = "blank", number.cex = 0.6)


# histograms each numeric predictor (separated in groups to ease visualization)
par(mfrow=c(2,3))
for(i in 5:10) {
  hist(training[,i], main=names(training)[i])
}

par(mfrow=c(2,2))
for(i in 11:14) {
  hist(training[,i], main=names(training)[i])
}

par(mfrow=c(1,3))
for(i in 15:17) {
  hist(training[,i], main=names(training)[i])
}

#### Considering distributions per classes #####

par(mfrow=c(2,3))
for(i in 5:10) {
  boxplot(training[,i] ~ training[,3] , main=names(training)[i], las =1, col= c(2,4))
  
}

par(mfrow=c(2,2))
for(i in 11:14) {
  boxplot(training[,i] ~ training[,3] , main=names(training)[i], las =1, col= c(2,4))
  
}


par(mfrow=c(1,3))
for(i in 15:17) {
  boxplot(training[,i] ~ training[,3] , main=names(training)[i], las =1, col= c(2,4))
  
  }

par(mfrow=c(1,1))
boxplot(training[,20] ~ training[,3] , main=names(training)[20], las =1, col= c(2,4))


####Research questions####

###Analysis part I:  features describing songs types###
 
  #what are the most common keys by class?
  
  by_key_count <-training %>% select(key, Is_hit) %>% group_by( key, Is_hit) %>% summarise(total_songs = n())
  
  
  by_key_count %>% ggplot(., aes(x=factor(key, labels =c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")), 
                                 y = total_songs, fill = Is_hit )) + 
    geom_bar(stat = "Identity" , position = "dodge") +
    scale_fill_brewer(palette = "Set1")  +
    theme(legend.position="none")
  
  
  
  #what are the most common genres by class?  
  
  by_genre_count <-training %>% select(genre_clean_factors, Is_hit) %>% group_by( genre_clean_factors, Is_hit) %>% summarise(total_songs = n())
  
  summary(by_genre_count$total_songs)
  
  #top ten genres for hit songs (by count of songs)
  by_genre_count %>% filter(Is_hit == "1") %>%
    arrange(desc(total_songs)) %>%
    head(10) 
  
  #top ten genres for non-hit songs (by count of songs)
  by_genre_count %>% filter(Is_hit == "0") %>%
    arrange(desc(total_songs)) %>%
    head(10) 
  
 
#checking proportions of hit non-hit by genre
prop_bygenre <- prop.table(table(training[,c("genre_clean_factors", "Is_hit")]), margin = 2 ) *100 

# distribution of  songs per genre
by_genre_count %>% group_by(total_songs) %>% 
  summarise(freq= n())%>% ggplot(., aes(x=as.factor(total_songs), y = freq )) + 
  geom_bar(stat = "Identity" ) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


#considering genres, key and mode as predictors

by_genre_key_mode <-training %>% select(genre_clean_factors, key, mode, Is_hit) %>% group_by( genre_clean_factors, key, mode, Is_hit) %>% summarise(total_songs = n())

#checking distribution of songs by genre and class
by_genre_key_mode %>% 
ggplot(., aes(x=total_songs, group=Is_hit, fill=Is_hit)) +
  geom_density(adjust=1.5, alpha=.4) +
    scale_x_continuous(trans='log2')


#considering key and mode relationship
prop.table(table(training[,c("key","mode")]) ) *100

by_genre_key_mode %>% 
ggplot(., aes(x = factor(key, labels =c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")), y = total_songs))+
  geom_bar(
    aes(fill = as.factor(mode) ), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+
  facet_wrap(~ Is_hit) + 
  scale_fill_brewer(palette = "Set1") 

#considering mode by itself as a possible predictor
prop.table(table(training[,c("Is_hit","mode")]), margin = 2 ) *100 

chisq.test(training$Is_hit, training$mode)  # independent variables

#mode density by class
training  %>% 
ggplot(., aes(x=mode, group=Is_hit, fill=Is_hit)) +
   geom_density(adjust=1.5, alpha=.4) 

#considering mode in relation to genre and key  
by_genre_key_mode  %>% group_by(mode) %>% 
  ggplot(., aes(x=mode, group=Is_hit, fill=Is_hit)) +
  geom_density(adjust=1.5, alpha=.4) 

#considering distribution of instrumentalness
#boxplot by class
training %>%
  ggplot(., aes(x=Is_hit, y=instrumentalness)) + 
  geom_boxplot( fill= c(2,4)) + 
  xlab("Is_hit")+
  scale_y_log10()

# histogram by class  
training %>%
  ggplot( aes(x=instrumentalness, fill=Is_hit)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")+
  scale_x_log10()

#considering the distribution of acousticness
#histogram by class
training %>%
  ggplot( aes(x=acousticness, fill=Is_hit)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

#boxplot by class
training %>%
  ggplot(., aes(x=Is_hit, y=acousticness)) + 
  geom_boxplot( fill= c(2,4)) + 
  xlab("Is_hit")

#considering the distribution of speechiness
#histogram by class
training %>%
  ggplot( aes(x=speechiness, fill=Is_hit)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="") +
  scale_x_log10()

#boxplot by class
training %>%
  ggplot(., aes(x=Is_hit, y=speechiness)) + 
  geom_boxplot( fill= c(2,4)) + 
  xlab("Is_hit")

#considering the relationship between instrumentalness and acousticness
training %>%
  ggplot(., aes(x=instrumentalness, y=acousticness, color=Is_hit)) +
  geom_point(alpha = 0.4) +
  theme(legend.position="none")

#considering the relationship between instrumentalness and speechiness
training %>%
  ggplot(., aes(x=instrumentalness, y=speechiness, color=Is_hit)) +
  geom_point(alpha = 0.4) +
  theme(legend.position="none")

###Analysis part II:  features describing elements of songs speed ###

#considering time_signature distribution

#time_signature distribution by classes

summary(training$time_signature)

by_time_sign_count <-training %>% select(time_signature, Is_hit) %>% group_by( time_signature, Is_hit) %>% summarise(total_songs = n())

#class by time_signature categories
by_time_sign_count %>%
  ggplot(., aes(x=factor(time_signature, labels =c("missing", "1/4","3/4","4/4","5/4")), 
                y = total_songs, fill = Is_hit )) + 
  geom_bar(stat = "Identity" , position = "dodge") +
  scale_fill_brewer(palette = "Set1")  +
  theme(legend.position="none")


#considering the distribution of liveness
#histogram by class
training %>%
  ggplot( aes(x=liveness, fill=Is_hit)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

#boxplot by class
training %>%
  ggplot(., aes(x=Is_hit, y=liveness)) + 
  geom_boxplot( fill= c(2,4)) + 
  xlab("Is_hit")

# considering tempo interquartile by classes

training %>% filter(Is_hit== "No") %>% select(tempo) %>% summary(.)

training %>% filter(Is_hit== "Yes") %>% select(tempo) %>% summary(.)

#tempo distribution by classes
training %>%
  ggplot( aes(x=tempo, fill=Is_hit)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

#valence distribution by classes
training %>%
  ggplot( aes(x=valence, fill=Is_hit)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

#considering the distribution of duration
#histogram by class
training %>%
  ggplot( aes(x=duration_ms, fill=Is_hit)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="") +
  scale_x_log10()

#boxplot by class
training %>%
  ggplot(., aes(x=Is_hit, y=duration_ms)) + 
  geom_boxplot( fill= c(2,4)) + 
  xlab("Is_hit")

#considering tempo and valence relationship
training %>%
  ggplot(., aes(x=valence, y=tempo, color=Is_hit)) +
  geom_point() +
  theme(legend.position="none")

#considering tempo and danceability relationship
training %>%
  ggplot(., aes(x=tempo, y=danceability, color=Is_hit)) +
  geom_point() +
  theme(legend.position="none")

#considering tempo and energy relationship
training %>%
  ggplot(., aes(x=tempo, y=energy, color=Is_hit)) +
  geom_point() +
  theme(legend.position="none")

#considering the relationship between energy and loudness
training %>%
  ggplot(., aes(x=energy, y=loudness, color=Is_hit)) +
  geom_point() +
  theme(legend.position="none")

#considering the relationship between tempo and duration
training %>%
  ggplot(., aes(x=duration_ms, y=tempo, color=Is_hit)) +
  geom_point(alpha = 0.2) +
  theme(legend.position="none")+
  scale_x_log10()

###################################################
#####FEATURES SELECTION####
###################################################

# a) Feature Selection

#creating indexes to evaluate features
colnames(training)
features_index_plusclass <- c(3, 5:17,20)

#define test data to check runtime before running the full feature selection model 
set.seed(75)
test_data <- sample_n(training[,features_index_plusclass],500)

#considering automated feature selection 
# test train the model for first model group
set.seed(75)
startTime <- Sys.time()
boruta_music <- Boruta(Is_hit ~ ., data=na.omit(test_data), doTrace=0)  
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/500)*26650)

#if execution time from test is acceptable run the  model over full data
# obtaining significant variables

set.seed(75)
startTime <- Sys.time()
boruta_music <- Boruta(Is_hit ~ ., data=na.omit(training[features_index_plusclass]), doTrace=0)  
endTime <- Sys.time()
print(endTime - startTime)

# Getting significant variables including tentative
boruta_signif_vars <- getSelectedAttributes(boruta_music, withTentative = TRUE)
roughFixMod_music <- TentativeRoughFix(boruta_music)
boruta_signif_vars <- getSelectedAttributes(roughFixMod_music)
print(boruta_signif_vars)

colnames(training[features_index_plusclass])

# Variable Importance Scores
imps <- attStats(roughFixMod_music)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_music, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

###################################################
#####DATA PRE-PROCESSING####
###################################################

#Pre-processing: encoding genre factors into a numeric with range 0 to 1

#converting gender range from 0 to 1  the distribution

preProcess_range_model <- preProcess(data.frame(new_negre =as.numeric(training[,c("genre_clean_factors")])), method='range')
new_genre_encoded <- predict(preProcess_range_model, newdata = data.frame(new_negre= as.numeric(training[,c("genre_clean_factors")])))

#appending, checking class and visualising distributions to ensure it remains unaltered 

training$new_genre <- new_genre_encoded$new_negre
class(training$new_genre)

par(mfrow=c(1,2))
for(i in 20:21) {
  boxplot(training[,i] ~ training[,3] , main=names(training)[i], las =1, col= c(2,4))
  
}

##performing same transformation on validation dataset

#converting gender range from 0 to 1  the distribution

preProcess_range_model <- preProcess(data.frame(new_negre =as.numeric(validation[,c("genre_clean_factors")])), method='range')
new_genre_encoded <- predict(preProcess_range_model, newdata = data.frame(new_negre= as.numeric(validation[,c("genre_clean_factors")])))

#appending, checking class and visualising distributions

validation$new_genre <- new_genre_encoded$new_negre
class(validation$new_genre)

par(mfrow=c(1,2))
for(i in 20:21) {
  boxplot(validation[,i] ~ validation[,3] , main=names(validation)[i], las =1, col= c(2,4))
  
}

#removing objects no longer needed
rm(preProcess_range_model, new_genre_encoded )

##Transforming integers to numeric to avoid issues with caret processing
#checking classes
## variables types
sapply(training, class) %>% as.data.frame(.)

#ensuring that transformation does not affect output 
data.frame( trans = unique(as.numeric(training$key)), norm = unique(training$key))
data.frame( trans = unique(as.numeric(training$mode)), norm = unique(training$mode))
data.frame( trans = head(as.numeric(training$duration_ms)), norm = head(training$duration_ms))

#transforming integer variables to numeric in training dataset

training$key <- as.numeric(training$key)
training$mode <- as.numeric(training$mode)
training$duration_ms <-as.numeric(training$duration_ms)
                            
#transforming integer variables to numeric in validation dataset

validation$key <- as.numeric(validation$key)
validation$mode <- as.numeric(validation$mode)
validation$duration_ms <-as.numeric(validation$duration_ms)

#creating indexes per model groups
colnames(training)
#From Exploratory Data Analysis

#features of songs characteristics (song types)
type_group_index<- c(3, 21,5,6,11,12,13)
sapply(training[,type_group_index], class) %>% as.data.frame(.)

#features of songs speed
speed_group_index <- c(3,7,8, 9,15,16)
sapply(training[,speed_group_index], class) %>% as.data.frame(.)

#from Automated features selection (6 top features)
AFS_group_index <- c(3,21,7, 11, 15,16,10)
sapply(training[,AFS_group_index], class) %>% as.data.frame(.)


#define test data to check runtime before running the full feature selection model 
set.seed(75) 
n_test <- 3000
test_data2 <- sample_n(training[,type_group_index],n_test)

test_data3 <- sample_n(training[,speed_group_index],n_test)

test_data4 <- sample_n(training[,AFS_group_index],n_test)

###################################################
#####SPOT_CHECKING POTENTIAL ALGORITHMS####
###################################################

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = T, classProbs=T)
metric <- "Accuracy"

# Test options and evaluation metric over small subset of data

# considering algorithms for models based on songs characteristics
# LG  
set.seed(75)
startTime <- Sys.time()
typegroup.fit.glm <- train(Is_hit ~ ., data = test_data2,  method="glm", metric=metric, trControl=trainControl,  family=binomial(link="logit"), na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# KNN
set.seed(75)
startTime <- Sys.time()
typegroup.fit.knn <- train(Is_hit ~ ., data=test_data2, method="knn", metric=metric, trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# CART
set.seed(75)
startTime <- Sys.time()
typegroup.fit.cart <- train(Is_hit ~ ., data=test_data2, method="rpart", metric=metric,
                  trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# Naive Bayes
set.seed(75)
startTime <- Sys.time()
typegroup.fit.nb <- train(Is_hit ~ ., data=test_data2, method="nb", metric=metric, trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# SVM
set.seed(75)
startTime <- Sys.time()
typegroup.fit.svm <- train(Is_hit ~ ., data=test_data2, method="svmRadial", metric=metric,
                 trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# RF
set.seed(75)
startTime <- Sys.time()
typegroup.fit.RF <- train(Is_hit ~ ., data=test_data2, method="rf", ntree=5, metric=metric,
                trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# considering algorithms for models based on songs speed
# LG
set.seed(75)
startTime <- Sys.time()
speedgroup.fit.glm <- train(Is_hit ~ ., data = test_data3,  method="glm", metric=metric, trControl=trainControl,  preProcess = 'center',  family=binomial(link="logit"), na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# KNN
set.seed(75)
startTime <- Sys.time()
speedgroup.fit.knn <- train(Is_hit ~ ., data=test_data3, method="knn", metric=metric, trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# CART
set.seed(75)
startTime <- Sys.time()
speedgroup.fit.cart <- train(Is_hit ~ ., data=test_data3, method="rpart", metric=metric,
                             trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# Naive Bayes
set.seed(75)
startTime <- Sys.time()
speedgroup.fit.nb <- train(Is_hit ~ ., data=test_data3, method="nb", metric=metric, trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# SVM
set.seed(75)
startTime <- Sys.time()
speedgroup.fit.svm <- train(Is_hit ~ ., data=test_data3, method="svmRadial", metric=metric,
                            trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# RF
set.seed(75)
startTime <- Sys.time()
speedgroup.fit.RF <- train(Is_hit ~ ., data=test_data3, method="rf", ntree=5, metric=metric,
                           trControl=trainControl, na.action=na.exclude)

endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# considering algorithms for models based on automated features selection

# LG  (with preprocess set to center to normalise the predictors as its beneficial for Logistic regression)
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.glm <- train(Is_hit ~ ., data = test_data4,  method="glm", metric=metric, trControl=trainControl, preProcess = 'center', family=binomial(link="logit"), na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# KNN
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.knn <- train(Is_hit ~ ., data=test_data4, method="knn", metric=metric, trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# CART
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.cart <- train(Is_hit ~ ., data=test_data4, method="rpart", metric=metric,
                           trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# Naive Bayes
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.nb <- train(Is_hit ~ ., data=test_data4, method="nb", metric=metric, trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# SVM
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.svm <- train(Is_hit ~ ., data=test_data4, method="svmRadial", metric=metric,
                          trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# RF
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.RF <- train(Is_hit ~ ., data=test_data4, method="rf", ntree=5, metric=metric,
                         trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)
print((as.numeric(endTime - startTime)/n_test)*26650)

# Compare algorithms
results <- resamples(list(LG_type=typegroup.fit.glm, LG_speed=speedgroup.fit.glm, LG_AFS=AFSgroup.fit.glm,
                          KNN_type=typegroup.fit.knn, KNN_speed=speedgroup.fit.knn, KNN_AFS=AFSgroup.fit.knn,
                          CART_type=typegroup.fit.cart, CART_speed=speedgroup.fit.cart, CART_AFS=AFSgroup.fit.cart,
                          NB_type=typegroup.fit.nb, NB_speed=speedgroup.fit.nb, NB_AFS=AFSgroup.fit.nb, 
                          SVM_type=typegroup.fit.svm, SVM_speed=speedgroup.fit.svm, SVM_AFS=AFSgroup.fit.svm,
                          RF_type=typegroup.fit.RF, RF_speed=speedgroup.fit.RF, RF_AFS=AFSgroup.fit.RF))
summary(results)
dotplot(results)

###################################################
#####EVALUATING SELECTED ALGORITHMS####
###################################################

# Checking performance with full training dataset to select best models

# RF type
set.seed(75)
startTime <- Sys.time()
typegroup.fit.RF2 <- train(Is_hit ~ ., data=training[,type_group_index], method="rf", ntree=5, metric=metric,
                          trControl=trainControl, na.action=na.exclude)
endTime <- Sys.time()
print(endTime - startTime)


# RF AFS
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.RF2 <- train(Is_hit ~ ., data=training[,AFS_group_index], method="rf", ntree=5, metric=metric,
                         trControl=trainControl, na.action=na.exclude, preProcess= "center")
endTime <- Sys.time()
print(endTime - startTime)

# CART Type
set.seed(75)
startTime <- Sys.time()
typegroup.fit.cart2 <- train(Is_hit ~ ., data=training[,type_group_index], method="rpart", metric=metric,
                            trControl=trainControl, na.action=na.exclude, preProcess= "center")
endTime <- Sys.time()
print(endTime - startTime)

#SVM_AFS excluded due to long processing time

# CART AFS
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.cart2 <- train(Is_hit ~ ., data=training[,AFS_group_index], method="rpart", metric=metric,
                             trControl=trainControl, na.action=na.exclude, preProcess= "center")
endTime <- Sys.time()
print(endTime - startTime)

# LG  AFS 
set.seed(75)
startTime <- Sys.time()
AFSgroup.fit.glm2 <- train(Is_hit ~ ., data = training[,AFS_group_index],  method="glm", metric=metric, trControl=trainControl, family=binomial(link="logit"), na.action=na.exclude,  preProcess= "center")
endTime <- Sys.time()
print(endTime - startTime)

# Compare first selection of algorithms
results_fisrt_select <- resamples(list(RF_type=typegroup.fit.RF,RF_AFS=AFSgroup.fit.RF,
                                       CART_type=typegroup.fit.cart, CART_AFS=AFSgroup.fit.cart,
                                       LG_AFS=AFSgroup.fit.glm, RF_type2=typegroup.fit.RF2,RF_AFS2=AFSgroup.fit.RF2,
                                       CART_type2=typegroup.fit.cart2, CART_AFS2=AFSgroup.fit.cart2,
                                       LG_AFS2=AFSgroup.fit.glm2))
summary(results_fisrt_select)
dotplot(results_fisrt_select)


###Checking for possible overfitting of the models

#defining a function to check performance with validation dataset
validation_function <- function(x,y) {
  predict_test <-predict(x, newdata=y)
  a <- confusionMatrix(predict_test,y$Is_hit)
  b<- data.frame(row.names = "results", acc = a$overall['Accuracy'],
                 pval= a$overall['AccuracyPValue'],
                 sen= a$byClass['Sensitivity'],
                 spe = a$byClass['Specificity'],
                 f1= a$byClass['F1'])
  return(b)
}

#summarising results

selected_models <- list(RF_type2=typegroup.fit.RF2,RF_AFS2=AFSgroup.fit.RF2,
                        CART_type2=typegroup.fit.cart2, CART_AFS2=AFSgroup.fit.cart2,
                        LG_AFS2=AFSgroup.fit.glm2)

sapply(selected_models, validation_function, y = validation)

#selected models: CART_AFS2 & LG_AFS2


###################################################
#####INTEPRETATION OF INDIVIDUAL FINAL MODELS SELECTED####
###################################################

#Interpretation of individual models

#AFS.CART
rpart.plot(AFSgroup.fit.cart2$finalModel)

#AFS.GLM
summary(AFSgroup.fit.glm2)


#probability curve for loudness example
training[,AFS_group_index]  %>%
  mutate(prob = ifelse(Is_hit == "Yes", 1, 0)) %>%
  ggplot(aes(loudness, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "loudness",
    y = "Probability of being hit song"
  )

#probability curve for duration example
training[,AFS_group_index]  %>%
  mutate(prob = ifelse(Is_hit == "Yes", 1, 0)) %>%
  ggplot(aes(duration_ms, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "song duration (centered)",
    y = "Probability of being hit song"
  )


###expanding on genres interpretation

### visualization of Top15 genres by class
Top_15_genre_hit <- by_genre_count %>% filter(Is_hit == 1) %>% arrange(desc(total_songs)) %>% head(15)

Top_15_genre_non_hit <- by_genre_count %>% filter(Is_hit == 0) %>% arrange(desc(total_songs)) %>% head(15)


# Generate the layout for Top_15_genre_hit
packing <- circleProgressiveLayout(Top_15_genre_hit$total_songs, sizetype='area')
packing$radius <- 0.95*packing$radius
Top_15_genre_hit_pack <- cbind(Top_15_genre_hit, packing)
Top_15_genre_hit_pack.gg <- circleLayoutVertices(packing, npoints=50)

# Generate the layout for Top_15_genre_non_hit
packing_nh <- circleProgressiveLayout(Top_15_genre_non_hit$total_songs, sizetype='area')
packing_nh$radius <- 0.95*packing$radius
Top_15_genre_non_hit_pack <- cbind(Top_15_genre_non_hit, packing_nh)
Top_15_genre_non_hit_pack.gg <- circleLayoutVertices(packing_nh, npoints=50)

# Plot Top_15_genre_hit
ggplot() + 
  geom_polygon(data = Top_15_genre_hit_pack.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = Top_15_genre_hit_pack, aes(x, y, size=total_songs, label = genre_clean_factors), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()


# Plot Top_15_genre_hit
ggplot() + 
  geom_polygon(data = Top_15_genre_non_hit_pack.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = Top_15_genre_non_hit_pack, aes(x, y, size=total_songs, label = genre_clean_factors), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()


###################################################
#####FINAL MODEL ENSEMBLE AND EVALUATION####
###################################################

#ensemble of two models selected

model_list <- caretList(
  Is_hit~., data=training[,AFS_group_index],
  trControl=trainControl,
  methodList=c("glm", "rpart")
)

#checking  models correlation
xyplot(resamples(model_list))

modelCor(resamples(model_list))

#ensembling
ensemble_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats=3, returnResamp = "final", 
                              savePredictions = "final",  classProbs = TRUE)

final_model <- caretEnsemble(
  model_list, 
  metric="Accuracy",
  trControl= ensemble_ctrl)

summary(final_model)

#importance of predictors
final_model_imp<- varImp(final_model)

final_model_imp[order(final_model_imp$overall,decreasing=TRUE),]

#evaluating models with validation dataset

#final model
validation_function(final_model ,validation)

#individual models
validation_function(AFSgroup.fit.cart2 ,validation)
validation_function(AFSgroup.fit.glm2 ,validation)


#testing final model on unbalanced dataset
(set.seed = 75)
validation_unbalanced_no <- validation %>% filter(Is_hit == "No") %>% sample_n(., 3000)
validation_unbalanced_yes <- validation %>% filter(Is_hit == "Yes") %>% sample_n(., 300)

validation_unbalanced <- rbind(validation_unbalanced_no, validation_unbalanced_yes) %>% arrange(.$track_id)

rm(validation_unbalanced_no, validation_unbalanced_yes)

#testing algorithm in unbalanced population
validation_function(final_model,validation_unbalanced)
