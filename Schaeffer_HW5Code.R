##########################################################
###                      Step 2                       ####
#Completing step 1 for every JSON File
##########################################################
library(stringr)
library(jsonlite)
library(tidyverse)


#Making our empty dataframe
empty.dataframe = data.frame(
  artist = character(),
  album = character(),
  track = character(),
  overall_loudness = numeric(),
  spectral_energy = numeric(),
  dissonance = numeric(),
  pitch_salience = numeric(),
  bpm = numeric(),
  beats_loudness = numeric(),
  danceability = numeric(),
  tuning_frequency = numeric()
)

song.dataframe = empty.dataframe

#Pulling list of all tracks to be iterated
folder.name = 'EssentiaOutput'
all.tracks = list.files(folder.name)

#Making sure only valid json files are being used
json.count = str_count(all.tracks, ".json")
track.indices = which(json.count == 1)


df.index = 1 #Initializing where to input in dataframe
for (i in track.indices) {
  #Running for loop over every track
  
  ##########################################################
  ###                      Step 1                       ####
  # Pulling file and extracting necessary info from JSON
  ##########################################################
  current.filename = all.tracks[i]
  #current.filename = "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"
  current.songname = str_sub(current.filename, end = -6)
  
  #Making a vector containing artist, album, and song
  song.vector = str_split_1(current.songname, "-")

  ###Pulling data from JSON file
  json.pathname = paste(folder.name, current.filename, sep = '/')
  json.track = fromJSON(json.pathname)
  
  
  song.dataframe[df.index, ] <- tibble( #Adding the information directly through a tibble
    artist = song.vector[1],
    album = song.vector[2],
    track = song.vector[3],
    overall_loudness = json.track$lowlevel$average_loudness,
    spectral_energy = json.track$lowlevel$spectral_energy$mean,
    dissonance = json.track$lowlevel$dissonance$mean,
    pitch_salience = json.track$lowlevel$pitch_salience$mean,
    bpm = json.track$rhythm$bpm,
    beats_loudness = json.track$rhythm$beats_loudness$mean,
    danceability = json.track$rhythm$danceability,
    tuning_frequency = json.track$tonal$tuning_frequency
  )
  
  
  df.index=df.index+1 #Moving to next dataframe index
}



##########################################################
###                      Step 3                       ####
# Making new dataframe using essentia models
##########################################################
#Loading our dataframe file
output.model = read.csv("EssentiaOutput/EssentiaModelOutput.csv")

###Writing the matrix utilizing tinyverse
###First focusing on transitioning to using means, then will select only desired columns
output.model = output.model %>%
  #I'm a little confused how rowwise works, but when doing some research this seemed
  #the best way to accomplish my task, is this saying that from anything past that pipe
  #I want to have my functions apply to the entirety of the row?
  rowwise() %>%
  mutate(
    arousal = mean(c(emo_arousal, deam_arousal, muse_arousal)),
    valence = mean(c(emo_valence, deam_valence, muse_valence)),
    aggressive = mean(c(eff_aggressive, nn_aggressive)),
    happy = mean(c(eff_happy, nn_happy)),
    party = mean(c(eff_party, nn_party)),
    relaxed = mean(c(eff_relax, nn_relax)),
    sad = mean(c(eff_sad, nn_sad)),
    acoustic = mean(c(eff_acoustic, nn_acoustic)),
    electric = mean(c(eff_electronic, nn_electronic)),
    instrumental = mean(c(eff_instrumental, nn_instrumental)),
    timbreBright = eff_timbre_bright
  )
###Now that all desired columns are made, we retain only desired
essentia.dataframe = output.model %>%
  select(artist, album, track, arousal, valence, aggressive, happy,
         party, relaxed, sad, acoustic, electric, instrumental, timbreBright)


##########################################################
###                      Step 4                       ####
# Making a final dataframe that includes all information
##########################################################
lyric.output = read.csv("LIWCOutput/LIWCOutput.csv")
#Loading dataframe


###Merging together our .wav data dataframe and our essentia dataframe
final.dataframe = merge(song.dataframe, essentia.dataframe,
                        by = c("artist", "album", "track")
)


###Adding our lyric dataframe
final.dataframe = merge(final.dataframe, lyric.output,
                        by = c("artist", "album", "track")
)

# Rename function column
final.dataframe = final.dataframe %>%
  rename(func = function.)

#Making every column that is numerical actually have numeric class
final.dataframe[, 4:ncol(final.dataframe)] <- lapply(final.dataframe[, 4:ncol(final.dataframe)], as.numeric)


##########################################################
###                      Step 5                       ####
# Separating out Allentown
##########################################################
#Making a file that contains everything except Allentown
write.csv(final.dataframe[final.dataframe$track!="Allentown", ], "trainingdata.csv")

#Making a file that ONLY contains Allentown
write.csv(final.dataframe[final.dataframe$track=="Allentown", ], "testingdata.csv")



View(final.dataframe)

##########################################################
###                      Step 5                       ####
##########################################################
#When trying to research how to use the dataframe we made in R,
#it was seeming to tell me I needed to download/load a library to reshape my
#dataframe into usable boxplot data

library(tidyr)
library(ggplot2)

#I'm a little confused by the tinyverse code here but this is what I found online
#And it does seem to successfully get the data
df_long <- final.dataframe %>% 
  pivot_longer(cols = 4:last_col(), names_to = "Variable", values_to = "Value")

boxplot(Value ~ get(colnames(final.dataframe)[3]), data = df_long)

######################################################################
####                          PLOT TWO
###########################################################################
library(tidyverse)
####################################
# Load Data
####################################
dat <- read_csv("trainingdata.csv")
####################################
# Create Plot
####################################
p <- ggplot(dat %>%
              drop_na(), aes(x = !!sym("track"), y = after_stat(count / sum(count)))) +
  geom_bar(position = "dodge", fill = "lightgrey", width = 0.5) +
  get("theme_bw")() +
  xlab("track") +
  ylab(ifelse("Proportion" == "", "Proportion", "Proportion")) +
  ggtitle("Plot 2", "") +
  geom_hline(yintercept = 0)
####################################
# Print Plot
####################################
p
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  dplyr::select("track") %>%
  group_by(!!sym("track")) %>%
  summarize(Observations = sum(!is.na(!!sym("track")))) %>%
  replace_na(list(Observations = 0))
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("track")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  filter(!(is.na(!!sym("track")))) %>%
  mutate(Proportion = Observations / sum(Observations)) %>%
  mutate(Percent = Proportion * 100) %>%
  mutate_if(is.numeric, round, 4) %>%
  ungroup() %>%
  add_row(`:=`(!!sym("track"), "Missing Data"), Observations = missing.obs)
####################################
# Print Data Summary
####################################
dat.summary

######################################################################
####                          PLOT THREE
####                 Comparing overall loudness of artists
###########################################################################
####################################
# Load Data
####################################
dat <- read_csv("trainingdata.csv")
####################################
# Select data for plot
####################################
df <- dat %>%
  dplyr::select("overall_loudness", "artist") %>%
  filter(!is.na(!!sym("artist"))) %>%
  mutate(denoted.group = paste("artist", " = ", !!sym("artist"), sep = ""))
####################################
# Create Plot
####################################
p <- ggplot(df, aes(x = !!sym("overall_loudness"), after_stat(!!sym("density")))) +
  geom_histogram(breaks = seq(0, 1, length.out = 9), color = "grey30", fill = "lightgrey") +
  get("theme_bw")() +
  xlab("overall loudness") +
  ylab("Density") +
  ggtitle("Comparing Overall Loudness between Artists", "") +
  geom_hline(yintercept = 0) +
  facet_wrap(~denoted.group)
####################################
# Print Plot
####################################
p
####################################
# Summarize Data
####################################
dat.summary <- dat %>%
  select(!!sym("overall_loudness"), !!sym("artist")) %>%
  group_by(!!sym("artist")) %>%
  summarize(Observations = sum(!is.na(!!sym("overall_loudness"))), Mean = mean(!!sym("overall_loudness"), na.rm = T), `Standard Deviation` = sd(!!sym("overall_loudness"), na.rm = T), Min = min(!!sym("overall_loudness"), na.rm = T), Q1 = quantile(!!sym("overall_loudness"), probs = 0.25, na.rm = T), Median = median(!!sym("overall_loudness"), na.rm = T), Q3 = quantile(!!sym("overall_loudness"), probs = 0.75, na.rm = T), Max = max(!!sym("overall_loudness"), na.rm = T), IQR = IQR(!!sym("overall_loudness"), na.rm = T)) %>%
  filter(!is.na(!!sym("artist"))) %>%
  tidyr::complete(!!sym("artist")) %>%
  mutate_if(is.numeric, round, 4)
missing.obs <- dat %>%
  summarize(missing = sum(is.na(!!sym("overall_loudness")) | is.na(!!sym("artist")))) %>%
  pull(missing)
dat.summary <- dat.summary %>%
  ungroup() %>%
  add_row(`:=`(!!sym("artist"), "Rows with Missing Data"), Observations = missing.obs, Mean = NA, `Standard Deviation` = NA, Min = NA, Q1 = NA, Median = NA, Q3 = NA, Max = NA, IQR = NA)
####################################
# Print Data Summary
####################################
dat.summary


