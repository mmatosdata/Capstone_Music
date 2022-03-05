# Capstone_Music

Measurement of a song's success is a relative matter. It could be assessed based on volume of sales, how popular it might be or from a purely artistic point of view regarding its aesthetics and characteristics.

For this project, I have defined as a measure of song success: whether a song  was listed in the Billboard Hot 100 chart, and therefore classified as a "hit".  

I obtained and combined relevant data to predict whether a song can become a "hit" based on its audio features. 

This analysis does not account for other possible confounding factors, such as marketing, performer abilities and trajectory, social or geographical environment, among others factors that could have a direct influence on a song becoming a hit.

The repository includes the following files:

- An Rmd file report detailing the methodology applied, 
- A PDF report (issued from the Rmd file above mentioned), 
- A Html version of the report
- An R script file that generates the analysis and  predictions, and
- source data used

**IMPORTANT NOTES:**

For code to run properly you need to do the following:

**A) Uploading main source data  files**

Place the relevant files in your working directory and review the path for the datasets to upload in lines 23 and 24 (Rscript file "Capstone_music_Code_final"): 

  * Hot_100_Audio_Features <- read.csv("~//Hot 100 Audio Features.csv")   # place your directory path if " ~ " does not work
 
  * genres_v2 <- read.csv("~/genres_v2.csv") # place your directory path if " ~ " does not work


**B) Spotify Performers data:**

**Option 1**- delete the Spotify processing lines and upload Performer data provided:

a) Place file "no_hits_performers_sample" (located in zip file "source_data") in your working directory

b) Delete lines from 106 to 140 in the Rscript file called "Capstone_music_Code_final"

c) Place the following code instead:

   * no_hits_performers_sample <- read.csv("~/no_hits_performers_sample.csv")*  # place your directory path if " ~ " does not work


**Option 2** - provide a Spotify ClientId and Client Secret (you need a Spotify account to obtain those):

In the Rscript file "Capstone_music_Code_final", add the definition of these element before line 106 as follows:

  * Client_Id <- "your client Id"  #input your client id here

  * Client_secret <- "your client secret"  #input your client secret here

See links for instructions on how to get your Client_id and Client secret:

https://cran.r-project.org/web/packages/spotidy/vignettes/Connecting-with-the-Spotify-API.html#:~:text=Log%20in%20with%20your%20Spotify,unhide%20your%20'Client%20secret'.

https://support.heateor.com/get-spotify-client-id-client-secret/


**REFERENCES**

The following material was used a reference:

* [EDX course material](https://rafalab.github.io/dsbook/)

* [Caret Package – A Practical Guide to Machine Learning in R](https://www.machinelearningplus.com/machine-learning/caret-package/#4howtovisualizetheimportanceofvariablesusingfeatureplot)

* [Spotifyr package](https://www.rdocumentation.org/packages/spotifyr/versions/2.2.3)

* [Boruta package](https://cran.r-project.org/web/packages/Boruta/Boruta.pdf) 

* [Machine learning mastery with R](https://machinelearningmastery.com/machine-learning-with-r/)

* [R-Gallery](https://www.r-graph-gallery.com/)

* [Data source: Hot 100 Billboard songs](https://data.world/kcmillersean/billboard-hot-100-1958-2017)

* [Data source: Dataset of songs in Spotify](https://www.kaggle.com/mrmorj/dataset-of-songs-in-spotify)




