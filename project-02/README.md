# the_past_decade_in_music

# Part 1: Visualizing Happiness

**FULL REPORT** [HERE](https://github.com/angel-sarmiento/the_past_decade_in_music/tree/master/reports)

## Introduction

We are constantly consuming multiple genres and subgenres of music each day. Developing an understanding of this music is something widely underappreciated and accomplished, however, the programmers at spotify deem it a necessary feature that was to be integrated into their platform and business model. In doing this, they created an API open for developers to integrate their data into applications and analytics. Other companies like Genius and LastFM have created APIs as well to both bring lyrics and genre tags respectively into the developer's hands. 
 

### What is the relationship between `energy` and `valence` from the last decade? 

From the Spotify API, there are a number of measures defining the *features* found in music. These features are as follows (taken from the official [documentation](https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/)):

`acousticness`: A confidence measure from 0.0 to 1.0 of whether the track is acoustic.  
`danceability`: Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity.  
`energy`:	Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity.  
`instrumentalness`:	Predicts whether a track contains no vocals.  
`liveness`: Detects the presence of an audience in the recording.  
`loudness`: The overall loudness of a track in decibels (dB).  
`speechiness`: Speechiness detects the presence of spoken words in a track.  
`valence`: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.  

One might think from the descriptions, that `energy` and `valence` *must* be related. Let's plot them all on a graph to see. 


![](https://github.com/angel-sarmiento/the_past_decade_in_music/blob/master/reports/images/point_viz.png)


From this graph it looks as though these two variables are in fact positively correlated. Some of the outliers show that it is possible to be a very happy song without being very energetic. This makes sense in contexts like songs that have really positive lyrics but are not generally so upbeat and happy. Hovering over **Whack World** in the album list shows one outlier, which happens to fall on an album that generally follows the trend stated before. 

### How happy was music around the globe? 

Being able to see how happy certain albums are is nice, but what about albums across the globe? How does the average happiness of music change as we move overseas? 


![](https://github.com/angel-sarmiento/the_past_decade_in_music/blob/master/reports/images/spatial_viz.png)


According to the data, of the top 100 songs/albums from the billboard top 100 of decade that are available in these countries, the african countries score the highest in happiness. Now keep in mind that this is based on *availability* not necessarily based on these country's own best music of the decade. It stands to reason that some music might not be available in all of these countries and this may skew these results. 

What might be more fruitful to look at is how these values change within continents. Take for example, Europe. The eastern countries in Europe tend to have much happier music when compared to their western counterparts. Moldova is the only exception to this rule. Australia and Japan have some of the saddest music from Billboard's charts, while Venezuela has some of the happiest.

### What is the relationship between happiness and the other variables? 

There are a few parameters in the Spotify API that are computed as functions of other parameters found in the data. These parameters are `energy`, `loudness`, and `acousticness`. For example, `loudness` is used in the calculations of `energy`. For this reason, these terms will be modeled as interaction terms in the regression model. The plots demonstrating these relationships are omitted here for this reason as well. The documentation expounding on these parameters is found [here](https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/).

For a GLM, k-folds cross-validation is done without any standardization of the parameters. The interaction terms are included. The objective of this model is developing an understanding of how these other parameters perform as predictors for `valence`, as well as how well they can be used for an inferential model.


![](https://github.com/angel-sarmiento/the_past_decade_in_music/blob/master/reports/images/reg-output.png)


From the model, it can be seen that `Danceability`, `energy`, `speechiness`, the intercept, and `acousticness` to a lesser extent are significant predictors of valence. Even with the interaction terms, `energy` is incredibly with a p-value of 2e-16. Unfortunately, the parameters listed only explain about 34% of the variance in `valence`. This is due to some missing information and imperfect modeling for the individual parameters. From this, the coefficients seem to lead in the direction described above in regards to `energy` and `valence`. That is, if there were an increase of energy from one song to another, then there should be an expected increase of about 0.65% in valence. 
