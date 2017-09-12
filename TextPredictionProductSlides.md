JHU Data Science Specialization - Capstone: Text Predictor App.
========================================================
author: Shashank Sane
date: 10/09/2017
autosize: true

Data Science Capstone: Text Predictor App.
========================================================
<small>As part of John's Hopkins University Data Science specialization
Capstone project in association with Swiftkey we need to build next word 
predictor Shiny App. Swiftkey has provided the text corpora for this
project</small>
<small>The prediction model was built by applying techniques of Natural Language Processing(NLP) and using the skills developed during the specialization. Specifically a 4-gram language model was developed, the final language model was pruned and optimized to about less than 2.4 MB in size.</small>

<small>The final approach involved the following steps:</small>
* <small>Random sub sampling of text data starting with 5% data to do initial exploratory data analysis. </small>
* <small>Creating 4-3-2-1 gram data model from the sampled data.</small>


========================================================
* <small>Prediction algorithm was built using Stupid Backoff for backing off from 4 to 3 gram and Katz Backoff (KBO) for backing off from 3 to 2 to 1 gram.</small>
* <small>Final model was arrived at by tuning the model parameters and 4-3-2-1 gram size using Cross Validation and benchmarking performance to arrive at model that provides maximum accuracy with minimum data.</small>

<small>All the above steps were performed under constraint of 3.86 GB available RAM and i3 CPU M350 2.27 GHz processor Win7 Laptop.</small>

<small>The final text predictor app can be accessed at: https://saneshashank.shinyapps.io/TextPredictorApp/</small>

<small>Some of the highlights of the app are:</small>
* <small>Extremely low memory footprint of less than 2.5 MB, can be easily run on mobile platform.</small>
* <small>Very fast response time of 10.21 milli seconds.</small>

========================================================
* <small>3 word completion suggestions as soon a user starts typing in the word.</small>
* <small>3 next word suggestions as soon as user hits space key after typing the word.</small>
* <small>The app has a benchmark accuracy of 19.33% for top three predictions.</small>

<small>The app can be used as shown below:</small>

<b>1</b>![](TextPredictor1.png)<b>2</b>![](TextPredictor2.png)

==========================
<b>3</b>![](TextPredictor3.png)

<small>1. Type word to see the word suggestion, click on suggested word to replace typed word with suggested word. </small>
<small>2. Once word has been typed hit space key to view the next word. Click on suggested word to add suggested word to the text.</small>
<small>3. Click on "Clear" button to reset predictor.</small>

Enjoy the app!! Thank You!!





