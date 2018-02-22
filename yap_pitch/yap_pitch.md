<style>
.section .reveal .state-background {
    background: #92a8d1;}
.section .reveal h1,
.section .reveal p {
    color: #000;
    position: relative;
    top: 4%;}

.reveal h1, .reveal h2, .reveal h3 {
  word-wrap: normal;
  -moz-hyphens: none;
}

</style>

YAP™: Yet Another Predictive Text Model
========================================================
id: slide1
author: Scott D. Weitzenhoffer
date: February 21, 2018
autosize: true
transition: rotate
css: ../www/style.css


========================================================
id: slide2
<h2>Motivation</h2><small>
YAP™ is the capstone project in the Coursera Johns Hopkins University
[Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science).</small>
<p>
<h2>Goal</h2><small>
The goal of this capstone is to build a predictive text model, similar to those
included in mobile texting apps. [SwiftKey](https://swiftkey.com/en) has 
partnered with Coursera for this capstone project and provided a body of [text 
documents](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) 
from twitter, news sites, and blogs.</small>

<h2>[App](https://sweitzen.shinyapps.io/yap-tm/)</h2><small>
Just start typing as if you were using a messaging app. YAP™ will return the top
prediction in context, and a table of up to the top five.</small>

========================================================
id: slide3
<h2>Data Ingest</h2><small>
The source data is 800 Mb, and consists of over four million lines of test. 98% 
was retained for training, and 2% for testing. These data were transformed into 
n-gram frequency tables. Further details can be found in the [milestone 
report](http://rpubs.com/sweitzen/data-science-milestone#ingest).</small>

<h2>Model</h2><small>
The n-grams in the frequency tables were split into the first n-1 words (the 
input X), and the last word (the predicted response, y). The responses are 
scored using a method called "Stupid Backoff". To save on memory, the training
n-grams are pruned to those with count > 8.

One embellishment beyond the base requirements is current-word prediction: If 
the last character of the input is not a space, the last word (fragment) is 
split from the input. The preceding input is used to generate predictions, which 
are then filtered to just those starting with the next-word starting fragment.</small>

========================================================
id: slide4
<h2>Performance</h2><small>
The prediction method typically executes in ~20 ms. The training n-grams occupy 
~79 Mb.  
Running on 10,000 samples from the training data:

```
  accuracy01 accuracy05 accuracy11 accuracy15
1 0.09703835  0.3465964  0.1283994  0.4569995
2 0.12813458  0.2870843  0.3178591  0.6354451
3 0.14035425  0.2767616  0.5588697  0.7802198
4 0.15504950  0.2964356  0.6608187  0.8128655
```
In the table above, row number is the size of the input X in words, and: 
* accuracy01 is the probability the top prediction matched the next word
* accuracy05 is the probability any of the top 5 predictions matched
* accuracy11, 15 are the same, but ignoring least frequent test cases, with 
count = 1
</small>

========================================================
id: slide5

<h2>References</h2> <small>
* ['Large Language Models in Machine Translation'](http://www.aclweb.org/anthology/D07-1090.pdf) 
by T. Brants et al, in EMNLP/CoNLL 2007.
* [Word Prediction Using Stupid Backoff With a 5-gram Language Model](https://rpubs.com/pferriere/dscapreport) 
by Phil Ferriere  
* [Common NLP tasks with Quanteda](http://rstudio-pubs-static.s3.amazonaws.com/169109_dcd8434e77bb43da8cf057971a010a56.html) 
by Eric Rodriguez
* [Capstone n-grams: how much processing power is required?](https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-ngramComputerCapacity.md) 
by Leonard Greski 

<hr>

Visit the YAP™ app at https://sweitzen.shinyapps.io/yap-tm/.  

All code supporting this project is available on GitHub at https://github.com/sweitzen/yap-tm. 

Visit the author on [LinkedIn](https://www.linkedin.com/in/sweitzen/).
</small>
