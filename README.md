## Yet Another Predictive Text Model (YAP&trade;) 
===========================================

YAP&trade; was implemented as the [Data Science Capstone](https://www.coursera.org/learn/data-science-project/) 
project for the Johns Hopkins University Coursera [Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science).

The YAP&trade; app uses a 5-gram language model to predict the next word from 
user input, ranking the predictions using a method called Stupid Backoff, as 
described in ['Large Language Models in Machine Translation'](http://www.aclweb.org/anthology/D07-1090.pdf) 
by T. Brants et al, in EMNLP/CoNLL 2007.


The data supporting this project can be found at  
* https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

---

The project milestone report is available at:
* http://rpubs.com/sweitzen/data-science-milestone

The final product pitch is available at:
* http://rpubs.com/sweitzen/yaptm_pitch

A live demo of YAP&trade; is available at shinyapps.io:
*  https://sweitzen.shinyapps.io/yap-tm/

---

Docker Compose
==============
If you wish to run locally in a Dockerized Shiny Server:

```
version: '3'

services:
  yaptm:
    image: yaptm
    ports:
      - '3838:3838' 
    volumes:
      - shiny-db:/srv/shiny-server

volumes:
  shiny-db:
```

Once the Docker app is up, YAP&trade; will be available in your browser at
`localhost:3838`.