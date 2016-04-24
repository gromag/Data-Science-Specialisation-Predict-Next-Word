
Predicting next word with Natural Language Processing
========================================================
author: Giuseppe Romagnuolo
date: 24 April 2016

Motivation
========================================================
Predictions of what word comes next in a sentence is very handy when writing on
portable devices that don't have a full size keyboard.

However the techniques used in texting application is common to a variety of other applications,
for example:
- genomics by segmenting DNA sequences
- speech recognition
- automatic language translation
- or even as one student in the course suggested music sequence prediction.

And many more.

ShinyApp application
========================================================

The application I built predicts up to 10 most likely words following of
a sentence. 

- A slider widget allows the user to adjust the number of words suggested from 1 to 10.

- The application was trained on 10% of a Blog, News and Twitter corpora provided in the class.

- it uses Interpolated Modified Kneser-Ney smoothing algorithm applied on trigrams, bigrams and unigrams.


Interpolated Modified Kneser-Ney
========================================================

Kneser-Ney discounting (Kneser and Ney, 1995) augments absolute discounting
with a more sophisticated way to handle the backoff distribution.

The classic example (Jurafsky and Martin 2007) is when faced with a sentence that wasn't seen before like:

"I can't see without my reading ____ ". The word "glasses" seems more appropriate in this context but the word "Francisco" has a higher probability when backing off to a unigram model using MLE. 

Kneser-Ney smoothing uses a different backoff distribution to the MLE, the intuition is to base the estimate on the number of different contexts a word _w_ has appeared in.


Free ShinyApp limitation
========================================================

When it comes to small application the free Shiny server really _shines_.

However it becomes a little bit more challenging to publish a bundle bigger than a few KB. 

In fact, I could not upload 157MB of trained .RData files on to the server.

I eventually had to separate the .RData files and upload to an Amazon server and modify the code so that loaded the data from this remote location.

Unfortunately what before took 5-10sec to initialise it is now taking much longer, however once the .RData is loaded the application is responsive and fun to use.

Acknowledgements and resources
========================================================

A  huge thanks to my fellow students at the John Hopkins Data Science Specialisation. The discussions in the forum have always been inspiring and tremendously helpful.

Resources:

- Speech and Language Processing... (Jurafsky and Martin 2007)
- Implementation of Modified Kneser-Ney Smoothing... (Korner 2013)
- An Introduction to Information Retrieval (Mannin, Raghavan, Shutze 2009)
- Bigrams and Trigrams (Fry 2011)
- NLP Lunch Tutorial: Smoothing (MacCartney 2005)




