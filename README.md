# kossuth-radio-nlp

### Summary

In this project I collected the subscripts of Kossuth Radio between 1988 and 1990, and analized its partialty towards the socialist regime. Kossuth Radio was, and still is the number one state radio in Hungary. This period was really turbulent, as these are the years of transition from socialism to democracy (actually, by 1988 it had already started). In the beginning of 1988, Károly Grósz was the Chairman of the Council of Ministers, who was devoted to the one-party system, while in the end of 1990, Hungary was a republic, and had its own, freely elected government.

My hypothesis is that in the beginning, the radio had a large bias towards the communists, which got more and more balanced later on.

In my analysis, I'm using two natural language processing techniques: sentiment analysis and wordfish. With the sentiment analysis, I can observe the sentiment towards the political actors, and the general sentiment of the radio. Wordfish is a method developed for locating political parties based on one dimension, and I try to use it for locating the sides according to how favored they are by the radio.

### Data

I collected the data from the website of the Open Society Archives. They have a collection called [Hungarian Monitoring](https://catalog.osaarchivum.org/?utf8=%E2%9C%93&search_field=search_all&q=hungarian%20radio%20monitoring) with all the subscripts I used. The data was acquired and preprocessed with Python (see src/data_collection and src/preprocess). After the preprocess I had a table where each row represented (roughly) one paragraph, along with the corresponding date and paragraph number within the text. Fortunately, all the documents contained text, not pictures. Also, I've collected the names of the people appearing in the subscripts - also from OSA's website. I used 1067 documents, originally in pdfs, with 70-100 pages each.

The quality of the data was far from the best, but I could work with it. The problems included rotated pages, which were rotated before the OCR process, so they had incomprehensible text. Since it occured randomly, I dropped these pages. One other problem was the frequent misspelling and failure of the OCR, which I left as is. These errors add noise to my analysis, but fortunately, the dataset is large, so I can cope with it.

### NLP

Further processing, NLP and visualization was done in R (see src/nlp.R).

My approach was to consider one paragraph as one document; one piece of text wich belongs together. I also had to label these paragraph according to which political side they belonged to. I did this labelling the following way:

1. I filtered the list of names from OSA's website to those with at least 15 mentions.

2. I categorized them into three political groups: the communists, the reformers and the opposition.

3. I located these people in the paragraphs, and flagged the paragraphs by their political group. One paragraph could get multiple flags from multiple parties.

The three groups are not too exact, but I followed this pattern:

- communists: Those people who wanted to maintain the socialist one-party regime. They were mostly in power before the November of 1988.

- reformers: Actually, they are reformcommunists. They wanted to reform the socialism, but maintain it to some extent. They were in power from November, 1988 to May 1990. Most of them continued their carreer in the Hungarian Socialist Party, which was one of the main political parties in the past 30 years.

- opposition: Basically everybody else, who pursued democracy.

Foreigners were also categorized in the same way (democratically elected leaders were labelled as opposition, as they were supporting the democratic transition).

#### Sentiment analysis

The general sentiment of the text can show us how the radio interprets the world around as. A good sentiment can have multiple sources: bad news, pessimistic interviewee or disapproval of somebody/something. My expectation is that a radio which supports the political party in power tends to favour positive sentiments in general. They prefer good news to bad news, optimistic views to pessimistic views, approves the incumbents and disapproves the opposition.

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/general_sentiment.svg)

The dashed line on the plot refers to changes of the director at the state radio, while the dotted line shows the major political events. Miklós Németh was the last Chairman of the Council of Ministers and the first prime minister as well, while Jószef Antall was the first elected prime minister.

The sentiments have a clear decreasing trend with some ups and downs. The spikes in the underlying data are mostly due to specific news, for example, the largest peak in 1988 June-July was because of the laudatory reports and news about the 'perestroika' (new economic stimuli started in the Soviet Union in 1985).

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/total_sentiment.svg)

I also plotted the total absolute sentiments. What's interesting here are the two positive outliers. The one in the beginning of July 1989 was due to the death of János Kádár, the former Secretary of the Communist party, and leader of Hungary for 32 years, and the reburial of Imre Nagy, prime minister of Hungary in the revolt of 1956 against the Russians. These two events on the same day generated many sentiments: negatives (sorrow, death, etc.), and positives (words of admiration) as well. The second peak in the end of 1989 is due to Christmas.

This latter plot however is more of a double check if my analysis has any plausibility.

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/political_group_level.svg)

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/political_group_relative.svg)

Here, I separated the sentiments into three groups according to the three political sides I identified. The sentiments were quite correlated, so I created the same plot, but now I sutbtracted the mean from all the three values. This second graphs supports my original hypothesis, as the communists' sentiment was dropping, while the opposition's was rising. Actually it also shows a slight evidence about the role of the director at the state radio, because both replacements happen at an inflection point (the causality can be the other way round though). One surprising thing is the role of the reformers: although they were in power in the middle (and most) of the period, they always had their relative sentiment slightly below the mean.

### Topic analysis

One thing I haven't talked about yet, but it's important: the words and topics in the text. I'm not going into topic analysis, but I had to deal with it a bit, because it turned out, that most of my texts are news about foreign policy.

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/word_count.svg)

These, and other common words describe the topics well, but they are mostly useless for me. In the top 100, we can find many other countries and capital cities (names of the people I collected for categorization are omitted). It's not surprising, that many of the sentimented words are connected to foreign people - the two most common are Gorbachev and Bush. The most common words are similar across the three groups.

I also tried topic modelling with LDA, and the result was the following:

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/lda_updated.svg)

I haven't translated this graph, but annotated all the words with red which has a clear reference to foreign policy (eg. countries, capitals). Yet, it's still not too interesting for me, but I'll explain soon, why is it important.

### Wordfish

Wordfish is a methodology developed for placing parties on a political spectrum (for more: J. B. Slapkin and S. Proksch (2008): A Scaling Model for Estimating Time-Series Party Positions from Texts). It's basically a scaling algorithm, which can scale the observarios accross time in one dimension. It uses Bayesian approach to predict the occurences of words.

There are two main parts of the results. One is the scaling over time, and the other one is the coefficient (beta) of each word. To make it simple, parties having a negative theta tend to have more of the words with negative betas, and the same is true for positive thetas and positive betas.

First, I created a wordfish model for all the tokens related to one of the parties, but it didn't work, because it used different countries and cities as most important words for the scaling, and all the groups were highly correlated.

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/wordfish_all.svg)

Actually, what happened is that it scaled the sides according to the actual foreign policy situation. I also tried it without foreiners in my labelling group, but it didn't help, most of the words were still about foreign policy in both ends. The only reasonable output I could get from the wordfish was when I only used the sentimented words.

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/wordfish_sent.svg)

The groups are still correlated, but they can be separated. At least, for the last two years, the opposition seems to differ from the other two groups.

![](https://github.com/dszokolics/kossuth-radio-nlp/blob/master/graphs/wordfish_sent_word.svg)

The words with negative and positive betas both include words with negative and positive sentiments. They are basically reflecting the words used for indicating if something is good or bad in a given political group (eg. slump is used for economic stagnation of the 80s mostly by the communists). Also, the actual topics can influence these features (eg. funeral, "solidarity" can stand for the Polist political party). To sum it up, based on the wordfish analysis, I can't really draw any conclusions about the bias towards one political group.

In order to have more accurate results, I would try more sophisticated algorithms to categorize foreign and local news, and focus on the latter one. Also, it would be favourable to look for reference texts, and try to do the scaling with the help of those.
