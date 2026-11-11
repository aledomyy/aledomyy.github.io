# What the president talks about: text analysis of Mexico's *mañaneras*

Scraper and text-analysis pipeline for the transcripts of President Andrés Manuel López Obrador's morning press conferences, from the start of his term until August 2021. The data are in Spanish.

Original coursework code (Tecnológico de Monterrey, Data Science for Decision-Making II, fall 2021). It is kept close to how it was written. Only absolute file paths were made relative.

## Pipeline

| Script | Step |
|---|---|
| `R/01_scrape_mananeras.R` | Scrapes every transcript from lopezobrador.org.mx (`rvest`, `httr`) → `Mananeras.csv` |
| `R/02_clean_tokenize_topics.R` | Keeps only the president's speech, splits it into ~213,000 sentences, removes stopwords, fits an LDA topic model (15 topics) |
| `R/03_sentiment.R` | Sentence-level sentiment with a Spanish AFINN lexicon (`data/lexico_afinn.csv`); topic-share time series; regression of mood on topic shares |
| `R/04_afinn_by_sentence.R` | Helper: AFINN score per sentence |
| `R/05_topic_model_draft.R` | Earlier draft of the topic model |

The raw transcript file (~50 MB) is not included. Running script 01 rebuilds it. The site may have changed since 2021, so the scraper may need updating.

<!-- TODO (Alejandro): note who else worked on this, if it was a team project, and any known limitations of the method. -->
