# Interactive Virsualization for Terrorism
Dara source: http://start.umd.edu/gtd/contact/ (The Global Terrorism Database (GTD))

Interactive Page: https://bson0001.shinyapps.io/Terrorism_June_4/ 

## Introduction
The main message I would like to convey is anti-terrorism by telling a story of a city called 'Baghdad' in Iraq. Terrorism has taken 0.17 million people’s lives, destroyed a hundred cities, made countless family broken within a decade. Especially, recent years, terrorist attacks happened around the world, the news and reports on the television and internet overwhelm us. Some people go off the track to join terrorist organizations, some governments furtively support terrorist organizations by funds or weapons because of some political purposes, and the public is not clear about the danger of terrorism. Therefore, the intended audience is the politician and the public. Besides, the information on the number of killed and wounded, location information, the terrorist organization, attack number, attack type, attack weapon, and text will be included in the shiny application.

## Implementation
I use Shiny with Rmarkdown in Rstudio to implement my design, because Shiny and Rmarkdown can work together to produce interactive web-based page.

There are two main files used for building the shiny application. The first one “displayPre.R” contains all functions used to generate charts, map and word cloud and useful list for interactive visualization selection such as top 30 dangerous cities’ names. Another file is “Terrorism.Rmd” which is runnable to produce the web page.
