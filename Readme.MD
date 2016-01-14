---
title: "Readme"
content: Analysis of Holiday Homes in Hungary from a popular advertisement site ingatlan.com 
---

This is a workflow that analysis the data available from a popular property sales site, ingatlan.com regarding holiday homes. See <http://ingatlan.com>.

A webscraper was created to download the data from the website using selectorgadget and rvest library in R. 

```{r}
webscraper_final.R
```

This data was then stripped down from its row format - available prices and site sizes together with lot sizes were gathered, and the addresses were stripped from unnecessary string bits. 

```{r}
text_wrangler_final.R
```

The addresses were then geocoded using google API

```{r}
geocoding_final.R
```


