---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*. 

```{r}

pdays1 <- combolog.test$pdays

#Create height categories of A = 0-7, B=7-14, C=14-998, D=998+

cutpdays1 <- cut(pdays1, breaks=c(-1,7,14,998,999), labels=c("A", "B", "C", "D"), right=TRUE)
combolog.test$pdays <- cutpdays1
colnames(combolog.test)[colnames(combolog.test) == "pdays"] <- "cutpdays"

write.csv(combolog.test, "combolog.test.csv")
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Cmd+Option+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file). 

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.

