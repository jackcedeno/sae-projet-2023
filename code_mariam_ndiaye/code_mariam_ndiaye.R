percents = c(

  'RHC' = nrow(df[df$SWANG1 == "RHC",]) / 5508,

  'Pas de RHC' = nrow(df[df$SWANG1 == "No RHC",]) / 5508

)

percents <- stack(percents)

 

p <- ggplot(percents, aes(ind, values)) + geom_bar(stat="identity", fill = rgb(0.1,0.4,0.5,0.7))+ ylab('Pourcentage') +

  geom_text(aes(label = paste(round(values*100,1), '%'), vjust = 2), color = 'white') +

  scale_y_continuous(labels = scales::percent)

p

 

 

percents = c(

  'No' = nrow(df[df$DEATH == "No",]) / 5508,

  'Yes' = nrow(df[df$DEATH == "Yes",]) / 5508

)

percents <- stack(percents)

 

p <- ggplot(percents, aes(ind, values)) + geom_bar(stat="identity", fill = rgb(0.1,0.4,0.5,0.7))+ ylab('Pourcentage') +

  geom_text(aes(label = paste(round(values*100,1), '%'), vjust = 2), color = 'white') +

  scale_y_continuous(labels = scales::percent)

p