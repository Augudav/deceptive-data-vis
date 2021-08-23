#Linegraph
library(ggplot2)
df <- data.frame(valuex=c(1, 4),
                 valuey=c(1, 4))
ggplot(df, aes(x=valuex, y=valuey)) + geom_line(size=2) + ggtitle("Základní linegraph") + xlab("Osa x") + ylab("Osa y")
#bargraph
df <- data.frame(valuex=c("A", "B"),
                 valuey=c(3, 6))
ggplot(data=df, aes(x=valuex, y=valuey)) + geom_bar(stat="identity", width = 0.3)+ ggtitle("Základní bargraph") + xlab("Osa x") + ylab("Osa y")
#effect as area (bubblegraph)
df <- data.frame(valuex=c("A", "B"),
                 velikost=c(2,3))

ggplot(df, aes(x=valuex, y=1, size = velikost)) +  geom_point() + ggtitle("Základní Effect as area") + xlab("Osa x") + ylab("Osa y")
#piechart
df <- c(1,3,7,5,2)
skupiny <- c("A","B","C","D","E")
pie(df, labels = skupiny) + title("Základní piechart ")
#cumulative
df <- data.frame(valuex=c(1:50),
                 valuey=c(rnorm(50, 50, 45)))
ggplot(df, aes(x=valuex, y=cumsum(valuey))) + geom_line(size=1) + ggtitle("Základní Cumulative graph") + xlab("Osa x") + ylab("Osa y")
#log chart
df <- data.frame(valuex=c(1:12),
                 valuey=c(1,4,9,16,25,36,49,64,81,100,121,169))
ggplot(df, aes(x=valuex, y=cumsum(valuey))) + geom_line(size=2) + scale_y_continuous(trans='log10')+ ggtitle("Základní Log graph") + xlab("Osa x") + ylab("Log Osa y")