library(ggplot2)
library(dplyr)
library(magrittr)
library(ggforce)

### Začátek upravených grafů
#Linegraph - AR
df <- data.frame(valuex=c(1, 4),
                 valuey=c(1, 4))
change_type <- "aspect_ratio"
ars <- seq(.5, 3, by = .1)
for (i in 1:length(ars)) {
  ar <- ars[i]
  ggplot(df, aes(x=valuex, y=valuey)) + ylim(0,4) + xlim(0,4) + theme(aspect.ratio = ar)+ geom_line(size=2) + ggtitle("Linegraph",ar) + xlab("Osa x") + ylab("Osa y")
  ggsave(path ="Linegraph", filename = sprintf("%s%f.png",change_type, ar))
}

#Linegraph - Y-limits 
df <- data.frame(valuex=c(1, 4),
                 valuey=c(1, 4))
change_type <- "Y_limits"
limits <- seq(0,5, by = 1)
for (i in 1:length(limits)) {
  limit <- limits[i]
  ggplot(df, aes(x=valuex, y=valuey)) + ylim(0,4+limit) + xlim(0,4) + theme(aspect.ratio = ar)+ geom_line(size=2) + ggtitle("Linegraph",limit) + xlab("Osa x") + ylab("Osa y")
  ggsave(path ="Linegraph", filename = sprintf("%s%f.png",change_type, limit))
}

#Linegraph - X-limits 
df <- data.frame(valuex=c(1, 4),
                 valuey=c(1, 4))
change_type <- "X_limits"
limits <- seq(0,5, by = 1)
for (i in 1:length(limits)) {
  limit <- limits[i]
  ggplot(df, aes(x=valuex, y=valuey)) + ylim(0,4) + xlim(0,4+limit) + theme(aspect.ratio = ar)+ geom_line(size=2) + ggtitle("Linegraph",limit) + xlab("Osa x") + ylab("Osa y")
  ggsave(path ="Linegraph", filename = sprintf("%s%f.png",change_type, limit))
}


#Linegraph - inverted_axis
df <- data.frame(valuex=c(1, 4),
                 valuey=c(1, 4))
change_type <- "inverted_axis"
ggplot(df, aes(x=valuex, y=valuey)) + ylim(0,4) + xlim(1,4) + scale_y_reverse()+ geom_line(size=2) + ggtitle("Linegraph inverted") + xlab("Osa x") + ylab("Osa y")
ggsave(path ="Linegraph", filename = sprintf("%s.png",change_type))



#piechart-rotace
change_type <- "Rotation"
df <- data.frame(valuex=c("A", "B", "C", "D", "E","F"),
                 valuey=c(3, 6, 1, 5, 2, 7))
rotations <- seq(.25,2, by = .25)
for (i in 1:length(rotations)) {
  rotation <- rotations[i]*pi
  ggplot(df, aes(x = "", y = valuey, fill = valuex)) +
    geom_bar(width = 1, stat = "identity", color = "white") +ggtitle("Piechart %s",change_type)+
    coord_polar("y", start = rotation)
  ggsave(path ="Piechart", filename = sprintf("%s%f.png",change_type,rotation ))
}


#piechart-řazení_podle_velikosti (nefunguje)
change_type <- "order"
df <- data.frame(valuex=c("A", "B", "C", "D", "E","F"),
                 valuey=c(3, 6, 1, 5, 2, 7))

ggplot(df, aes(x = "", y = valuey, fill = valuex)) +
  geom_bar(width = 1, stat = "identity", color = "white") +ggtitle("Piechart_unordered")+
  coord_polar("y", start = 0)
ggsave(path ="Piechart", filename = sprintf("piechart_unordered.png"))

##kód z konzultace na ordered piechart
order_subj <- 
  df %>%
  group_by(subject_id) %>% 
  do(Hmisc::smean.cl.boot(.$correct) %>% t() %>% as_tibble()) %>% 
  arrange(Mean) %>% 
  ungroup() %>% 
  mutate(subject_id = forcats::as_factor(as.character(subject_id)))
##konec kódu z konzultace

ggplot(sorted_df, aes(x = "", y = valuey, fill = valuex)) +
  geom_bar(width = 1, stat = "identity", color = "white") +ggtitle("Piechart_ordered")+
  coord_polar(theta="y", start = 0)
ggsave(path ="Piechart", filename = sprintf("piechart_ordered.png"))

#cumulative
df <- data.frame(valuex=c(1:50),
                 valuey=c(rnorm(50, 50, 45)))
ggplot(df, aes(x=valuex, y=cumsum(valuey))) + geom_line(size=1) + ggtitle("Základní Cumulative graph") + xlab("Osa x") + ylab("Osa y")
Printing_cumsum <-function(a) {
  ggplot(a, aes(x=valuex, y=cumsum(valuey))) + geom_line(size=1) + ggtitle("Cumulative graph") + xlab("Osa x") + ylab("Osa y")
  ggsave(path ="Cumulative", filename = sprintf("Cumulative.png"))
  ggplot(a, aes(x=valuex, y=valuey)) + geom_line(size=1) + ggtitle("Cumulative graph-rozloženo") + xlab("Osa x") + ylab("Osa y")
  ggsave(path ="Cumulative", filename = sprintf("Cumulative_rozlozeno.png"))
}

#log chart
df <- data.frame(valuex=c(1:12),
                 valuey=c(1,4,9,16,25,36,49,64,81,100,121,169))
ggplot(df, aes(x=valuex, y=valuey)) + geom_line(size=2) + scale_y_continuous(trans='log10')+ ggtitle("Základní Log graph") + xlab("Osa x") + ylab("Log Osa y")
ggsave(path ="Log_chart", filename = sprintf("Log_chart_y.png"))

ggplot(df, aes(x=valuex, y=cumsum(valuey))) + geom_line(size=2) + ggtitle("Log graph bez transformace") + xlab("Osa x") + ylab("Log Osa y")
ggsave(path ="Log_chart", filename = sprintf("Log_chart_basic.png"))

#effect as area (pouze kód z  konzultace)
df <- tibble(x = c(0,10), y =c(5,5), r = c(2,3))
df %>%
  ggplot(aes(x0 = x,y0 = y, r =r)) +
  geom_circle(fill = "blue") +
  ylim(-5,15) +
  xlim(-5,15)+
  coord_fixed()
### Konec upravených grafů
