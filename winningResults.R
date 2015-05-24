#Read in data
history = read.table("data/winningResultsByYear.txt", header=T)

ggplot(history, aes(x=Year, y=Time, group=1)) + 
  geom_line(size=1, colour="green3") + 
  geom_point(size=4, colour="blue") + 
  ggtitle("History of winning times (1st place result) by year") +
  scale_x_continuous(breaks=c(2008:2014), labels=c(2008:2014))