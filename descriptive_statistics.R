#Read in data
data = read.table("data/processedData.txt", header=T)

#Find correlation between attributes using t-test
t.test(data$s.nr, data$place)

t.test(data$particip.time, data$place)

data$age.group.number = as.numeric(substr(data$age.group, 2, 3))
t.test(data$age.group.number, data$place)

data$country.number = as.numeric(data$country)
t.test(data$country.number, data$place)
