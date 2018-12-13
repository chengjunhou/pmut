df = data.frame(ggplot2::diamonds)
head(df)

g <- ggplot(df, aes(x=cut))
g + geom_bar(aes(fill=clarity),position = "fill") + scale_y_continuous(labels = scales::percent)

g <- ggplot(df, aes(x=cut))
g + geom_bar(aes(fill=clarity),position = "dodge") + scale_y_continuous(labels = scales::percent)

g <- ggplot(df, aes(x=cut))
g + geom_bar(aes(y=..density.., fill=clarity),position = "fill") + scale_y_continuous(labels = scales::percent)

g <- ggplot(df, aes(x=cut))
g + geom_bar(aes(y=..density.., fill=clarity),position = "dodge") + scale_y_continuous(labels = scales::percent)


g <- ggplot(mpg, aes(class))
g + geom_bar(aes(fill = drv), position = "fill")



ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
  geom_bar(position = "dodge")

ggplot(mtcars, aes(factor(cyl), fill = factor(vs))) +
  geom_bar()

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram()

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(position = "dodge", binwidth = 500)

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(position="identity", binwidth = 500, alpha=.5)

ggplot(diamonds, aes(price, colour = cut)) +
  geom_freqpoly(binwidth = 500)

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(aes(y = ..density..), position="identity", binwidth = 500, alpha=.5) +
  scale_y_continuous(labels = scales::percent)

ggplot(diamonds, aes(price, colour = cut)) +
  geom_freqpoly(aes(y = ..density..), binwidth = 500) +
  scale_y_continuous(labels = scales::percent)



ggplot(diamonds, aes(price)) + geom_histogram(binwidth = 500)
ggplot(diamonds, aes(price)) + geom_histogram(aes(y = ..density..), binwidth = 500)

ggplot(diamonds[diamonds$cut=="Ideal",], aes(price)) +
  geom_histogram(aes(y = ..density..), binwidth = 500) + scale_y_continuous(labels = scales::percent)



dd = df; dd$cut=as.character(dd$cut);dd$clarity=as.character(dd$clarity);
dd[dd$cut=="Ideal"&dd$clarity=="IF","clarity"]=""
g <- ggplot(dd, aes(x=cut))
g + geom_bar(aes(fill=clarity),position = "fill") + scale_y_continuous(labels = scales::percent)




pmut.comp.disc(df, varstring="cut", targetstring="clarity", hist.bool=TRUE)
pmut.comp.cont(df, varstring="cut", targetstring="price", hist.bool=TRUE)


