d = read.delim("http://dnett.github.io/S510/LeafArea.txt")
library(lme4)

#o = lme(LeafArea ~ Dose , data = d, random = ~ 1 + Dose| ResearchStation)
oo <- lmer(LeafArea ~ Dose + (1 + Dose | ResearchStation), data = d)
summary(oo)

o7 = lm(LeafArea ~ Dose, data = d[d$ResearchStation == 7,])


#o_red <- lme(LeafArea ~ 1 , data = d, random = ~ 1 + Dose| ResearchStation)
oo_red <- lmer(LeafArea ~ (1 + Dose | ResearchStation), data = d)

#f
cco <- fixef(oo)

dco <- fixef(oo) + ranef(oo)[[1]][7,]

eco <- coef(o7)

cco <- as.numeric(cco)
dco <- as.numeric(dco)
eco <- as.numeric(eco)

ab <- data.frame(line = c("c", "d", "e"), intercept = c(cco[1], dco[1], eco[1]), slope = c(cco[2], dco[2], eco[2]))

#plot

ggplot(d[d$ResearchStation == 7,], aes(x = Dose, y = LeafArea)) + geom_point() + geom_abline(data=ab, mapping=aes(slope=slope, intercept=intercept, linetype=line))

# i,j,k

AIC(oo)

oo_red2 <- lmer(LeafArea ~ 0 + Dose + (1 + Dose | ResearchStation), data = d)

AIC(oo_red2)

AIC(oo_red)

