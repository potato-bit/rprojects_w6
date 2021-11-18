library('tidyverse')
library('emmeans')
library('afex')
library('cowplot')

d1 <- read_csv("cornwell_krantz_2014_s2.csv") %>%
  mutate(
    condition =
      factor(
        condition,
        levels = 1:4,
        labels = c("third-person", "second-person", "passive", "no-justification")
      ))
head(d1)

# TASK 1
d1 %>% group_by(id) %>% summarise(n=n()) %>% ggplot(aes(x=id,y=n)) + geom_line()
### all participants have the same number of observations (n=8)

### number of participants per condition
d1 %>% group_by(condition) %>% summarise(n=n_distinct(id))

### number of scenarios per condition
(t1 <- d1 %>% group_by(scenario,condition) %>% summarise(n=n()) %>% pivot_wider(names_from=condition,values_from=n))


# TASK 2
a1 <- aov_car(support ~ condition + Error(id), d1)
a1
### Only significant at the 10% level, fail to reject the hypothesis that support is the same between the different 
### conditions
a2 <- aov_car(achieve ~ condition + Error(id), d1)
a2
### At the 5% level, we reject the hypothesis that achieve values are the same for different conditions between 
### participants
a3 <-  aov_car(unintended ~ condition + Error(id), d1)
a3
### We fail to reject the hypothesis that unintended values are the same for different conditions between participants


### H_0: All conditions achieve the same level of the DV v. 
### H_1: At least one condition achives a different level of the DV
### At the 10% significance level, both support and achieve are statistically significant, however only achieve is 
### statistically significant at the 5% level.


# TASK 3
d1 <- d1 %>% mutate(acceptability=(support + achieve + (8-unintended))/3)
a4 <- aov_car(acceptability ~ condition + Error(id), d1)
a4
### Only significant at the 10% level, fail to reject the hypothesis that acceptability scores are statistically 
### similar for different conditions between participants

### This will produce different results because it is using non-aggregated data in the ANOVA unlike afex.
l1 <- lm(acceptability ~ condition, d1)
car::Anova(l1,type=3)

### aggregate by participant id
d2 <- d1 %>% group_by(id,condition) %>% summarise(support=mean(support),achieve=mean(achieve),unintended=mean(unintended),
                                                  acceptability=mean(acceptability))
head(d2)

### running car::Anova() again
l2 <- lm(acceptability ~ condition, d2)
car::Anova(l2,type=3)
### which has the same results as aov_car()



# TASK 4
p1 <- afex_plot(a1,'condition')
p2 <- afex_plot(a2,'condition')
p3 <- afex_plot(a3,'condition')
p4 <- afex_plot(a4,'condition')
plot_grid(p1,p2,p3,p4,nrow=2)



# TASK 5
### Applying contrasts to ANOVAs at at least 5% level of significance and a4
(e1 <- emmeans(a2,'condition'))
(e2 <- emmeans(a4,'condition'))
c1 <- list(III_v_other=c(1,-1/3,-1/3,-1/3))
c2 <- list(III_v_II=c(1,-1,0,0))
c3 <- list(III_v_IIandP=c(1,-1/2,-1/2,0))
c4 <- list(II_v_other=c(-1/3,1,-1/3,-1/3))
c5 <- list(NJ_v_III=c(-1,0,0,1))
c6 <- list(NJ_v_II=c(0,-1,0,1))
c7 <- list(NJ_v_P=c(0,0,-1,1))

contrasts = list(c1,c2,c3,c4,c5,c6,c7)
e_means = list(e1,e2)

for (c in contrasts) {
  print(contrast(e1,c))
}

for (c in contrasts) {
  print(contrast(e2,c,adjust='holm'))
}


### research contrasts: III_v_II, IIInII_v_NJnP, III_v_NJnP, II_v_NJnP



# TASK 6
e2
s1 <- d2 %>% group_by(condition) %>% summarise(avg=mean(acceptability),se1=sd(acceptability)/sqrt(n()))
d2 <- d2 %>% mutate(sqd=(acceptability-mean(d2$acceptability))^2) %>% select(id:unintended,acceptability,sqd)
s2 <- d2 %>% group_by(condition) %>% summarise(se2=(0.861/sqrt(n())))
s2              
e2
s1
?emmeans
