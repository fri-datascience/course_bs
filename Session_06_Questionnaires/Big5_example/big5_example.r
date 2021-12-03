# The scale was labeled 1=Disagree, 3=Neutral, 5=Agree
# 
# EXT1	I am the life of the party.
# EXT2	I don't talk a lot.
# EXT3	I feel comfortable around people.
# EXT4	I keep in the background.
# EXT5	I start conversations.
# EXT6	I have little to say.
# EXT7	I talk to a lot of different people at parties.
# EXT8	I don't like to draw attention to myself.
# EXT9	I don't mind being the center of attention.
# EXT10	I am quiet around strangers.
# EST1	I get stressed out easily.
# EST2	I am relaxed most of the time.
# EST3	I worry about things.
# EST4	I seldom feel blue.
# EST5	I am easily disturbed.
# EST6	I get upset easily.
# EST7	I change my mood a lot.
# EST8	I have frequent mood swings.
# EST9	I get irritated easily.
# EST10	I often feel blue.
# AGR1	I feel little concern for others.
# AGR2	I am interested in people.
# AGR3	I insult people.
# AGR4	I sympathize with others' feelings.
# AGR5	I am not interested in other people's problems.
# AGR6	I have a soft heart.
# AGR7	I am not really interested in others.
# AGR8	I take time out for others.
# AGR9	I feel others' emotions.
# AGR10	I make people feel at ease.
# CSN1	I am always prepared.
# CSN2	I leave my belongings around.
# CSN3	I pay attention to details.
# CSN4	I make a mess of things.
# CSN5	I get chores done right away.
# CSN6	I often forget to put things back in their proper place.
# CSN7	I like order.
# CSN8	I shirk my duties.
# CSN9	I follow a schedule.
# CSN10	I am exacting in my work.
# OPN1	I have a rich vocabulary.
# OPN2	I have difficulty understanding abstract ideas.
# OPN3	I have a vivid imagination.
# OPN4	I am not interested in abstract ideas.
# OPN5	I have excellent ideas.
# OPN6	I do not have a good imagination.
# OPN7	I am quick to understand things.
# OPN8	I use difficult words.
# OPN9	I spend time reflecting on things.
# OPN10	I am full of ideas.

library(psych)
library(GPArotation)

# load & remove non-questions
x <- read.csv("data.csv", sep = "\t")
y <- x[,-c(1,2,3,4,5,6,7)]

# standardizing the data
# - When looking for factors, it typically makes sense to standardize the data.
#   It does in other analyses as well, but here we really don't want some
#   variable to stand out just because it has a large scale and therefore
#   variability. Of course, if there is some meaningful scale, we should not
#   change it. For example, if all our measurements are in meters, we typically 
#   wouldn't want to standardize, because it will make the least variable measurements
#   relatively more important.
dat <- scale(y)

# preliminary inspection of the data
# - Just looking at a correlation plot might reveal some patterns. In this case,
#   that there appear to be 5 groups of questions. Of course this is more 
#   dificult when not all scales are at least ordinal. For nominal scales, 
#   we need to do more work or treat those questions separately. Note that you 
#   can always treat binary questions as ordinal, albeit low resolution.
cor.plot(dat)

# factor analysis(FA)
# - One of the key things in FA is to determine the number of factors. In this 
#   case even an eye-test is enough. In general, this is a known problem, equivalent
#   to determining the number of clusters. We'll use a scree plot (a plot of the
#   eigenvalues [explained variance] as we add more factors). We'll also use a
#   simulation-based procedure to determine what is to be expected for such data
#   if the data were random. This will give us a cut-off point.
# - If we use the 'elbow' method, we might say that there are 6-7 factors. The
#   automated procedure suggests there are 10. 

fa.parallel(dat, fa = "fa")

# - If we look at the proportion of variance explained for 6 factors, we can see
#   that there is a substantial drop at the 6th factor. So we could argue that 
#   there are only 5 major factors.

fa(dat, nfactors =  6, rotate = "varimax")

# - As expected, the 5 factors load on 10 questions each.

res <- fa(dat, nfactors =  5, rotate = "varimax")
print(res)
fa.diagram(res)

# split-half reliability
# - We're now going to evaluate the internal consistency. We have to do this for
#   each factor separately. We'll use Extraversion as an example, but the process
#   is the same.
# - We have to make sure that all the correlations are positive. We already know
#   that all these questions are aligned with the same factor, but in order to
#   check internal consistency, we need them all "pointing in the same direction",
#   not some of them pointing in the opposite direction, because it will affect 
#   the means.

z <- y[,1:10]
cor(z)
z[, seq(2, 10, 2)] <- -z[, seq(2, 10, 2)] 
cor(z)

# - Internal consistency is just another word for redundancy. We want to check
#   if these questions are indeed redundant measurements of the same latent
#   variable. Ideally, we would have perfect redundancy and could discard all 
#   but one question. In practice, however, responses are noisy. So we benefit
#   from having more than one question for each latent dimension. By taking their
#   mean, we cancel out some of the error and reduce uncertainty.
# - First, we'll use the simple split-half approach, spliting at the mid point.
#   If we have a lot of redundancy, we would expect that the mean obtained from
#   a subset of the questions would correlate (across respondents) with the mean
#   obtained from the other questions.

x1 <- rowMeans(z[,1:5])
x2 <- rowMeans(z[,6:10])
cor(x1, x2)

# - 0.819 is relatively good and similar to numbers reported in research of the
#   Big 5 personality traits questionnaire.
# - We should deal with 2 issues, however:
#    * negate the arbitrary split; we'll do via Monte Carlo instead of exhaustive
#      computation of all possible splits into two equal-sized halves,
#    * correct for the fact that we use one half of the items instead of all
#      to compute the mean (if we "used all 10", we would have less noise in the
#      means and therefore better correlation); we do this using the Spearman-Brown
#      correction n * rho / (1 + (n - 1) * rho). This formula can also be used to 
#      estimate how reliability will change if we increase/decrease the length of a 
#      questionnaire. The case n = 2 is when we double the number of questions.

m <- 1000
cors <- c()
set.seed(0)
for (i in 1:m) {
  idx  <- sample(1:10, 5, rep = F)
  x1   <- rowMeans(z[,idx])
  x2   <- rowMeans(z[,-idx])
  rho  <- cor(x1, x2)
  cors <- c(cors, 2 * rho / (1 + rho))
}
mean(cors)
sd(cors) / sqrt(m)

# - Split-half has been superseded by Cronbach's alpha, which is now more or less
#   the gold standard for reporting internal consistency (reliability). However,
#   despite some technical advantages, Cronbach's alpha is in practice very similar
#   to reliability obtained with our split-half procedure.

res <- psych::alpha(z) 
res$total$raw_alpha

# confirmatory factor analysis
# - Another way of testing internal consistency is to use confirmatory factor 
#   analysis (CFA). Unlike exploratory factor analysis (EFA), where we explore 
#   the factor structure via FA, we in CFA suppose a factor structure and use FA
#   to verify it. 
# - CFA is a special case of a more general family of approaches called structural 
#   equation modeling (SEM). SEM is a methodology for representing, estimating, 
#   and testing a network of relationships between measured variables and latent 
#   constructs.
# - We're going to utilize blavaan, a Bayesian analogue to the most popular R
#   SEM package lavaan.

library(lavaan)
library(blavaan)

# - blavaan uses the lavaan modeling language which is in turn similar to 
#   R formulas. In the background, blavaan now also uses Stan.
# - We'll only run it for 1000 data points, because it runs for a long time.

CFA_model <- ' 
 extraversion =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
 openness =~ O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
 conscientiousness  =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
 agreeableness  =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
 neuroticism =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10'
fit <- bcfa(CFA_model, data = dat[1:1000,], n.chains = 1, sample = 200, burnin = 200)
summary(fit)
ppc <- ppmc(fit, fit.measures = c("rmsea"))
summary(ppc)


# - Let's also run lavaan on the entire dataset, for comparison.
# - A nice tutorial on CFA in lavaan: https://stats.idre.ucla.edu/r/seminars/rcfa/
fit <- cfa(CFA_model, data = dat)
summary(fit)
fitMeasures(fit, c("rmsea"))

# - Interpreting CFA results is not trivial, unless the model is extremely poor
#   or close to perfect. As it is true for all modeling tasks - we can't really
#   interpret model quality in the absolute sense, only relative to the performance
#   of alternative models.
# - If we do have to look at a single number, Root Mean Square Error of 
#   Approximation (RMSEA) is one of the most commonly used errors. It measures how
#   well the hypothesized model is relative to the population covariance.
#   Rule of thumb: RMSEA < 0.01 is a really good fit, < 0.08 is decent, 0.10 poor.
#   This model is a decent fit for our data.
# - There are also a couple of packages that visualize SEM models produced by lavaan.

library(semPlot)
semPaths(fit)