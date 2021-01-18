library(readxl)
library(dplyr)
library(ggplot2)

# Import file
# MIA vs BOS - Game 5 - Combined Data V3_SentimentAnalysis.xlsx
data <- read_excel(file.choose())
View(data)


# Tweet Count by Time Frame
nrow(data) # 12145
before_count = nrow(filter(data, `Time Frame` == "Before Game")) # 2293
q1_count = nrow(filter(data, `Time Frame` == "Q1")) # 1656
q2_count = nrow(filter(data, `Time Frame` == "Q2")) # 1566
half_count = nrow(filter(data, `Time Frame` == "Halftime")) # 516
q3_count = nrow(filter(data, `Time Frame` == "Q3")) # 1529
q4_count = nrow(filter(data, `Time Frame` == "Q4")) # 1997
post_count = nrow(filter(data, `Time Frame` == "Post Game")) # 2588
tweet_count <- c(before_count, q1_count, q2_count, half_count, q3_count, q4_count, post_count)
barplot(tweet_count,
        main = "Tweet Count by Time Frame",
        xlab = "Time Frame",
        ylab = "Counts of Tweets",
        names.arg = c("Before", "Q1", "Q2", "Half", "Q3", "Q4", "Post"),
        col = "darkgrey",
        horiz = FALSE)


# Tweet Count by Category
mia_count = nrow(filter(data, Category == "Heat")) # 4046
bos_count = nrow(filter(data, Category == "Celtics")) # 4565
mixed_count = nrow(filter(data, Category == "Mixed")) # 3534
category_count <- c(mia_count, bos_count, mixed_count)
barplot(category_count,
        main = "Tweet Count by Category",
        xlab = "Category",
        ylab = "Counts of Tweets",
        names.arg = c("Heat", "Celtics", "Mixed"),
        col = "darkgrey",
        horiz = FALSE)


# Add column for hashtag count 
data$hashtags_count <- count.fields(textConnection(data$hashtags), sep = ",")
data$hashtags_count <- as.numeric(data$hashtags_count)


# Add column for binary value for categorical columns
data$verified_bi <- as.integer(as.logical(data$is_verified_user))
data$retweeted_bi <- ifelse(data$`Retweeted?`=="Yes", "1", "0")
data$retweeted_bi <- as.integer(data$retweeted_bi)

# Add columns to see if MIA/BOS ahead or behind
data$mia_ahead <- ifelse(data$`MIA Score Differential`>0, "1", "0")
data$bos_ahead <- ifelse(data$`BOS Score Differential`>0, "1", "0")


# Add column indicating positive/neutral/negative sentiment
# Add column for binary value for positive/negative sentiment
data$sentiment = ifelse(data$negative == data$positive, "Neutral", ifelse(data$negative>0, "Negative", "Positive"))

positive_count = nrow(filter(data, sentiment == "Positive")) # 2437
neutral_count = nrow(filter(data, sentiment == "Neutral")) # 8283
negative_count = nrow(filter(data, sentiment == "Negative")) # 1425
sentiment_count <- c(positive_count, neutral_count, negative_count)
barplot(sentiment_count,
        main = "Tweet Count by Sentiment",
        xlab = "Sentiment",
        ylab = "Counts of Tweets",
        names.arg = c("Positive", "Neutral", "Negative"),
        col = "darkgrey",
        horiz = FALSE)

data_emotional = filter(data, sentiment == "Positive" | sentiment == "Negative")
data_emotional$sentiment_bi = ifelse(data_emotional$sentiment == "Positive", 1, 0)


# Add columns for binary value for english/spanish
data$english_bi = ifelse(data$`Language - Code` == "en", 1, 0)
data$spanish_bi = ifelse(data$`Language - Code` == "es", 1, 0)

# Correlation
cor(data$hashtags_count, data$retweet_count, use = "complete.obs") # -0.149
cor(data$hashtags_count, data$retweeted_bi, use = "complete.obs") # -0.04
cor(data$hashtags_count, data$`MIA Score Differential`, use = "complete.obs") # 0.116
cor(data$hashtags_count, abs(data$`MIA Score Differential`), use = "complete.obs") # -0.083
cor(data_emotional$hashtags_count, data_emotional$sentiment_bi, use = "complete.obs") # -0.047
cor(data$retweet_count, data$user_follower_count) # -0.016
cor(data$retweet_count, data$user_friend_count) # 0.001


# Scatterplot
data_game = filter(data, `Time Frame` == "Q1" | `Time Frame` == "Q2" | `Time Frame` == "Q3" | `Time Frame` == "Q4")
plot(data_game$Time, data_game$`MIA Score Differential`)

ggplot(data, aes(x=hashtags_count, y=retweet_count)) +
  geom_point() +
  ggtitle("Hashtag counts / retweet counts")

ggplot(data, aes(x=abs(`MIA Score Differential`), y=retweet_count)) +
  geom_point() +
  ggtitle("Score difference / retweet counts")


# Run t test (grouping factor must have exactly 2 levels)
?t.test

# t.test(x~y)
t.test(hashtags_count ~ `Retweeted?`, data, var.equal = TRUE) 
# p-value = 0.000137
t.test(negative ~ `Retweeted?`, data, var.equal = TRUE) 
# p-value < 2.2e-16
t.test(positive ~ `Retweeted?`, data, var.equal = TRUE) 
# p-value < 2.2e-16
t.test(hashtags_count ~ sentiment, data_emotional, var.equal = TRUE) 
# p-value = 0.01252
t.test(retweet_count ~ sentiment, data_emotional, var.equal = TRUE) 
# p-value < 2.2e-16

t.test(verified_bi ~ `Retweeted?`, data, var.equal = TRUE) 
# p-value = 3.584e-10
t.test(user_follower_count ~ `Retweeted?`, data, var.equal = TRUE)
# p-value = 7.2e-06
t.test(user_friend_count ~ `Retweeted?`, data, var.equal = TRUE)
# p-value = 0.0003896



##
data_mia = filter(data, Category == "Heat")
data_bos = filter(data, Category == "Celtics")
data_mix = filter(data, Category == "Mixed")


# Run linear regression model
# Response variable: retweet_count
# lm(y~x)
linear_mod1 <- lm(data$retweet_count ~ 
                    data$hashtags_count + data$verified_bi + 
                    data$user_follower_count + data$user_friend_count +
                    data$negative + data$positive +
                    data$`MIA Score Differential`)
summary(linear_mod1)

# For Miami Heat
linear_mod2 <- lm(data_mia$retweet_count ~ 
                    data_mia$hashtags_count + data_mia$verified_bi + 
                    data_mia$user_follower_count + data_mia$user_friend_count + 
                    data_mia$negative + data_mia$positive +
                    data_mia$`MIA Score Differential`)
summary(linear_mod2)

# For Boston Celtics
linear_mod3 <- lm(data_bos$retweet_count ~ 
                    data_bos$hashtags_count + data_bos$verified_bi + 
                    data_bos$user_follower_count + data_bos$user_friend_count +
                    data_bos$negative + data_bos$positive +
                    data_bos$`MIA Score Differential`
                    )
summary(linear_mod3)

# For Mixed
linear_mod4 <- lm(data_mix$retweet_count ~ 
                    data_mix$hashtags_count + data_mix$verified_bi + 
                    data_mix$user_follower_count + data_mix$user_friend_count +
                    data_mix$negative + data_mix$positive +
                    data_mix$`MIA Score Differential`)
summary(linear_mod4)


# Run logistic regression model
# glm(y~x)

# Response variable: sentiment (positive=1; negative=0)
logistic_mod1 <- glm(sentiment_bi ~ hashtags_count + verified_bi + user_follower_count + user_friend_count,
                      data = data_emotional,
                      family = "binomial")
summary(logistic_mod1)

# For Miami Heat
logistic_mod2 <- glm(sentiment_bi ~ hashtags_count + verified_bi,
                     data = filter(data_emotional, Category == "Heat"),
                     family = "binomial")
summary(logistic_mod2)

# For Boston Celtics
logistic_mod3 <- glm(sentiment_bi ~ hashtags_count + verified_bi,
                     data = filter(data_emotional, Category == "Celtics"),
                     family = "binomial") 
summary(logistic_mod3)

# For Mixed
logistic_mod4 <- glm(sentiment_bi ~ hashtags_count + verified_bi,
                     data = filter(data_emotional, Category == "Mixed"),
                     family = "binomial") 
summary(logistic_mod4)
# NONE

# Response variable: retweeted_bi (yes=1; no=0)

logistic_mod5 <- glm(retweeted_bi ~ hashtags_count + verified_bi + user_follower_count + user_friend_count + negative + positive + `MIA Score Differential`,
                     data = data,
                     family = "binomial") 
summary(logistic_mod5)

# For Miami Heat
logistic_mod6 <- glm(retweeted_bi ~ 
                       hashtags_count + verified_bi + user_follower_count + user_friend_count + negative + positive + `MIA Score Differential`,
                     data = filter(data, Category == "Heat"),
                     family = "binomial") 
summary(logistic_mod6)

# For Boston Celtics
logistic_mod7 <- glm(retweeted_bi ~
                       hashtags_count + verified_bi + user_follower_count + user_friend_count + negative + positive + `MIA Score Differential`,
                     data = filter(data, Category == "Celtics"),
                     family = "binomial") 
summary(logistic_mod7)

# For Mixed
logistic_mod8 <- glm(retweeted_bi ~ 
                       hashtags_count + verified_bi + user_follower_count + user_friend_count + negative + positive  + `MIA Score Differential`,
                     data = filter(data, Category == "Mixed"),
                     family = "binomial")
summary(logistic_mod8)




#######################
# Add language factor #
#######################


# English or not?

t.test(retweet_count ~ english_bi, data, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweet_count ~ english_bi, data_mia, var.equal = TRUE) # p-value = 5.571e-14
t.test(retweet_count ~ english_bi, data_bos, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweet_count ~ english_bi, data_mix, var.equal = TRUE) # p-value = 5.463e-10

t.test(retweeted_bi ~ english_bi, data, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweeted_bi ~ english_bi, data_mia, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweeted_bi ~ english_bi, data_bos, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweeted_bi ~ english_bi, data_mix, var.equal = TRUE) # p-value = 4.989e-06

# Spanish or not?

t.test(retweet_count ~ spanish_bi, data, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweet_count ~ spanish_bi, data_mia, var.equal = TRUE) # p-value = 0.0003094
t.test(retweet_count ~ spanish_bi, data_bos, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweet_count ~ spanish_bi, data_mix, var.equal = TRUE) # p-value = 0.0657

t.test(retweeted_bi ~ spanish_bi, data, var.equal = TRUE) # p-value = 0.0003167
t.test(retweeted_bi ~ spanish_bi, data_mia, var.equal = TRUE) # p-value <= 5.344e-06
t.test(retweeted_bi ~ spanish_bi, data_bos, var.equal = TRUE) # p-value < 2.2e-16
t.test(retweeted_bi ~ spanish_bi, data_mix, var.equal = TRUE) # p-value < 2.2e-16


nrow(data) # 12145
nrow(filter(data, `Language - Code` == "es")) # 651
651/12145 # In whole dataset, 5.4% tweets are in spanish

nrow(filter(data, Category == "Heat")) # 4046
nrow(filter(data, Category == "Heat" & `Language - Code` == "es")) # 185
185/4046 # For Miamia Heat, 4.6% tweets are in spanish

nrow(filter(data, Category == "Celtics")) # 4565
nrow(filter(data, Category == "Celtics" & `Language - Code` == "es")) # 252
252/4565 # For Boston Celtics, 5.5% tweets are in spanish

nrow(filter(data, Category == "Mixed")) # 3534
nrow(filter(data, Category == "Mixed" & `Language - Code` == "es")) # 214
214/3534 # For Mixed, 6.1% tweets are in spanish 




#######################
#     Conclusion      #
#######################

# Recommendation
# 1. In order to be retweeted, use less hashtags
# 2. In order to be retweeted, post more positive tweets
# 3. English tweets are more popular
# 4. When Boston Celtics is ahead, more people retweeted (does it mean Celtics fans are more active on Twitter?)



