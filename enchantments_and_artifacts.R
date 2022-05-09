library(ggplot2)
library(MASS)
library(dplyr)
library(tidyr)
library(tree)
library(ggcorrplot)

# read in the dataset
cards = read.csv("cards_cleanish.csv")

# make top 30% column
cards$top30Per <- "2"

# move top30Per columb to 0 index
cards <- cards %>% relocate(top30Per, .before = edhrecRank)

# take a subset of the top 10000 ranked cards
cards <- subset(cards, edhrecRank <= 10000)


# Only keep cards with main rarities (removing "special")
card_rarities = c('common', 'uncommon', 'rare', 'mythic')
cards <- cards[cards$rarity %in% card_rarities,]

# Only keep the card types that are most common. In order to simplify analysis we do not use combinations of card types such as "Artifact Creature" or "Artifact Land"
card_types = c('Creature', 'Instant', 'Artifact', 'Land', 'Sorcery', 'Enchantment')
cards <- cards[cards$types %in% card_types,]


# only keep the mono colored, two colored, and three colored cards 
card_colors = c('W', 'U', 'B', 'R', 'G', 
                'R,U', 'G,R', 'B,R', 'R,W', 'G,W', 'G,U', 'B,W', 'U,W', 'B,U', 'B,G',
                'G,W,U', 'W,U,B', 'U,B,R', 'B,R,G', 'R,G,W', 'W,B,G', 'U,R,W', 'B,G,U', 'R,W,B', 'G,U,R', '')

cards <- cards[cards$colors %in% card_colors,]


cards$colors <- paste0("(", cards$colors, ")")


max_rank = max(cards["edhrecRank"])
# if a card is in the top 30% on edhrec assign a value of 1
cards$top30Per <- ifelse(cards$edhrecRank > (0.3*max_rank), 0, 1)

cards$top30Per <- as.factor(cards$top30Per)
cards$hasFoil <- as.factor(cards$hasFoil)
cards$hasNonFoil <- as.factor(cards$hasNonFoil)
cards$isReserved <- as.factor(cards$isReserved)
cards$isTextless <- as.factor(cards$isTextless)
cards$colors <- as.factor(cards$colors)
cards$toughness <- as.integer(cards$toughness)
cards$power <- as.integer(cards$power)
cards$combosCnt <- as.integer(cards$combosCnt)

cards$treasure <- as.factor(cards$treasure)
cards$life <- as.factor(cards$life)
cards$draw <- as.factor(cards$draw)
cards$battlefield <- as.factor(cards$battlefield)
cards$sacrifice <- as.factor(cards$sacrifice)
cards$search <- as.factor(cards$search)
cards$turn <- as.factor(cards$turn)
cards$discard <- as.factor(cards$discard)
cards$flying <- as.factor(cards$flying)
cards$infect <- as.factor(cards$infect)
cards$trample <- as.factor(cards$trample)
cards$haste <- as.factor(cards$haste)
cards$buyback <- as.factor(cards$buyback)
cards$vigilance <- as.factor(cards$vigilance)
cards$destroy <- as.factor(cards$destroy)
cards$counter <- as.factor(cards$counter)
cards$target <- as.factor(cards$target)
cards$upkeep <- as.factor(cards$upkeep)
cards$win <- as.factor(cards$win)
cards$damage <- as.factor(cards$damage)
cards$all <- as.factor(cards$all)
cards$rarity <- as.factor(cards$rarity)


table(cards$top30Per)







# Create creature subset
# Only keep the card types that are most common. In order to simplify analysis we do not use combinations of card types such as "Artifact Creature" or "Artifact Land"
#card_types = c('Creature')
card_types = c('Enchantment', 'Artifact')
enchant <- cards[cards$types %in% card_types,]
enchant$power <- as.integer(enchant$power)
enchant$toughness <- as.integer(enchant$toughness)



# Create training and testing dataset for enchant
set.seed(123)
idx.train <- sample(1:nrow(enchant), size = 0.8 * nrow(enchant))
train.enchant <- enchant[idx.train,]
test.enchant <- enchant[-idx.train,]

print("Training set for Enchantments and Artifacts")
table(train.enchant$top30Per)

print("Test set for Enchantments and Artifacts")
table(test.enchant$top30Per)



# Initial logistic regression model on the training set
glm1 <- glm(top30Per ~ printings + rarity  + combosCnt , 
            data=train.enchant, 
            family=binomial)
summary(glm1)

# make predictions based on the creature dataset
probs <- predict(glm1, test.enchant, type='response')
creature.pred <- ifelse(probs > 0.5, 1, 0)
caret::confusionMatrix(factor(creature.pred), test.enchant$top30Per)




# Stepwise selection for enchant and logistic regression
m1 <- glm(top30Per ~ rarity + manaValue + counter + hasFoil + printings + combosCnt + treasure  + 
            search + turn + discard + flying  + trample + vigilance + destroy + life + 
            target + upkeep + win + damage + all + draw,
          data=train.enchant, family=binomial)
step <- stepAIC(m1, direction = "both", trace = FALSE)
summary(step)


probs <- predict(step, test.enchant,type="response")
creature.pred <- ifelse(probs > 0.5, 1, 0)
caret::confusionMatrix(factor(creature.pred), test.enchant$top30Per, positive = '1')









# decision trees

# Grow a tree
tree.cards = tree(top30Per ~ . -edhrecRank - power - toughness,
                  data=enchant)
#tree.cards = tree(top30Per ~ types + manaValue + power + toughness + rarity + 
#                    printings + hasFoil + subtypes + colors, 
#                  data=cards)
summary(tree.cards)



plot(tree.cards)
text(tree.cards, cex = 0.7,pretty=1)



#Estimate test error rate using validation set approach -->
set.seed(420)

# Create training and test sets
train = sample(1:nrow(enchant), size = 0.80 * nrow(enchant))
cards.train = enchant[train, ]
cards.test = enchant[-train, ]

#train.omit <- na.omit(cards.train) 
#test.omit <- na.omit(cards.test) 

# Grow a tree using the training set 
tree2.cards = tree(top30Per ~ . -edhrecRank-power-toughness, data=cards.train)


# Get predictions on the test set
preds = predict(tree2.cards, newdata = cards.test, type = "class")

#Compute the confusion matrix 
caret::confusionMatrix(preds, cards.test$top30Per, positive='1')

plot(tree2.cards)
text(tree2.cards, cex = 0.8,pretty=1)


