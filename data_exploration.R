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


top <- subset(cards, top30Per == 1)
top <- subset(top, types != 'Land')
mean(top$manaValue)
mean(top$printings)
mean(top$combosCnt)
table(top$types)
table(top$rarity)
table(top$target)


bottom <- subset(cards, top30Per == 0)
bottom <- subset(bottom, types != 'Land')
mean(bottom$manaValue)
mean(bottom$printings)
mean(bottom$combosCnt)
table(bottom$types)

# plots 
counts_type <- table(cards$top30Per, cards$types)

barplot(counts_type, main="Number of Card types distributed among the top 30% and below",
        xlab="Card Type", col=c("light blue", "lavender"),
        legend = rownames(counts_type))

counts_rarity <- table(cards$top30Per, cards$rarity)

barplot(counts_rarity, main="Card Rarities distributed among the top 30% and below",
        xlab="Rarity", col=c("light blue", "lavender"),
        legend = rownames(counts_rarity))

boxplot(convertedManaCost ~ top30Per, data=cards, col=(c("gold", "darkgreen")))

types <- as.factor(cards$types)


chisq.test(cards$top30Per, cards$rarity)
chisq.test(cards$top30Per, cards$colors)
chisq.test(cards$top30Per, as.factor(cards$types))
chisq.test(cards$top30Per, cards$battlefield, correct=FALSE)
