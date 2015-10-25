# Exploring Ingredients Data Set
jsalzwedel  
`r format(Sys.Date())`  

Let's take a look at the Kaggle "What's cooking" data set.  The goal of the
competition is to predict the cuisine of a recipe based on the recipe's 
ingredients.

# Exploratory analysis

## Read in the data

The training and test files are in the json format.  [This site](http://rstudio-pubs-static.s3.amazonaws.com/31702_9c22e3d1a0c44968a4a1f9656f1800ab.html) makes a pretty
strong argument that the jsonlite library is generally the best package to use
for parsing json data.



```r
library(jsonlite)
file <- "train.json"
trainFromJSON <- fromJSON(txt=file)
```

The dplyr::tbl package makes viewing data much more convenient.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
options(dplyr.width = Inf)
train <- tbl_df(trainFromJSON)
```

## Preliminary look

Let's take a quick look at the data.


```r
head(train,2)
```

```
## Source: local data frame [2 x 3]
## 
##      id     cuisine ingredients
## 1 10259       greek    <chr[9]>
## 2 25693 southern_us   <chr[11]>
```

We have three columns: ID of the recipe, type of cuisine, and list of
ingredients.  The ingredients entry for a given recipe is a list of character
vectors, one for each ingredient.  We'll have to split those apart later.

What are the unique cuisine types?


```r
unique(train$cuisine)
```

```
##  [1] "greek"        "southern_us"  "filipino"     "indian"      
##  [5] "jamaican"     "spanish"      "italian"      "mexican"     
##  [9] "chinese"      "british"      "thai"         "vietnamese"  
## [13] "cajun_creole" "brazilian"    "french"       "japanese"    
## [17] "irish"        "korean"       "moroccan"     "russian"
```

How many times does each cuisine appear?


```r
cuisineFrequency <- count(train, cuisine, sort = TRUE)
```


# Analysis of ingredient correlations

## Strategy

We want to look at each ingredient and see how often it is used in a given
type of cuisine.  There are many ways to use this information.  The simplest
might be to ascribe a single cuisine to each ingredient based on where it is
used most.  When we want to make predictions, each ingredient in a recipe
will weight that recipe towards a certain cuisine.  We can then then add up the
weights to see which cuisine is most favored.  In essence, each ingredient will
vote for a certain cuisine type.

Our analysis strategy will be as follows:

- Make a new data table with a row for each ingredient from each recipe.  We'll
keep the cuisine and recipe ID as additional columns.  

- For each ingredient, see where it is most often used, and associate it with
that type of cuisine.  Here we may need to be careful to not be biased by the raw
number of recipes for any given cuisine. For example, cabbage might be used in 
10/100 French recipes, but in 5/5 Korean recipes.

- Make a new data table for the test set with a row for each recipe.

- Use the previously determined ingredient weights to determine which cuisine 
has a plurality among the ingredients.


## Reformatting the data

Let's make a new data table for the ingredients.  First, unpack the list of
ingredients.  Also, keep track of how many ingredients belong to each recipe.

```r
ingredientList <- unlist(train$ingredients)
nlengths <- sapply(train$ingredients, length)
mean(nlengths)
```

```
## [1] 10.76771
```
The averge recipe has about 11 ingredients.

Now make the data table


```r
ingredients <- data.frame(id = rep(train$id, nlengths),
                        cuisine = rep(train$cuisine, nlengths),
                        ingredient = ingredientList)
ingredients <- tbl_df(ingredients)
```



## Looking at the ingredients

Let's get a list of unique ingredients.


```r
uniqueIng <- unique(ingredients$ingredient)
```

Let's tabulate cuisines by ingredient.


```r
occurances <- count(ingredients, ingredient, cuisine, sort=TRUE)
print(occurances, n=50)
```

```
## Source: local data frame [29,179 x 3]
## Groups: ingredient
## 
##                                                  ingredient      cuisine
## 1                           (10 oz.) frozen chopped spinach      italian
## 2  (10 oz.) frozen chopped spinach, thawed and squeezed dry        greek
## 3  (10 oz.) frozen chopped spinach, thawed and squeezed dry      mexican
## 4                                 (14.5 oz.) diced tomatoes      mexican
## 5                                 (14.5 oz.) diced tomatoes      italian
## 6                         (14 oz.) sweetened condensed milk       indian
## 7                         (14 oz.) sweetened condensed milk      mexican
## 8                                    (15 oz.) refried beans      mexican
## 9                                     1% low-fat buttermilk        irish
## 10                                    1% low-fat buttermilk      italian
## 11                                    1% low-fat buttermilk  southern_us
## 12                                1% low-fat chocolate milk       french
## 13                                1% low-fat chocolate milk  southern_us
## 14                                1% low-fat cottage cheese      italian
## 15                                1% low-fat cottage cheese       french
## 16                                1% low-fat cottage cheese  southern_us
## 17                                          1% low-fat milk      italian
## 18                                          1% low-fat milk       french
## 19                                          1% low-fat milk  southern_us
## 20                                          1% low-fat milk        irish
## 21                                          1% low-fat milk      mexican
## 22                                          1% low-fat milk       indian
## 23                                          1% low-fat milk      spanish
## 24                                          1% low-fat milk        greek
## 25                                          1% low-fat milk cajun_creole
## 26                                          1% low-fat milk    brazilian
## 27                                          1% low-fat milk     japanese
## 28                                          1% low-fat milk     moroccan
## 29                                          1% low-fat milk      russian
## 30          2 1/2 to 3 lb. chicken, cut into serving pieces      mexican
## 31                            25% less sodium chicken broth      italian
## 32                            25% less sodium chicken broth cajun_creole
## 33                                 2% low fat cheddar chees      mexican
## 34                                2% low-fat cottage cheese      italian
## 35                                2% low-fat cottage cheese        greek
## 36                                   2% lowfat greek yogurt        greek
## 37                       2% milk shredded mozzarella cheese      italian
## 38                                      2% reduced-fat milk  southern_us
## 39                                      2% reduced-fat milk       french
## 40                                      2% reduced-fat milk      italian
## 41                                      2% reduced-fat milk      mexican
## 42                                      2% reduced-fat milk cajun_creole
## 43                                      2% reduced-fat milk      british
## 44                                      2% reduced-fat milk        irish
## 45                                      2% reduced-fat milk    brazilian
## 46                                      2% reduced-fat milk        greek
## 47                                      2% reduced-fat milk       indian
## 48                                      2% reduced-fat milk     japanese
## 49                                      2% reduced-fat milk      russian
## 50                          33% less sodium cooked deli ham      italian
## ..                                                      ...          ...
##     n
## 1   3
## 2   1
## 3   1
## 4   2
## 5   1
## 6   1
## 7   1
## 8   3
## 9   2
## 10  1
## 11  1
## 12  1
## 13  1
## 14 10
## 15  1
## 16  1
## 17 68
## 18 51
## 19 18
## 20 15
## 21 13
## 22  9
## 23  7
## 24  6
## 25  2
## 26  1
## 27  1
## 28  1
## 29  1
## 30  2
## 31  2
## 32  1
## 33  1
## 34  5
## 35  1
## 36  2
## 37  1
## 38 21
## 39 12
## 40 11
## 41 11
## 42  5
## 43  3
## 44  3
## 45  1
## 46  1
## 47  1
## 48  1
## 49  1
## 50  1
## .. ..
```

For a given ingredient, which cuisine appears the most often?  When we try to 
predict cuisines, that ingredient will "vote" for its associated cuisine


```r
votes <- occurances %>% group_by(ingredient) %>% top_n(n=1, wt = n)
votes <- rename(votes, cuisine.vote = cuisine, n.votes = n)
```

Note that if an ingredient has two cuisines that tie for max occurances, it will
get a row for each of them.  For now, I guess that means that that lucky (and
indecisive) ingredient gets two votes!

## Voting with the training set

Now that we have a preferred cuisine for each ingredient, let's take the
training recipes and let their ingredients vote on which kind of cuisine they
probably are.  Let's hope for a reasonable success rate!


```r
trainVotes <- inner_join(ingredients, votes)
```

```
## Joining by: "ingredient"
```

```r
voteTally <- count(trainVotes, id, cuisine.vote, sort=TRUE)
voteWinners <- voteTally %>% group_by(id) %>% top_n(n=1, wt = n)
voteWinners <- transmute(voteWinners, cuisine.vote)
voteWinners
```

```
## Source: local data frame [46,924 x 2]
## Groups: id
## 
##    id cuisine.vote
## 1   0      italian
## 2   1      mexican
## 3   2      italian
## 4   3      chinese
## 5   4       french
## 6   6      chinese
## 7   8      italian
## 8   8  southern_us
## 9   9      italian
## 10  9  southern_us
## .. ..          ...
```

There are still some ties (Recipe 8 has four votes for italian and four for
southern_us).  We can break this tie by choosing the cuisine which appears more
often in the overall training dataset.


```r
ties <- voteWinners %>% group_by(id) %>% filter(n() > 1)
ties <- rename(ties, cuisine = cuisine.vote)
ties <- inner_join(ties, cuisineFrequency)
```

```
## Joining by: "cuisine"
```

```
## Warning: joining factor and character vector, coercing into character
## vector
```

```r
tieBreaker <- ties %>% group_by(id) %>% top_n(n=1, wt = n)
tieBreaker <- transmute(tieBreaker, cuisine.choice = cuisine)
```

Let's use this to remove the ties in voteWinners.


```r
voteWinners <- left_join(voteWinners,tieBreaker)
```

```
## Joining by: "id"
```

```r
voteWinners[is.na(voteWinners$cuisine.choice),]$cuisine.choice <- as.character(voteWinners[is.na(voteWinners$cuisine.choice),]$cuisine.vote)
voteWinners <- transmute(voteWinners, cuisine.choice)
voteFinal <- distinct(voteWinners)
```

Now we'll merge voteFinal with the training set and see how often we guess
correctly.


```r
trainingGuess <- left_join(train, voteFinal)
```

```
## Joining by: "id"
```

So how did we do?


```r
# Distinct recipes:
total <- length(trainingGuess$id)
# Recipes we got correct:
ncorrect <- length(which(trainingGuess$cuisine == trainingGuess$cuisine.choice))
```

Well, we managed to get 0.52 correct.
Hopefully that's not too bad for a first try!

## Applying solution to the test set

Let's take the scheme from above and try it out on the test set!


```r
fileTest <- "test.json"
testFromJSON <- fromJSON(txt=fileTest)
test <- tbl_df(testFromJSON)
```

Break down the recipes by ingredient:

```r
testIngredientList <- unlist(test$ingredients)
testNlengths <- sapply(test$ingredients, length)
testIngredients <- data.frame(id = rep(test$id, testNlengths),
                        ingredient = testIngredientList)
testIngredients <- tbl_df(testIngredients)
```

Let's merge our voting scheme with the ingredient list.  Inner_join will
exclude ingredients that we don't have any vote for.


```r
testVotes <- inner_join(testIngredients, votes)
```

```
## Joining by: "ingredient"
```

```
## Warning: joining factors with different levels, coercing to character
## vector
```

```r
testVoteTally <- count(testVotes, id, cuisine.vote, sort=TRUE)
testVoteWinners <- testVoteTally %>% group_by(id) %>% top_n(n=1, wt = n)
testVoteWinners <- transmute(testVoteWinners, cuisine.vote)
testVoteWinners
```

```
## Source: local data frame [11,817 x 2]
## Groups: id
## 
##    id cuisine.vote
## 1   5      italian
## 2   7      mexican
## 3  11      italian
## 4  12  southern_us
## 5  13  southern_us
## 6  17      italian
## 7  18      italian
## 8  23      italian
## 9  26      italian
## 10 28  southern_us
## .. ..          ...
```

Again, we'll remove ties.


```r
testTies <- testVoteWinners %>% group_by(id) %>% filter(n() > 1)
testTies <- rename(testTies, cuisine = cuisine.vote)
testTies <- inner_join(testTies, cuisineFrequency)
```

```
## Joining by: "cuisine"
```

```
## Warning: joining factor and character vector, coercing into character
## vector
```

```r
testTieBreaker <- testTies %>% group_by(id) %>% top_n(n=1, wt = n)
testTieBreaker <- transmute(testTieBreaker, cuisine.choice = cuisine)
testVoteWinners <- left_join(testVoteWinners, testTieBreaker)
```

```
## Joining by: "id"
```

```r
testVoteWinners[is.na(testVoteWinners$cuisine.choice),]$cuisine.choice <- 
    as.character(testVoteWinners[is.na(testVoteWinners$cuisine.choice),]$cuisine.vote)
testVoteWinners <- transmute(testVoteWinners, cuisine.choice)
```

Our prediction for each recipe's cuisine:


```r
prediction <- distinct(testVoteWinners)
prediction <- rename(prediction, cuisine = cuisine.choice)
write.csv(prediction, "prediction.csv", row.names = FALSE, quote=FALSE)
```

Well, that got us a 49% success rate.  Not too bad, and much better than 
"every recipe is Italian" prescription, which gets a 19% success rate.  In the
next section, we'll see if we can improve it by looking at two-ingredient
correlations.

# Analysis using two-ingredient correlations

Here we'll look to see if a coincidence of two ingredients in a recipe can be a
predictor of the cuisine type.  For example, black olives and feta might
usually occur together in Greek food, while black olives and refried beans would
suggest Mexican food.



