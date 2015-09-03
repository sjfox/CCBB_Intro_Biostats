#Let's Make a Deal Simulation
##Original Formulation
Suppose you're on a game show, and you're given the choice of three doors. Behind one door is a car; behind the others, goats. 

You pick a door, say No. 1. Then the host, who knows what's behind the doors, opens another door, say No. 3, which has a goat. He then says to you, "Do you want to switch your door to No. 2?" 

**Is it to your advantage to switch your choice?**

It's a crazy world--you could end up on an American game show from the 1970s tomorrow. So we'll coach you on strategy. Using R, of course.

##Rules of the Game
1. The host must always open a door that was not picked by the contestant.
2. The host must always open a door to reveal a goat and never the car.
3. The host must always offer the chance to switch between the originally chosen door and the remaining closed door.

More information on the problem can be found [here](https://en.wikipedia.org/wiki/Monty_Hall_problem).

## Some necessary functions
This chunk contains code we haven't talked about in class, so you can simply run this chunk for now (It needs to be run for the rest to work). Only come back to it if you have time in the end or are interested later on.
```{r, echo=FALSE}
rm(list=ls())
set.seed(12345)
openDoor <- function(prizeDoor, firstChoice, possibleDoors=c(1,2,3)){
  # Returns the door that gets opened
  if(prizeDoor==firstChoice){
    return(sample(x = possibleDoors[-prizeDoor], size = 1, replace = T))
  } else {
    return(possibleDoors[-c(prizeDoor, firstChoice)])
  }
}
openDoors <- Vectorize(openDoor)

chooseCorrect <- function(prizeDoor, myFinalChoice){
  ifelse(prizeDoor==myFinalChoice, TRUE, FALSE)
}

switchSingle <- function(firstChoice, openedDoor, possibleDoors=c(1,2,3)){
  possibleDoors[-c(firstChoice, openedDoor)]
}

switchStrategy <- Vectorize(switchSingle)

montyHallSim <- function(strategy="stay"){
  possibleDoors <- c(1,2,3)
  prize <- sample(x = possibleDoors, size = 1, replace=T)
  choice <- sample(x = possibleDoors, size = 1, replace=T)
  openDoor <- openDoor(prizeDoor = prize, firstChoice = choice, possibleDoors=possibleDoors)
  if(strategy=="stay"){
    finalChoice <- choice
  } else if(strategy=="switch"){
    finalChoice <- switchSingle(firstChoice = choice, openedDoor = openDoor, possibleDoors = possibleDoors)
  } else{
    print("Unrecognized strategy")
  }
  correct <- chooseCorrect(prizeDoor = prize, myFinalChoice = finalChoice)
  return(c(prizeLocation = prize, firstChoice = choice, openDoor=openDoor, finalChoice=finalChoice, win=correct))
}
```

##Simulating a Single Game
First, create a vector of all the possible door choices:
```{r}
possibleDoors <- c(1, 2, 3)
```

Now let's randomly sample from this list to decide which door has the prize.

The `sample` function takes in a vector of items, and will randomly choose one of them to return,
as we have set the argument `size = 1`.
```{r}
prizeLocation <- sample(x = possibleDoors, size = 1)
# type ?sample for more info
```
If you wanted to select more than one to return, then you also need to specify whether you want to sample with replacement (argument `replace=TRUE`).

Let's also assume our first choice is random amongst the doors:
```{r}
firstChoice <- sample(x = possibleDoors, size = 1)

print(c(prize = prizeLocation, 
        firstChoice = firstChoice))
```

Great, now we have our prize and our first door choice! 

Now we need to open a door according to the rules of the game. I've created a function to do this called `openDoor`, which takes two integer arguments: the location of the prize and your first door choice. If you are interested in this function, you can look at the first code chunk.
```{r}
openedDoor <- openDoor(prizeDoor = prizeLocation, firstChoice = firstChoice)
```

Now, we need our strategy. Let's say that we stay with our first door choice
```{r}
finalChoice <- firstChoice
```

Consider how you might code a switch to your door based on the rules of the game. If you have time, try it out here.
```
# your code goes here
```

Awesome, now we have a single full simulation of the game!
```{r}
print(c(prize = prizeLocation, 
        firstChoice = firstChoice, 
        openDoor=openedDoor, 
        finalChoice=finalChoice))

# Check to see if we won
finalChoice==prizeLocation
```

Now if we won (lost) does that mean that is the right (wrong) strategy? No, of course not. Run the code again and see what happens. Was the outcome the same? Maybe, or maybe not, but either way it's obvious we need to run more replicates and this is not an efficient way to do so.

##Simulating Many Games
Now I've coded a function called `montyHallSim()` that does this process for us. The only argument it takes in is a strategy (either "stay" or "switch"). If you have time or the desire, take a look at it in the first code chunk. 
So what does the function output?
```{r}
strategy <- "switch"
# strategy <- "stay"
montyHallSim(strategy)
```

You can see it outputs the same game result as we had previously, but we can do it with one line: all the previous commands have been coded into a single function. I've also added a "win" number, which equals "1"" if we won, or "0" if we lost. 

Now let's run this a few times and store the output in a matrix. First, we set up some variables governing the simulations:
```{r}
numReps <- 10
strategy <- "switch" # or "stay"
```

Now let's setup a matrix to store the results. We don't want any row names, as these just correspond to simulation number:
```{r}
rowNames <- NULL

# Column names need to be the same exact order as our simulation output
colNames <- c("prizeLocation", "firstChoice", "openDoor", "finalChoice", "win")
```

Now let's make our matrix of the correct dimensions by specifying the number of rows (`nrow`) which is the number of replicates we want to run (`numReps`), and the number of columns (`ncol`) which is the same length as our column names. Argument `dimnames` gives the names we specified previously to the rows and columns. We first fill the matrix in with missing values by specifying `data=NA`, which correspond to no data (see `?matrix` for more information on this process).
```{r}
montyData <- matrix(data = NA, nrow = numReps, ncol = length(colNames), 
                    dimnames = list(rowNames, colNames))
```

Now we'll run a loop for the specified number of simulations (`numReps` which we defined a few code blocks back), and store the result of each simulation in the correct row.
```{r}
for(rep in seq(1,numReps)){
  montyData[rep,] <- montyHallSim(strategy)
}

head(montyData)
```

Okay, so what can we get from this? First, let's see if everything kind of looks correct. We should have roughly equal number of prize door selections, and first choices (since they were both random).
```{r}
# The table function prints occurence counts in a column
table(montyData[,"prizeLocation"])

table(montyData[,"firstChoice"])
```
Hm, that may not look correct depending on how many times you ran your simulation. Go back and change `numReps` to a larger number (1000, 10000, etc.), rerun the simulations, and then look at these tables. Do they look closer to your expectation? 

Just as a secondary sanity check, we also know that we should never have opened a door that has the prize, or was our first selection, so let's check that as well.
```{r}
# Logical values FALSE evaluate to 0 and TRUE evaluate to 1,
# so if everything is correct, the sum should equal 0
# meaning there are no instances in which the doors are the same
sum(montyData[,'prizeLocation'] == montyData[,'openDoor'])
sum(montyData[,'firstChoice'] == montyData[,'openDoor'])
```

Great, things look to be working! Okay, using the technique from that last step, try coding for yourself how you might calculate the percent of times your final door was the correct door. 
```{r}
# your code goes here
```

Okay, now run everything again for the other strategy. Which was better?

## Advanced Questions: 
* Add in a strategy function that after the door is opened, randomly chooses between the two remaining doors as its selection. 
* What should the probability of this winning be? Do your simulated calculations match? 
* What if there were four doors? Can you code up a way to choose between the remaining 3 if only one door was opened, and does the winning strategy stay the same?

```{r}
prizeLocation <- sample(x = possibleDoors, size = numReps, replace = T)
firstChoice <- sample(x = possibleDoors, size = numReps, replace = T)

#Vector holding which door was opened
#The revealed door is the door not either the car location or your first choice
openedDoor <- openDoors(prizeLocation, firstChoice)

montyData <- data.frame(prizeLocation, firstChoice, openedDoor)
```

Great, now what do our data look like?
```{r}
head(montyData)
```

```{r}
#Always stays with the first door made
montyData$alwaysStay <- firstChoice

montyData$alwaysSwitch <- switchStrategy(firstChoice = montyData$firstChoice, openedDoor = montyData$openedDoor)

chooseCorrect(prizeDoor = montyData$prizeLocation, myFinalChoice = montyData$alwaysSwitch)
```
