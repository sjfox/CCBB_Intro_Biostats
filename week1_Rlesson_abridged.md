This is an abridged version of a longer tutorial, which can be found at:
https://github.com/sjfox/CCBB_Intro_Biostats/blob/master/week1_Rlesson_unabridged.md

# Introduction to R

R is a high-level programming language most typically used for statistical analysis and data manipulation.

**Why (when) to use R:**
* R is free and easy, many people use it which facilitates collaboration (though the same applies to Python)
* R has a huge (and constantly growing) base of peer-contributed packages and statistical routines
* R is an excellent platform for simulation and numerical routines
* R has a lot of useful syntax for data manipulation
* R has great graphics capabilities
* R interfaces very well with C++ (via Rcpp) if you need speed
* R markdown/Sweave/Shiny make it really easy to combine text, presentations, analysis, and output in a single file
	
**Why/when not to use R (in my experience):**
* R is not great for string manipulation (you can certainly do it but it's more cumbersome than in Python)
* R has little support for symbolic algebra (use SymPy or Sage or Mathematica)
* R is relatively slow (but see above about Rcpp)
* R is not meant to create stand-alone programs (ie. a graphical interface outside the console)

This lesson is not meant to be a comprehensive survey of syntax, or of statistical routines (which would take waaaay to long); but instead is meant to introduce some important functionalities and topics.

There is a helpful (if patronizing) interactive tutorial series called [Swirl](http://swirlstats.com/). I recommend it for beginners to programming (others will find it rather annoying).

In the following code snippets, anything following `#@` is output, and anything following `#` is a comment.

### Objects and assignment

R is an object oriented programming language. Assign values to objects with `=` or `<-`.
```r
x <- 1.3
fx1 <- cos(2*pi*x)
x <- 2.6
fx2 <- sin(2*pi*x)
y <- "yakkity yak" # text string
```
Here I am assigning some values to objects. `fx1` and `fx2` store the result of a trigonometric function applied to another number, which is stored in `x`. The format for calling a function is `function_name(function_arguments)`. I'll talk about functions in more detail later.

You can see the contents of an object by typing its name.
```r
fx1
	#@ -0.309017
y
	#@ "yakkity yak"
```

Objects have attributes. The most basic attribute of an object is its class(es).
```r
class(fx2)
	#@ "numeric"

class(y)
	#@ "character"
```

You will commonly see `numeric`, `character`, and `factor` classes. Think of factors like integers (ie. having an order), but where each integer has an arbitrary name.
```r
watterson <- factor(c("Calvin", "Hobbes", "Susie"))

watterson
	#@ [1] Calvin Hobbes Susie 
	#@ Levels: Calvin Hobbes Susie
	
as.numeric(watterson) ## the integer identity is revealed
	#@ [1] 1 2 3
```

Why use classes? Classes have intrinsic functions called *methods*; ie. functions which share the same name but which vary their behavior based on the class of the object that the function is applied to.
```r
## levels() shows levels of a factor
levels(watterson)

## but doesn't work with a character vector
levels(c("Calvin", Hobbes", "Susie"))
```

### Data structures

*Vectors* are concatenations of values of the same class. Concatenations are formed with the function `c()`.
```r
myVector <- c(fx1, fx2)
myVector
	#@ [1] -0.3090170 -0.5877853
```

Note that a concatenations of values of different classes will be coerced to to a common class.
```r
myVector <- c(y, fx1, fx2)
myVector 
	#@ [1] "yakkity yak" "-0.309016994374947" "-0.587785252292473"
# notice the parentheses, indicating that the numbers have been converted to strings
```

Access elements of a vector by square brackets `[]`.
```r
myVector[3]
	#@ "-0.587785252292473"
```
Here, we access the third element of `myVector`. I'll explain indexing in more detail later.

Vectors can have names associated with elements.
```r
names(myVector) <- c("chip", "and", "dale")
myVector
	#@          chip                  and                 dale 
	#@ "yakkity yak" "-0.309016994374947" "-0.587785252292473" 

myVector["chip"] # can access element by name
	#@          chip 
	#@ "yakkity yak" 
```

*Matrices* (and more generally *arrays*) are multi-dimensional generalizations of vectors. All the elements of an array all the same value, but in the shape of a rectangle, cube, etc.
```r
myMatrix <- matrix(c(1,2,3,4,5,6), nrow=2, ncol=3)
myMatrix
	#@      [,1] [,2] [,3]
	#@ [1,]    1    3    5
	#@ [2,]    2    4    6
	
myArray <- array(c(1,2,3,4,5,6,7,8), dim=c(2,2,2)) 
myArray
	#@ , , 1
	#@ 
	#@      [,1] [,2]
	#@ [1,]    1    3
	#@ [2,]    2    4
	#@ 
	#@ , , 2
	#@ 
	#@      [,1] [,2]
	#@ [1,]    5    7
	#@ [2,]    6    8
```

Access for matrices and arrays is similar vectors, but distinguish between dimensions with commas, `[,]` (ie. rows, columns)
```r
myMatrix[2,2]
	#@ [1] 4

myArray[1,2,2]
	#@ [1] 7
```

Leaving a dimension blank will extract all elements in that dimension.
```r
myMatrix[2,]
	#@ [1] 2 4 6

myArray[,,2]
	#@      [,1] [,2]
	#@ [1,]    5    7
	#@ [2,]    6    8
```

Matrices have linear algebra operators, for example for a given matrix **X** we might want to calculate the cross product **X'X**.
```r
t(myMatrix) %*% myMatrix
	#@      [,1] [,2] [,3]
	#@ [1,]    5   11   17
	#@ [2,]   11   25   39
	#@ [3,]   17   39   61
```

The most general class of data structure is a *list*.
```r
myList <- list()
```

Lists can contain objects of arbitrary class and length. For example, a list could contain a mixture of lists and single values. Assignment/access from lists can be by name or position of the element(s).
```r
## named assignment
myList$myValue <- fx1

## numerical assignment
myList[[2]] <- fx2

myList
	#@ $myValue
	#@ [1] -0.309017
	#@ 
	#@ [[2]]
	#@ [1] -0.5877853

## access by name (if present) or number
myList$myValue
	#@ [1] -0.309017

myList[["myValue"]]
	#@ [1] -0.309017

myList[[1]]
	#@ [1] -0.309017

```
Assigning to a name which does not already exist, assigns that name in the first available numerical position in the list.

A data frame is a list of vectors, where each vector has the same length, but can have different classes.
```r
myDataFrame <- data.frame(darn=c(1,2,3,4), geez=c(5,6,7,8))
myDataFrame
	#@   darn geez
	#@ 1    1    5
	#@ 2    2    6
	#@ 3    3    7
	#@ 4    4    8

myDataFrame$blarg <- c(1,2) ## when the length of a new entry is a factor of the row number, it will automatically expands to match length
myDataFrame
	#@   darn geez blarg
	#@ 1    1    5     1
	#@ 2    2    6     2
	#@ 3    3    7     1
	#@ 4    4    8     2

## when the length of a new entry is not a factor of the row number, it fails
myDataFrame$yonk <- c(1,2,3) 
	#@  Error in `$<-.data.frame`(`*tmp*`, "yonk", value = c(1, 2, 3)) : 
	#@   replacement has 3 rows, data has 4
``` 

Access individual elements of a data frame by name or double bracketed number (extracts column), by single bracket number (extracts element), or by brackets with comma (pulls out element in row, column)
```r
myDataFrame$darn
	#@ [1] 1 2 3 4

myDataFrame[[3]] ## third column
	#@ [1] 1 2 1 2

myDataFrame[1,2] ## first element of second column
	#@ [1] 5

colnames(myDataFrame) ## show column names
	#@ [1] "darn"  "geez"  "blarg"
```

View the structure of a data container with `str()`:
```r
str(myList)
	#@ List of 2
	#@  $ myValue: num -0.309
	#@  $        : num -0.588

str(myDataFrame)
	#@ 'data.frame':	4 obs. of  3 variables:
	#@  $ darn : num  1 2 3 4
	#@  $ geez : num  5 6 7 8
	#@  $ blarg: num  1 2 1 2
```

### Reading in, writing out, and indexing data

First, we need to move into the working directory with our data. This is where files will get read in from, and written out to.
```r
## for example ... replace the following with a path specific to your computer, or use the R/RStudio GUI menu.
setwd("~/Dropbox/School/computing2015") 
```

It's most convenient to input data as a delimited table. Use the function `read.table(file, options)` to read in a delimited table as a data frame.
```r
# reads data from a comma-separated table with a header
Adoxo_count <- read.table("Adoxophyes_counts.csv", sep = ",", header = TRUE)
Adoxo_temp <- read.table("Adoxophyes_temp.csv", sep = ",", header = TRUE)
str(Adoxo_count)
	#@ 'data.frame':	2754 obs. of  3 variables:
	#@ $ day              : int  64 69 74 79 84 89 95 100 105 110 ...
	#@ $ year             : int  1961 1961 1961 1961 1961 1961 1961 1961 1961 1961 ...
	#@ $ Adoxophyes_honmai: int  0 0 1 3 0 0 4 0 0 0 ...
	
head(Adoxo_temp)
	#@   year day temperature
	#@ 1 1960   1       5.800
	#@ 2 1960   2       7.775
	#@ 3 1960   3       9.750
	#@ 4 1960   4      11.725
	#@ 5 1960   5      13.700
	#@ 6 1960   6       0.700
```
The argument `header = TRUE` indicates that the first row is column names.

The function `write.table(data, file, options)` takes `data` and writes it into `file`.
```r
# writes data out to a comma-separated table without the numeric row names
write.table(Adoxo_count, "Adoxo_count_copy.csv", sep = ",", row.names=F)  
```

##### Bracket notation for indexing
The following examples index a data frame, but the same applies to vectors/matrices/arrays.

Bracket notation is [integer position in 1st dimension, in 2nd dimension, in 3rd dimension, etc.] 

If one dimension is left blank, the entire row/column/whatever is returned. Note that R starts indexing at 1, not zero!
```r
Adoxo_temp[4,] # 4th row
	#@   year day temperature
	#@ 4 1960   4      11.725
```

A slice is a set of indices, for example the set \[ \{1,2,3,4,5\} \] is given by `1:5` in R, which can be used to extract elements 1 through 5:
```r
1:5
	#@ [1] 1 2 3 4 5

Adoxo_temp[4,1:2] # 4th row, elements from columns 1 through 2
	#@   year day
	#@ 4 1960   4
```

Slicing does not have to be contiguous: integers pieced together with `c()` are valid slices. Order matters, and elements can be duplicated.
```r
Adoxo_temp[4,c(1,3)] # 4th row, elements from columns 1 and 3
	#@   year temperature
	#@ 4 1960      11.725

subs <- c(2:1, 3, 519, 2) # 2nd through first element, 3rd element, 519th element, 2nd element again
Adoxo_temp[subs,] # replicates the rows listed in subs in the order given
	#@     year day temperature
	#@ 2   1960   2       7.775
	#@ 1   1960   1       5.800
	#@ 3   1960   3       9.750
	#@ 519 1961 153      17.300
	#@ 2.1 1960   2       7.775
```

Introducing a negative symbol before a vector of index numbers will drop these elements.
```r
## drop first column. 
## we use head because otherwise it would flood your console
head(Adoxo_temp[,-c(1)]) 
	#@   day temperature
	#@ 1   1       5.800
	#@ 2   2       7.775
	#@ 3   3       9.750
	#@ 4   4      11.725
	#@ 5   5      13.700
	#@ 6   6       0.700
``` 

Indexing is recursive.
```r
Adoxo_temp[-c(1),][2,] ## pulls out third row, because we dropped the first row, then asked for the second row of those that remain.
	#@   year day temperature
	#@ 3 1960   3        9.75
```


##### Logical indexing
We can evaluate logical statements using `==` (equals) `!=` (does not equal) `>=` (greater than or equal to) `|` (or) `&` (and).
```r
1==1
	#@ [1]  TRUE

c(1,2,3)==1
	#@ [1]  TRUE FALSE FALSE
```

If the logical evaluation is put in brackets, R will pull out those elements which are `TRUE`.
```r
Adoxo_head <- Adoxo_count[1:10,]
Adoxo_head
	#@    day year Adoxophyes_honmai
	#@ 1   64 1961                 0
	#@ 2   69 1961                 0
	#@ 3   74 1961                 1
	#@ 4   79 1961                 3
	#@ 5   84 1961                 0
	#@ 6   89 1961                 0
	#@ 7   95 1961                 4
	#@ 8  100 1961                 0
	#@ 9  105 1961                 0
	#@ 10 110 1961                 0

Adoxo_head$Adoxophyes_honmai == 3
	#@ [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

Adoxo_head[Adoxo_head$Adoxophyes_honmai == 3, ]
	#@   day year Adoxophyes_honmai
	#@ 4  79 1961                 3

Adoxo_head[Adoxo_head$Adoxophyes_honmai != 1 & Adoxo_head$Adoxophyes_honmai > 0, ]
	#@   day year Adoxophyes_honmai
	#@ 4  79 1961                 3
	#@ 7  95 1961                 4
```

The function `which()` returns the index numbers of those elements which match the logical evaluation,
```r
## note that the fourth and seventh elements have a value that is greater than 1
Adoxo_head$Adoxophyes_honmai
	#@ [1] 0 0 1 3 0 0 4 0 0 0
which(Adoxo_head$Adoxophyes_honmai > 1)
	#@ [1] 4 7
Adoxo_head$Adoxophyes_honmai[-which(Adoxo_head$Adoxophyes_honmai > 1)]
	#@ [1] 0 0 1 0 0 0 0 0
```

The function `match(x,y)` goes through each element of `x` and finds the index of the first value in `y` which matches that element. For example, we have two data frames of different length: the count data is only on some days of the year, while the temperature data is on all days of the year. We might want to extract the temperature values on those days which match the days in the count data.
```r
## to make things easier we'll only work with a single year.
## notice I use logical indexing to pull out only those data from the year 1961
Adoxo_count_1961 <- Adoxo_count[Adoxo_count$year == 1961, ]
Adoxo_temp_1961 <- Adoxo_temp[Adoxo_temp$year == 1961, ]

## just to make the example clearer, I'll scramble the order of the rows in the temperature data
set.seed(101)
Adoxo_temp_1961 <- Adoxo_temp_1961[sample(1:nrow(Adoxo_temp_1961)),]

## match returns a bunch of indices -- the number of which matches the number of rows in the count data
match(Adoxo_count_1961$day, Adoxo_temp_1961$day)
	#@ [1] 211  37  23 249 258 106 150 159 295 137 253 ... <snip>

## if I use these indices on the days in the temperature data ...
Adoxo_temp$day[match(Adoxo_count_1961$day, Adoxo_temp_1961$day)]
	#@ [1]  64  69  74  79  84  89  95 100 105 110 ... <snip>

## I get the same set of days as is given in the count data.
Adoxo_count_1961$day
	#@ [1]  64  69  74  79  84  89  95 100 105 110 ... <snip>
	
## Therefore I can add a temperature column to the count data very easily.
Adoxo_count_1961$temp <- Adoxo_temp_1961$temperature[match(Adoxo_count_1961$day, Adoxo_temp_1961$day)]
```

Note that missing values--denoted `NA` behave differently. You need to use `is.na()` for logical evaluations of missing values.
```r
NA==1
	#@ [1] NA

1==c(1,NA,4)
	#@ [1]  TRUE    NA FALSE

is.na(c(1,NA,4))
	#@ [1] FALSE  TRUE FALSE
```

Here are some other useful logical evaluations which operate on data containers.
```r
"monkey" %in% c("cow", "monkey", "Homo sapiens")
	#@ [1] TRUE

any(c(1,2,3,4,5) > 10)
	#@ [1] FALSE

all(c(1,2,3) < 5)
	#@ [1] TRUE
```

### Flow control and vectorization

Often we wish to repeat the same action, or apply it to multiple values/objects/whatever. Loops have the syntax `for(iterator in vector){ do stuff for each iterator }`.
```r
for(i in 1:5){ print(i) }
	#@ [1] 1
	#@ [1] 2
	#@ [1] 3
	#@ [1] 4
	#@ [1] 5
```

Note the curly brackets: as for functions, the curly brackets contain the operation you wish to perform at each iteration of the loop. `if` and `else` statements also require curly brackets:
```r
for(i in c(3,19,12,10)){
	if(i == 12) {
		print("whoops")
	} else {
		print(i)
	}
}
	#@ [1] 3
	#@ [1] 19
	#@ [1] "whoops"
	#@ [1] 10
```

Looping is extremely slow in R. Whenever possible, we wish to vectorize (perform an operation on all the elements of a data container simultaneously). Most operators and many functions in R are vectorized.
```r
c(1:10) + 5
	#@ [1]  6  7  8  9 10 11 12 13 14 15
c(1:10) + c(1:10) ## when objects have the same dimensions, the operations are done 'element-wise'
	#@ [1]  2  4  6  8 10 12 14 16 18 20
```

Many functions (not just mathematical operators) are vectorized:
```r
dnorm(Adoxo_temp$temperature[1:5], log=T) # evaluates standard normal pdf for each of the first 5 values in the temperature data
	#@ [1] -17.73894 -31.14425 -48.45019 -69.65675 -94.76394
```

Looping is slow. You can use `apply` and related functions to move quickly over rows or columns of any array, applying a function to each row. Read more about this in the full version of this document.

### Help

To see help regarding a specific function (note that the package that the function is in must be loaded!) do `?function_name`
```r
?apply
```

To search for a function across packages which are installed, do `??function_name`
```r
??coxph
```

And of course, [stackoverflow](http://stackoverflow.com/questions/tagged/r).

### Full document
Again, this is an abridged form of a slightly longer document. You can find it at:
https://github.com/sjfox/CCBB_Intro_Biostats/blob/master/week1_Rlesson_unabridged.md
