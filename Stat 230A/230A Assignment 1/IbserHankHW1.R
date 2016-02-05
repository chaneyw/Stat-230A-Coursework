###  HW 1   Hank Ibser
###  I tend to use three pound symbols at the beginning of lines
###  that don't have to do with the code, like answering questions,
###  writing anything long.  I'd like you to do this for this class.
###  You can either leave in my comments or take them out.
###
###  Part 1.
###
### 1. put your code here for loading the data, write comments if anything
### isn't TOTALLY obvious.  For comments that explain the code, I like to use
### single pound symbols.

### 2. Write the function regcoef() as described in the assignment.
### You can change what I did but this is a suggestion as to how I'd
### start off writing the function.  For example it's not necessary to
### have a default for df but if you are going to be
### running and testing your function it will save typing.

regcoef=function(df=family[4:5]){
  # This function takes input of a data frame and returns regression 
  # coefficients predicting the second variable from the first.
}

### 3. Similarly, write the regline() function.

regline=function(df=family[4:5]){
  # This function plots points and regression line, doesn't need to return
  # anything.
  coefs=regcoef(df)
}