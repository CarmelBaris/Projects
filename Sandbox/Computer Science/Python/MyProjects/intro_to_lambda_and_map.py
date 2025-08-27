## example for using a lambda function:
# guest_num is a variable to which we assigned an anonymous lambda function
# the function takes in three parameters and sums their values
guests_num = lambda adults, children, babies: (adults+children+babies)
print (guests_num (2, 4, 0)) #when calling the function, we need to pass values for its parameters
print (guests_num (2,2,1)) #the values can be changed each time


## example for using a map function with a list of arguments:
# the map function is assigned to the hour_passed variable
# it takes the values of the "day" variable and executes the operations defined by the lambda function
# since the variable is a list (between the square brackets), it repeats this process for each variable, returning a list of results
hours_passed = map (lambda days: days * 24, [1, 3, 14, 21])
print (list(hours_passed)) # note that when printing the responses, we are expecting a list and therefore we need to convert the results to a list.


##example for using a map function with a pre-defined list argument:
temps = (29, 36, 19, 26, 27, 28, 22, 32) # we have a list of temparatures in celsius
farenheit = map (lambda celsius: celsius * 9 /5 + 32, temps) # we use the lambda func to convert them to farenheit
print (list (farenheit)) #we convert the result from a map object to a list so that we could print it


##how can we do the same map function ^ using a tuple?
## temps = [("Argentina", 29), ("New York", 36), ("Los Angeles", 19), ("Jerusalem", 26), ("Tel Aviv", 27), ("Tokyo", 28), ("Nairobi", 22), ("Paris", 32)] # we have a tuple of temparatures in celsius

