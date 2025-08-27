def sum_double(a, b):
  if a != b: # if the two numbers are not the same
    return (a + b) # return their sum
  
  elif a ==b: # if they are the same number
    return (2 * (a + b)) # return double their sum

print(sum_double(7,7))
