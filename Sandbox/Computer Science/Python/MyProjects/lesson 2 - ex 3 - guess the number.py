'''import random

generated = random.randint(1,10) ##choose a pseudo-random between 1 and 10.
user_response = "You there! Yes, you! Pick a number, any number! " # a global variable, can be called anywhere in the code

def message():
    return ("Step right up to see the marvelous mathemagician!")

def choose_number ():
    return user_response = (input(user_response))

def generate_random():
    for x in range(1):
        return (generated)

def compare_num():
##    if generated == user_response: #in range (1,10):
    if user_response == 2: #in range (1,10):
        print ("You win!")
    else:
        print ("Not your best guess...")

print (message())
choose_number()
generate_random()
compare_num()
'''

'''import random

generated = random.randint(1,10) #choose a pseudo-random between 1 and 10.
user_response = "You there! Yes, you! Try beating him by guessing his magic number. Pick a number, any number between 1 and 10! " #a global variable, can be called anywhere in the code

def message():
    return ("Step right up to see the marvelous mathemagician!")

def choose_number ():
   global answer
   answer = input(user_response)
   return (answer)

def generate_random():
    for x in range(1): #for all variables between 1 and 1 , meaning return an answer only once. what is this line good for?
        return (generated)

def compare_num():
    
    if generated == int(answer): #we have a match!
#    if int(answer) == 2: #the numerical value of the user's response (which is returned as a string)
        print ("Bull's eye! You are the great master of them all :)")
        
    elif generated > int(answer): #your answer is too small:
        print ("Too small, not your best call :(")
        
    elif generated < int(answer): #your answer is too small:
        print ("Too big, better luck next time :/")
        
    else:
        print ("Say, where you even listening to my instructions?? Try again, buddo...")

print (message())
choose_number()
generate_random()
compare_num()
'''







###YARDEN'S FIX


import random

generated = random.randint(1,3) #choose a pseudo-random between 1 and 10.
user_response = '''You there! Yes, you! Try beating him by guessing his magic number.
Pick a number, any number between 1 and 10! ''' #a global variable, can be called anywhere in the code

def message():
    return ("Step right up to see the marvelous mathemagician!")

def choose_number ():
   global answer
   answer = input(user_response)
   return (answer)

def generate_random():
    for x in range(1): #for all variables between 1 and 1 , meaning return an answer only once. what is this line good for?
        print (generated)
        return (generated)

def compare_num():
    
    if generated == int(answer): #we have a match! compare to the numerical value of the user's response (which is returned as a string)
        print ("Bull's eye! You are the great master of them all :)")
        
    elif generated > int(answer): #your answer is too small:
        print ("Too small, not your best call :(")
        
    elif generated < int(answer): #your answer is too small:
        print ("Too big, better luck next time :/")
        
    else:
        print ("Say, where you even listening to my instructions?? Try again, buddo...")

print (message())
choose_number()
generate_random()
compare_num()
