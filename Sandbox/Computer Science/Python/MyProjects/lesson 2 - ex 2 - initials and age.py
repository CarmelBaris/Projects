# this works
# main TypeErrors are handled

import datetime

def initials_age():
    print ("Please enter the year you came into this amazing world:")
    user_birthyear = input()
    if (user_birthyear.isalpha() == False and len(user_birthyear) > 0 and ' ' not in user_birthyear): # test: enter a string or numbers with spaces between them
       # convert the string input into a number for later use:
        user_birthyear = int(user_birthyear)
        current_year = datetime.datetime.now().year
        if 0 <= user_birthyear <= current_year: # test: enter next year
            # calculate how old the user will be this year:
            user_age = current_year - user_birthyear
            print ("What is your first name?")
            user_firstname = input()
        else:
            print ("Please try again.")
    else:
        return ("Please try again.") 
    if user_firstname.isalpha() == True and len(user_firstname) > 0 and ' ' not in user_firstname:
        print ("What is your last name?")
        user_surname = input()
    else:
        return ("Please try again.") 
    if user_surname.isalpha() == True and len(user_firstname) > 0 and ' ' not in user_surname:
        message = "Congrats " + user_firstname[0].capitalize() + user_surname[0].capitalize() + "! This year you will turn " + str(user_age) + "!"
        return message ## how do I ensure the program doesn't kill the console window after it finishes?
    else:
        return ("Please try again.") 
            # it's better to return and not print here, because I print the function later on.
            # if I do print here as well, it will return "None" or the else statement'''
    
print(initials_age())
