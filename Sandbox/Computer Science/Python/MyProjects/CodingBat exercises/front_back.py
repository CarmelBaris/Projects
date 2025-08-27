def front_back(str):
    if str.isalpha() == False: # if it isn't a real string, don't handle it
        return ('Oops! This is not a string')
    elif str.isalpha() == True and len(str) == 1: # if it is a string, of a single letter
        return str # don't change the string
    elif str.isalpha() == True: # it is redundant to state that the length has to be bigger than one, since we covered that in the previous conditions.
        first_char = str[0]
        last_char = str[len(str)-1]
        middle_char = str[1:len(str)-1]
        new_str = last_char + middle_char + first_char # switch the first and last letters.
        return new_str
    else: # I don't really know when this will be True.
        return 'Blank'

print(front_back('').capitalize())



# I don't understand where I went wrong before.
# What is the real order of execution of this code?
'''def front_back(str):
    str_length = len(str)
    if str_length > 1 and str.isalpha() == True:
        first_char = str[0] # first letter is the first index.
        last_char = str[len(str)-1] # the string's length is 1 above the last index number.
        middle = str[1:(len(str)-1)] # without the first or last letters.
        new_str = last_char + middle + first_char
        return (new_str)

    elif str_length == 1 and str.isalpha() == True: # if the string is only a single letter.
        new_str = str
        return (new_str) # don't touch the string.

    elif str.isalpha() == False:
        return("Don't use numbers")

    else:
        return ('Blank')

print(front_back (5).capitalize())
'''
