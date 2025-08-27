'''
def magic_eight_ball():
    answer = input('Want to ask the magic 8 ball a question?').lower()
    if answer == "Yes":
        print ("You're in luck! he just passed by.")
    elif answer not 3:
        print ("That's absolutely right!")
    else:
        print ("Huh? Come again?")
        magic_eight_ball()

magic_eight_ball()'''


'''
state = input()

if state.lower() == 'israel':
    print('Shalom')

else: print('Hello')
'''

'''
x = 4 > 5
if x is True:
    print('Correct')
elif x is not True:
    print('Wrong!!!')
    '''

import sys

print ('Welcome to the Pig Latin Translator!')



def pyglatin():
  original = input('''What would you like to translate today?
''')
  # make sure the user writes something, not just blank space.
  if original == "exit":
    sys.exit ("Program closed")
  elif len(original) > 0 and original.isalpha() == True:
     pyg_translation = original[1:] + original[0] + 'ay'
     print(pyg_translation.capitalize())
  else:
      print("Oops! Please enter a single word and use only letters")
      
# redundant, since raw user input is always a string.
#  elif type != "<class 'str'>":
#      print ("Oops! We can only translate words, not numbers")

while True:
  pyglatin() # run the program again and again

