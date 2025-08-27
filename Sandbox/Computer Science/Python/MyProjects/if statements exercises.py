def magic_eight_ball():
    answer = input('Want to ask the magic 8 ball a question?').lower()
    if answer == "Yes":
        print ("You're in luck! he just passed by.")
    elif answer not 3:
        print ("That's absolutely right!")
    else:
        print ("Huh? Come again?")
        magic_eight_ball()

magic_eight_ball()


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
