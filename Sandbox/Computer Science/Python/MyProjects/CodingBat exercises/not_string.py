def not_string(str):
# to start from the first letter we state the position 0, or we don't state its position.
# to include the third letter, we must state a position larger by 1, since the last stated position is not included.
  if str[:3] == 'not': # if the string starts with "not"
    return (str) # don't touch the string      
  else: # if it doesn't start with "not"
    return ('not ' + str) # add a "not" prefix

print(not_string('quite coolio'))


print(type('Hello'))
