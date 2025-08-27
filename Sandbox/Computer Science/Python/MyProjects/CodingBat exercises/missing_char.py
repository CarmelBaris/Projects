'''
def missing_char(str, n):
  new_string = str[:n] + str[(n+1):]
  if type(str) = '<class '/str'>' and (len(str) - 1) >= n >= 0:
    return new_string
'''


def missing_char(str, n):
  new_string = str[:n] + str[(n+1):]
  if len(str) > 0 and str.isalpha() == True and (len(str) - 1) >= n >= 0:
    return new_string
  else:
    print("Hey there! Make sure you inserted a single word and that your index is in the range of the chosen word")

print(missing_char('CrazyPants', 0)) # test to make sure it works
