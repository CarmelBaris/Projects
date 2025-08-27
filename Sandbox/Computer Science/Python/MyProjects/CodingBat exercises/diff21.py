def diff21(n):
  if n <= 21:
    return abs(n -21)
  else: # if n is bigger than 21, double the absolute delta.
    return (2 * abs(n-21))

print(diff21(43))
