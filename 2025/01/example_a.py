s = 50
c = 0
s = (s-68) % 100; s or (c := c + 1)
s = (s-30) % 100; s or (c := c + 1)
s = (s+48) % 100; s or (c := c + 1)
s = (s-5) % 100; s or (c := c + 1)
s = (s+60) % 100; s or (c := c + 1)
s = (s-55) % 100; s or (c := c + 1)
s = (s-1) % 100; s or (c := c + 1)
s = (s-99) % 100; s or (c := c + 1)
s = (s+14) % 100; s or (c := c + 1)
s = (s-82) % 100; s or (c := c + 1)
print(c)
