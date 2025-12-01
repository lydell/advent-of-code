s = 50
c = 0
for _ in range(0, 68):
  s = (s - 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 30):
  s = (s - 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 48):
  s = (s + 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 5):
  s = (s - 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 60):
  s = (s + 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 55):
  s = (s - 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 1):
  s = (s - 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 99):
  s = (s - 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 14):
  s = (s + 1) % 100
  if s == 0:
    c = c + 1
for _ in range(0, 82):
  s = (s - 1) % 100
  if s == 0:
    c = c + 1
print(c)
