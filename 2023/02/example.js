// Generated by b.fish from example.txt
s = 0
blue = 0, green = 0, red = 0; blue = Math.max(blue, 3), red = Math.max(red, 4); red = Math.max(red, 1), green = Math.max(green, 2), blue = Math.max(blue, 6); green = Math.max(green, 2); s += blue * green * red
blue = 0, green = 0, red = 0; blue = Math.max(blue, 1), green = Math.max(green, 2); green = Math.max(green, 3), blue = Math.max(blue, 4), red = Math.max(red, 1); green = Math.max(green, 1), blue = Math.max(blue, 1); s += blue * green * red
blue = 0, green = 0, red = 0; green = Math.max(green, 8), blue = Math.max(blue, 6), red = Math.max(red, 20); blue = Math.max(blue, 5), red = Math.max(red, 4), green = Math.max(green, 13); green = Math.max(green, 5), red = Math.max(red, 1); s += blue * green * red
blue = 0, green = 0, red = 0; green = Math.max(green, 1), red = Math.max(red, 3), blue = Math.max(blue, 6); green = Math.max(green, 3), red = Math.max(red, 6); green = Math.max(green, 3), blue = Math.max(blue, 15), red = Math.max(red, 14); s += blue * green * red
blue = 0, green = 0, red = 0; red = Math.max(red, 6), blue = Math.max(blue, 1), green = Math.max(green, 3); blue = Math.max(blue, 2), red = Math.max(red, 1), green = Math.max(green, 2); s += blue * green * red
console.log(s)
