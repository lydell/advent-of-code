read line
set values (string match -r 'x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)' $line)
set target_x1 $values[2]
set target_x2 $values[3]
set target_y1 $values[4]
set target_y2 $values[5]
echo target x $target_x1 $target_x2 : $target_y1 $target_y2

# We will visit the exact same spots on the way up and down.
# The higher velocity we have when landing, the further up we must have been.
# Because at the peak, the velocity is 0. The higher up, the more steps it will
# take to get down. The more steps down, the more increase in velocity.
# If we go from the point of the target furthest away to 0 in one step, that
# should be the maximum possible velocity that also hits 0 (the starting point).
# So we can start from the end and work out what `y` will be when the velocity
# hits 0 (the peak).
set y $target_y1
set vy $y

# It’s probably possible to do this with math, but my code brain was quicker than my math brain.
while test $vy != 0
    set y (math $y - $vy)
    set vy (math $vy + 1)
end

echo $y
