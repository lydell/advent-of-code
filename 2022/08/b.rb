$forest = Hash.new

width = 0
height = 0

ARGF.each_with_index do |line, y|
    width = line.length - 1
    height += 1
    line.strip.chars.each_with_index do |char, x|
        $forest[[x, y]] = char.to_i
    end
end

$x_end = width - 1
$y_end = height - 1

def scenic_score(start_x, start_y)
    tree_height = $forest[[start_x, start_y]]

    d1 = 0
    (start_x - 1).downto 0 do |x|
        d1 += 1
        break if $forest[[x, start_y]] >= tree_height
    end

    d2 = 0
    (start_x + 1).upto $x_end do |x|
        d2 += 1
        break if $forest[[x, start_y]] >= tree_height
    end

    d3 = 0
    (start_y - 1).downto 0 do |y|
        d3 += 1
        break if $forest[[start_x, y]] >= tree_height
    end

    d4 = 0
    (start_y + 1).upto $y_end do |y|
        d4 += 1
        break if $forest[[start_x, y]] >= tree_height
    end

    d1 * d2 * d3 * d4
end

max = 0

(0..$x_end).each do |x|
    (0..$y_end).each do |y|
        score = scenic_score x, y
        max = score if score > max
    end
end

puts max
