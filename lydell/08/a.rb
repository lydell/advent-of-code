tree_max_height = 9

forest = Hash.new

width = 0
height = 0

ARGF.each_with_index do |line, y|
    width = line.length - 1
    height += 1
    line.strip.chars.each_with_index do |char, x|
        forest[[x, y]] = { height: char.to_i, is_visible: false }
    end
end

x_end = width - 1
y_end = height - 1

directions = [
    {
        inner_range: (0.upto x_end),
        outer_range: (0.upto y_end),
        mode: :inner_first,
    },
    {
        inner_range: (x_end.downto 0),
        outer_range: (0.upto y_end),
        mode: :inner_first,
    },
    {
        inner_range: (0.upto y_end),
        outer_range: (0.upto x_end),
        mode: :outer_first,
    },
    {
        inner_range: (y_end.downto 0),
        outer_range: (0.upto x_end),
        mode: :outer_first,
    },
]

directions.each do |direction|
    position = {}
    direction[:outer_range].each do |outer|
        tallest = -1
        direction[:inner_range].each do |inner|
            coordinate = if direction[:mode] == :inner_first then [inner, outer] else [outer, inner] end
            tree = forest[coordinate]
            if tree[:height] > tallest
                tree[:is_visible] = true
                tallest = tree[:height]
            end
            break if tallest == tree_max_height
        end
    end
end

puts forest.values.select { |tree| tree[:is_visible] }.count
