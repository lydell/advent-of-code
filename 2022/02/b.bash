function score() {
    case "$1" in
        'A X') echo 3;;
        'A Y') echo 4;;
        'A Z') echo 8;;
        'B X') echo 1;;
        'B Y') echo 5;;
        'B Z') echo 9;;
        'C X') echo 2;;
        'C Y') echo 6;;
        'C Z') echo 7;;
        *)
            echo "Invalid score: $1"
            exit 1;;
    esac
}

sum=0

while read -r line; do
    sum=$((sum + $(score "$line")))
done

echo $sum
