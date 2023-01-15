function score() {
    case "$1" in
        'A X') echo 4;;
        'A Y') echo 8;;
        'A Z') echo 3;;
        'B X') echo 1;;
        'B Y') echo 5;;
        'B Z') echo 9;;
        'C X') echo 7;;
        'C Y') echo 2;;
        'C Z') echo 6;;
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
