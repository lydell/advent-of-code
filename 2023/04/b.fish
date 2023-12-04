begin
    echo 'cards = ['
    cat \
        | string replace -r '^Card +\d+: +' 'len({' \
        | string replace -r ' +\| +' '}&{' \
        | string replace -r '$' '}),' \
        | string replace -ar ' +' ','
    echo ']
num_cards = [1 for _ in cards]
for i, wins in enumerate(cards):
    for j in range(i + 1, i + 1 + wins):
        num_cards[j] += num_cards[i]
print(sum(num_cards))'
end | python
