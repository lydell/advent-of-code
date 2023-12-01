cat \
    | string replace -a oneight 18 \
    | string replace -a threeight 38 \
    | string replace -a fiveight 58 \
    | string replace -a nineight 98 \
    | string replace -a twone 21 \
    | string replace -a eightwo 82 \
    | string replace -a eighthree 83 \
    | string replace -a sevenine 79 \
    | string replace -a one 1 \
    | string replace -a two 2 \
    | string replace -a three 3 \
    | string replace -a four 4 \
    | string replace -a five 5 \
    | string replace -a six 6 \
    | string replace -a seven 7 \
    | string replace -a eight 8 \
    | string replace -a nine 9 \
    | fish a.fish
