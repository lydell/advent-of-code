grep -vE '(1[3-9]|[2-9]\d|\d{3,}) red|(1[4-9]|[2-9]\d|\d{3,}) green|(1[5-9]|[2-9]\d|\d{3,}) blue' | string match -r '\d+' | string join + | math
