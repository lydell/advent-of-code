rg 'mul\((\d{1,3}),(\d{1,3})\)' -or '$1 * $2' | string join + | math
