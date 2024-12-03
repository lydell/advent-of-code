cat | string join '' | string replace -ar 'don\'t\(\).*?(?:do\(\)|$)' '' | fish a.fish
