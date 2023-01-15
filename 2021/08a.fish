cat | string replace -r '^[^|]+\| ' '' | string split ' ' | string match -r '^(?:.{2,4}|.{7})$' | wc -l | string trim
