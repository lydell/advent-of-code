paste $argv[1] $argv[1] | string replace -r '^\D*(\d).+(\d)\D*$' '$1$2' | string join + | math
