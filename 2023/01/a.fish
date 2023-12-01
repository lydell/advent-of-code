cat | string replace -r '.+' '$0$0' | string replace -r '^\D*(\d).+(\d)\D*$' '$1$2' | string join + | math
