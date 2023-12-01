set file $argv[1]
rev $file | paste $file - | string replace -r '\D*(\d)\S*\t\D*(\d)\S*' '$1$2' | string join + | math
