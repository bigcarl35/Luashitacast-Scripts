echo "Enter file root: "
read fname
set io "$fname.lua"
set xt "s.lua"
set fname "$fname$xt"

set KW "Notorious"
awk -v kw="$KW" '{
    i = index($0, kw)
    if (i > 0) {
        print substr($0, 1, i-1)
    } else {
        print $0
    }
}' $io > $fname
