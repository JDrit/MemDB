test () {
    for i in `seq $1 $(($1 + 5000))`;
    do
        ./client get  $i > /dev/null
    done
    echo $1
}

test 1 &
test 5000 &
test 10000 &
test 15000 &
test 20000 &
test 25000 &
test 30000 &
test 35000 &
test 40000 &
test 45000 &
test 50000 &
test 55000 &
test 60000 &
read
