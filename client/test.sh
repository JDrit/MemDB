./client 1 1
./client 2 2
./client 3 3
./client 4 4
./client 5 5
./client 6 6
./client 7 7
./client 8 8
./client 9 9
./client 10 10
./client 11 11
./client 12 12
./client 13 13
./client 14 14
./client 15 15
./client 16 16
./client 17 17
./client 18 18
./client 19 19
./client 20 20

test () {
    for i in `seq $1 $(($1 + 5000))`;
    do
        ./client $i "fuck this shit up the fuck" > /dev/null
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
read
