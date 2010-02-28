#!/bin/sh
cd ../../demos/md5_collision_search && make all
./master  > ../../tests/md5_collision_search/master.log &
./reducer > ../../tests/md5_collision_search/reducer_1.log &
./reducer -ls-port=9001 > ../../tests/md5_collision_search/reducer_2.log &
./mapper  > ../../tests/md5_collision_search/mapper_1.log &
./mapper -ls-port=8001  > ../../tests/md5_collision_search/mapper_2.log &