#!/bin/sh
cd ../../demos/count_characters && make all
./master > ../../tests/count_characters/master.log &
./reducer > ../../tests/count_characters/reducer.log &
./mapper > ../../tests/count_characters/mapper.log &