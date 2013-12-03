#!/bin/sh

for i in `ls ru/`
do 
  list="$list `ls ru/$i/*.md`"
done

cat $list > book/erlangbook.ru.md

pandoc -f markdown book/erlangbook.ru.md -o book/erlangbook.ru.epub -t epub3
