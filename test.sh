#!/bin/sh

for file in tests/stage_*/valid/*.c
do
	cabal run C-Compiler -- $file > out.s
	gcc out.s
	echo "$file:"
	./a.out; myOut=$?
	gcc $file
	./a.out; gccOut=$?
	if [ "$myOut" = "$gccOut" ]; then
		echo -e "\e[1;32mThe same.\e[0m"
	else
		echo -e "\e[1;31mNot the same.\e[0m"
		echo "My compiler: $myOut"
		echo "GCC: $gccOut"
	fi
done

