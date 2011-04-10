for file in `ls -1 python_src/*.ast`; 
do 
	echo -n $file "\t: "; 
	./main.native $file; 
done &> result
grep "syntax error" result > syntax_error
