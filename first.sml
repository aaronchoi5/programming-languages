(*Aaron Choi*);
(*This program will provides a means to generate primes up to a given number i.e. 10000 in the case of this assignment*);
(*Below this function returns true or false based on the ints given to it. It tests whether or not the second number is a multiple *);
fun isMult x y = (y mod x) = 0;

(*not function implemented here*);
fun negate func x = not (func x);

(*This function originally reversed a list I modified it so that it will also filter out the multiples of the current number too and keep recursing*);
fun reverseAndFilter lst =

    let fun rev'([], reversed)=
        reversed
	| rev'(x::xs,reversed)=
		rev'(List.filter(negate (isMult x)) xs, x::reversed)
	in 
	rev'(lst,[])
end;

(*literally the reverse function taken from the lecture notes i use it to reverse the list again*)
fun rev2 lst =
	let
	fun rev’ ([], reversed)
		    = reversed
	 |  rev’ (x::xs, reversed) 
	        = rev’(xs, x::reversed)
	in
		rev’ (lst,[])
end;


(*This is the range function from Prof. Schlipf. Generates numbers within a range.*);
fun range low high =
if high < low
then []
else low:: (range(low +1)high);

(*This function will give you primes up to a number. Give it a whirl!*);
fun primesthrough x = rev2( reverseAndFilter(range 2 x));

(*returns a reciprocal of a number*)
fun recipmaker x = 1.0/(real)x;

(*forgot about this it was an old idea*)
fun maptolist list = rev2 (map recipmaker list);

(*adds all the things in a list together*)
fun addall lst = (foldl (op +) 0.0) lst;
 
 (*helper function that filters primes based on if they are less than the limit then maps the recipmaker to them and adds em up*)
fun sumOfRecipsThrough' primelist limit_num = addall (map recipmaker (List.filter(fn x=> x < limit_num) primelist));

(*Do the prev function to every element in the limit list*)
fun sumofRecipsThrough limitlist primelist = map (sumOfRecipsThrough' primelist) limitlist;

(*for some reason nth didnt work i made my own*)
fun nth' (x::xs) index = 

if index = 0
then 
x
else
nth'(xs) (index -1);
(*the magic helper function that recurses stops when it reaches the point where there are less than 4 primes left*)
fun qualified primes =
let fun qualified'([], result) =
	result
	| qualified' (x::xs, result) =
	
	if length (x::xs) >=4
	then 
		if (nth (x::xs) 3) - (nth (x::xs) 0) <= 9
		then 
		x::qualified'(xs, result)
		else
		qualified'(xs, result)
	else 
	[]
in
qualified'(primes, [])
end;

(*if you give it a limit its gonna generate primes up to that limit and run the qualified function on the list*)
fun primeQuadruplesThrough limit = 

qualified(primesthrough limit);