fun countZeroes lst =
	let fun counter([], count) =
		count
	|counter(x::xs, count)=
	if x = 0
	then 
		counter(xs, count+1)
	else
		counter(xs,count)
	in	
		counter(lst,0)
	end;

fun addall lst =
(foldl (op +) 0) lst;

fun replaceBlankWithNumber (x, sumoflst) =
	if x = 0
	then
		45 - sumoflst
	else
		x;
	
fun sub lst=
	let fun substit([], reversed)=
		rev reversed
	|substit(x::xs, reversed)=
	substit(xs, replaceBlankWithNumber(x,addall(lst))::reversed)
	in
		substit(lst, [])
	end;

fun force(lst) = 
	if countZeroes(lst) = 1
	then 
		sub lst
	else
		lst;
		
(*use this to force values upon the grid*)
fun forcer lstoflsts = 
	let fun overwrite([],reversed) =
		rev reversed
	|overwrite(x::xs, reversed)=
	overwrite(xs, force(x)::reversed)
	in
		overwrite(lstoflsts,[])
	end;

fun transpose ([]::_)= []
|transpose rows = 
(map hd rows)::transpose(map tl rows);

(*compare after forcing probably then force again*)
fun compare ([], [], finallst)=
	rev finallst
|compare(x::xs, y::ys, finallst)=
	if x = 0 andalso y <> 0
		then compare(xs, ys, y::finallst)
	else
		if x<>0 andalso y = 0
			then compare(xs, ys, x::finallst)
		else 
			compare(xs, ys, x::finallst);

fun merge([],[], reversed) =
		rev reversed
|merge(x::xs, y::ys, reversed)=
		merge(xs,ys, compare(x, y, [])::reversed);
		
fun leftBlock lst = 
let fun leftBlock2List ([], reversed)=
	List.concat (rev(reversed))
| leftBlock2List(x::xs, reversed)=
leftBlock2List(xs,List.take(x,3)::reversed)
in
leftBlock2List(lst,[])
end;

fun middleBlock lst = 
let fun middleBlock2List ([], reversed)=
	List.concat (rev(reversed))
| middleBlock2List(x::xs, reversed)=
middleBlock2List(xs,List.take(List.drop(x,3),3)::reversed)
in
middleBlock2List(lst,[])
end;

fun rightBlock lst = 
let fun rightBlock2List ([], reversed)=
	List.concat (rev(reversed))
| rightBlock2List(x::xs, reversed)=
rightBlock2List(xs,List.drop(x,6)::reversed)
in
rightBlock2List(lst,[])
end;

(*will invert to either row lists or block lists*)
fun blocksAndRowsInverter lstoflsts=
[leftBlock(List.take(lstoflsts, 3)),middleBlock(List.take(lstoflsts,3)),rightBlock(List.take(lstoflsts,3)),
leftBlock(List.take(List.drop(lstoflsts,3),3)),middleBlock(List.take(List.drop(lstoflsts,3),3)),rightBlock(List.take(List.drop(lstoflsts,3),3)),
leftBlock(List.drop(lstoflsts,6)),middleBlock(List.drop(lstoflsts,6)),rightBlock(List.drop(lstoflsts,6))];
		
fun forces lstoflsts =
	let fun foo(lst, false )=
		lst
	|foo(lst, true)=
		if merge(merge(forcer(lst), transpose (forcer(transpose lst)),[]), blocksAndRowsInverter (forcer(blocksAndRowsInverter lst)),[]) = lst
		then
			foo(merge(merge(forcer(lst), transpose(forcer(transpose lst)),[]),blocksAndRowsInverter (forcer(blocksAndRowsInverter lst)),[]),false)
		else
			foo(merge(merge(forcer(lst), transpose(forcer(transpose lst)),[]), blocksAndRowsInverter (forcer(blocksAndRowsInverter lst)),[]), true)
	in foo(lstoflsts, true)
end;
		

fun findDupesInSortedLst([]) = false
  | findDupesInSortedLst(head::[]) = false
  | findDupesInSortedLst(head::h_tail::tail) = 
    if head = h_tail 
	then 
		true
    else
		findDupesInSortedLst(h_tail::tail)
;
		

		
fun quicksort [ ] = [ ]
| quicksort [x] = [x]
| quicksort (x::xs) =
	let val lowVals =
		(List.filter (fn y=> y<x) xs)
	val midVals =
		(x::(List.filter (fn y=> y=x) xs))
	val hiVals =
		(List.filter (fn y=> y>x) xs)
	in
		(quicksort lowVals)
		@ midVals
		@ (quicksort hiVals)
end;

fun isZero x = 
if x = 0
then
	true
else 
	false;
	
fun negate func arg = not (func arg);

(*more for convienence*)
fun hasDupes lst = 
	if findDupesInSortedLst(quicksort (List.filter(negate isZero) lst)) = true
	then 
		true
	else 
		false;


(*use this method later with the transpose and inverter*)
fun duplicates lstoflsts =
	let fun process([]) =
		false
	|process(x::xs) =
		if hasDupes x = true
			then
				true
			else
				process(xs)
	in process(lstoflsts)
end;
(*fix soon*)
fun contradictionDetected lstoflsts=
if (duplicates lstoflsts) = true orelse (duplicates (transpose lstoflsts)) = true orelse (duplicates (blocksAndRowsInverter lstoflsts)) = true
then 
	true
else 
	false;

		
fun emptySpaceCheck lst = 
	let fun checker([])=
		false
	|checker(x::xs)= 
	if(isZero x) = true
	then
		true
	else
		checker(xs)
	in checker(lst)
end;

fun hasEmptyPos lstoflsts = 
	let fun listChecker([])=
		false
	|listChecker(x::xs)= 
	if(emptySpaceCheck x) = true
	then
		true
	else
		listChecker(xs)
	in listChecker(lstoflsts)
end;

fun replace(lst, colPos, entry)=
	let fun workthru([], reversed, count) =
		rev reversed
	|workthru(x::xs, reversed, count)=
		if count <> colPos
		then
			workthru(xs,x::reversed, count+1)
		else
			if isZero x = true
			then
				workthru(xs,entry::reversed, count+1)
			else
				workthru(xs,x::reversed, count+1)
		in workthru(lst,[],1)
end;

fun fillin( lstoflsts, (rowPos, colPos), entry)=
	let fun tailRecurse([],reversed, count)=
		rev reversed
	|tailRecurse(x::xs, reversed, count)=
		if count<> rowPos
		then
			tailRecurse(xs, x::reversed, count + 1)
		else
			tailRecurse(xs, (replace (x, colPos, entry))::reversed, count+1)
	in tailRecurse(lstoflsts, [],1)
end;

fun findFirstBlankColPos lst =
	let fun locating([], count) =
		0
	|locating(x::xs, count) =
		if (isZero x) = true
		then
			count
		else
		locating(xs,count+1)
	in locating(lst, 1)
end;

fun findFirstBlank lstoflsts =
let fun scanning([],count)=
	(0,0)
	|scanning(x::xs,count)=
	if (emptySpaceCheck x) = true
		then 
			( count, findFirstBlankColPos x)
		else
		scanning(xs,count+1)
	in
		scanning(lstoflsts, 1)
end;


fun generateListOfLegals(lstoflsts, (row, col))=
	let fun finding(lstoflsts, (row,col), [], legalvalueslist)=
		((rev legalvalueslist))
	|finding(lstoflsts,(row,col), x::xs, legalvalueslist)=
		if (contradictionDetected (fillin(lstoflsts,(row,col), x))) = true
		then
			finding(lstoflsts,(row,col),xs,legalvalueslist)
		else
			finding(lstoflsts,(row,col),xs, x::legalvalueslist)
	in
		finding(lstoflsts, (row,col), [1,2,3,4,5,6,7,8,9], [])
end;
		
		(*fun trueforce lstoflsts =
			let val moo = generateListOfLegals(lstoflsts, findFirstBlank lstoflsts)
			in 
				if length (generateListOfLegals(lstoflsts, findFirstBlank lstoflsts)) = 1
				then 
					forcarino (fillin(lstoflsts, (findFirstBlank lstoflsts),  #1moo))
			end
		
		;*)
fun newbrds(lstoflsts, (row, column), lstoflegals)=
	let fun generatebrds(lstoflsts,boards, [])=
		boards
	|generatebrds(lstsoflsts, boards, x::xs)=
		generatebrds(lstoflsts, fillin(lstoflsts,(row,column), x)::boards, xs)
	in 
		generatebrds(lstoflsts, [], lstoflegals)
end;


fun solve board =
	let fun fillInAndSolve (entry, board')=
		let val newbrd1 = fillin (board',(findFirstBlank board),entry)
		in 
			if(contradictionDetected newbrd1) = true
			then 
				board'
			else 
				let val newbrd2 = 
					forces newbrd1
				in 
					if(contradictionDetected newbrd2) = true
					then
						board'
					else

						if (hasEmptyPos newbrd2) = false
						then
							newbrd2
						else
							solve newbrd2
				end
		end
		
		in foldr fillInAndSolve board (generateListOfLegals(board, findFirstBlank board))
	end;