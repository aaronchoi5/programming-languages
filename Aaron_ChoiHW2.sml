(*Aaron Choi *)
(*2/3/2016*)
(*This file contains function definitions that are used to create ROBDD's and manipulate them with boolean algebra*)
(*Programming assignment #2 CS 4003 section 2*)

datatype ROBDD = True
|False
|IfThenElse of string * ROBDD * ROBDD;

fun bddAssert propLetter = IfThenElse(propLetter, True, False);

fun Reduce True = True
|Reduce False = False
|Reduce (IfThenElse(x,y,z)) =
if y = z
then 
y 
else
IfThenElse(x, Reduce(y), Reduce(z));

fun bddAnd (x,True) = Reduce(x)
|bddAnd (x, False) = False
|bddAnd (True,x) = Reduce(x)
|bddAnd (False,x) = False
|bddAnd (IfThenElse(x,leftOfFirstTree,rightOfFirstTree), IfThenElse(a,leftOfSecondTree,rightOfSecondTree))= 

if x = a 
then Reduce(IfThenElse(x,bddAnd(leftOfSecondTree,leftOfFirstTree),bddAnd(rightOfSecondTree, rightOfFirstTree)))
else if x < a 
then Reduce(IfThenElse(x,bddAnd(leftOfFirstTree,IfThenElse(a,leftOfSecondTree,rightOfSecondTree)),bddAnd(rightOfFirstTree,IfThenElse(a,leftOfSecondTree,rightOfSecondTree))))
else
Reduce(IfThenElse(a,bddAnd(leftOfSecondTree,IfThenElse(x,leftOfFirstTree,rightOfFirstTree)),bddAnd(rightOfSecondTree,IfThenElse(x,leftOfFirstTree,rightOfFirstTree))));

infix bddAnd;

fun bddNot True = False
|bddNot False = True
|bddNot (IfThenElse(x,leftOfTree,rightOfTree)) =
Reduce(IfThenElse(x,bddNot(leftOfTree), bddNot(rightOfTree)));

fun bddOr (bdd1, bdd2) =  bddNot((bddNot bdd1) bddAnd (bddNot bdd2));

infix bddOr;

fun bddImplies (hypo, conc) = ((bddNot hypo) bddOr conc);

fun bddIfThenElse( test,trueCase, falseCase) = ((bddNot(test)) bddOr (trueCase)) bddAnd (test bddOr falseCase);

fun bddEvalToTrue(True, propLetter)  = True
| bddEvalToTrue(False, propLetter) = False
| bddEvalToTrue(IfThenElse(x,leftOfTree,rightOfTree), propLetter) = 
if x = propLetter
then 
Reduce(leftOfTree)
else
Reduce(IfThenElse(x,bddEvalToTrue(leftOfTree, propLetter), bddEvalToTrue(rightOfTree, propLetter)));

fun bddEvalToFalse(True, propLetter)  = True
| bddEvalToFalse(False, propLetter) = False
| bddEvalToFalse(IfThenElse(x,leftOfTree,rightOfTree), propLetter) = 
if x = propLetter
then 
Reduce(rightOfTree)
else
Reduce(IfThenElse(x,bddEvalToFalse(leftOfTree, propLetter), bddEvalToFalse(rightOfTree, propLetter)));
