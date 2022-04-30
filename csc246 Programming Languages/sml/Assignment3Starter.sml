(* Nick Wright *)

(* Implement the following functions based upon the given definitions. *)

exception ImperfectExpression

datatype BinaryOperator = Add | Sub | Mul | Div
datatype UnaryOperator = Plus | Minus
datatype ExpTree = 
	 Leaf of int
       | UnaryNode of UnaryOperator * ExpTree
       | BinaryNode of BinaryOperator * ExpTree * ExpTree

(* You are required to use pattern matching in all functions. *)

(* 
The height function takes an ExpTree that represents a math expression, returns
the height of the tree. The height of a tree is the length of the deepest path 
from the root to leaves. The height of a leaf is zero. The height from the root 
to its direct leaf child is one. The height of the whole tree is the max of all
path lengths from root to leaves.
*)
						      
(* maxHelper made as a helper function *)
fun maxHelper(a,b)=
    if a < b then b else a
fun height exp =
    case exp of
	Leaf a => 0
      | UnaryNode(operator,l) => 1 + (height l)
      | BinaryNode(operator,l,r) => 1 + maxHelper(height(l),height(r))
				  

(* 
The eval function takes an ExpTree that represents a math expression, returns 
the evaluation result as an integer. For example, the expression "-3 * (4+1)"
should return -15. 
*)				   
fun eval exp = 
    case exp of
	Leaf x => x
      | UnaryNode(operator,l) => if operator = Plus
			    then eval l
				      (*else the operantor is a minus*)
			    else eval l * ~1
      | BinaryNode(operator,l,r) => if operator = Add
			       then eval l + eval r
			       else
				   if operator = Sub
				   then eval l - eval r
				   else
				       if operator = Mul
				       then eval l * eval r
				       else eval l div eval r
							    
(* | UnaryNode(operator,l) => if operator = Plus 
then eval l
else if operator = Minus
then eval (l*~1)*) 
(* | UnaryNode(operator,l) = > if operator = Plus
then eval l 
else if operator = Minus
then eval *)
	    


(*  
The function unaryOperators takes an ExpTree that represents a math expression,
returns a list of the unary operators in it. You are required to use tail 
recursion in this function.
*)
fun unaryOperators (acc, exp) = 
	case exp of
		Leaf(x) => acc
			| UnaryNode(operator,l) => (operator::acc)@unaryOperators(acc,l)
			| BinaryNode(operator,l,r) => unaryOperators(acc,l)@unaryOperators(acc,r)


(*  
The function binaryOperators takes an ExpTree that represents a math expression,
returns a list of the binary operators in it. You are required to use tail 
recursion in this function.
*)
fun binaryOperators (acc, exp) = 
    case exp of
    Leaf(x) => acc
    | UnaryNode(operator,l) => binaryOperators(acc,l)
    | BinaryNode(operator,l,r) => (operator::acc)@binaryOperators(acc,l)@binaryOperators(acc,r)
	

(*  
The function countOperands takes an ExpTree that represents a math expression,
returns the number of (integer) operands in it. You are required to use tail 
recursion in this function.
*)
fun countOperands (acc,exp) =
    case exp of
    Leaf(x) => 1
      | UnaryNode(operator,l) => countOperands(acc,l)
      | BinaryNode(operator,l,r) => countOperands(acc,l) + countOperands(acc,r)


(*  
The function needImprove takes an ExpTree that represents a math expression,
returns a boolean value. True means this expression needs to be improved. An 
expression needs improve if it has two consective unary operators in it such 
as "-(-3)" or "-(-(4+5))" since they can be simply written as "3" or "4+5". 
Raise an ImperfectExpression exception when this happens. Do not crash your 
program when the exception occurs. Instead, handle it gracefully by printing 
a message saying "You have consective unaray operators." and return true.
*)
									
(*fun needImprove exp = 
    false (* raise the exception as needed *)*)


(*  Here are two ready expressions for you to test with. Do not change them. *)
(*  Create more expressions to test if you want, but do not change these two. *)

val exp1 = BinaryNode(Add, 
		      UnaryNode(Minus, 
				Leaf(9)), 
		      BinaryNode(Mul, 
				 Leaf(7), 
				 BinaryNode(Sub, 
					    Leaf(9), 
					    Leaf(5))))
val exp2 = BinaryNode(Mul, 
		      UnaryNode(Minus, 
				UnaryNode(Minus, 
					  BinaryNode(Add, 
						     Leaf(6), 
						     Leaf(7)))), 
		      BinaryNode(Add, 
				 Leaf(1), 
				 Leaf(2)))

(* These are test cases. The results should match what is in the comments. *)
val depth1 = height exp1	(* 3 *)
val depth2 = height exp2	(* 4 *)
val r1 = eval exp1		(* 19 *)
val r2 = eval exp2		(* 39 *)
val uops1 = unaryOperators([], exp1) (* [Minus] *)
val bops1 = binaryOperators([], exp1) (* [Add,Mul,Sub] *)
val uops2 = unaryOperators([], exp2) (* [Minus,Minus] *)
val bops2 = binaryOperators([], exp2) (* [Mul,Add,Add] *)
val numOprands1 = countOperands(0, exp1) (* 4 *)
val numOprands2 = countOperands(0, exp2) (* 4 *)
(*val exp1NeedImprove = needImprove exp1	 (* false *)
                      (* Add your code to handle the possible exception *)
val exp2NeedImprove = needImprove exp2 (* true and a message should be displayed *)
		      (* Add your code to handle the possible exception *)*)


val exp3 = BinaryNode(Sub,Leaf(3),Leaf(4))
val extraTestEval = eval exp3
			 
val exp4 = BinaryNode(Sub,Leaf(17),Leaf(4))
val extraTestEval2 = eval exp4
			 
