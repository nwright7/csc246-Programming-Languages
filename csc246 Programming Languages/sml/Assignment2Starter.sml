(* Assignment2.sml*)
(* Implement the following  *)
(* functions as designated. *)

(* You are required not to use loops.*)
(* You are required to use pattern matching.*)
(* You may need to use nested case expressions.*)
(* You may need to conditional expressions, let expressions.*)

(* 13 pts
Function 'member' takes an int 'e' and an int list 'lst', returns true
if 'e' is in 'lst'; false otherwise.
Example:
   member(1,[1,3,5]) -> true
   member(2,[1,3,5]) -> false
*)
fun member(r, lst) = 
   case lst of
    [] => false
    |  h::t => if h = r then true
	       else member(r,t)

(* 13 pts 
Function 'subset' takes two integer lists 'lst1' and 'lst2' representing
two integer sets, returns true if 'lst1' is a subset of 'lst2'. Set T is a 
subset of S iff all elements of T are also elements of S. The empty set is 
subset of any set. 
Example:
   subset([1,5],[1,3,5]) -> true
   subset([1,7],[1,3,5]) -> false
*)
(* Hint: use the 'member' function *)
fun subset([], lst2) = true
  | subset (a::b, lst2) =
    if member(a,lst2) then subset(b,lst2)
    else false;

(* 17 pts
Funtion 'zip' takes two lists as input and returns an option. If the lengths
of these two pairs do not match, return NONE; if both are empty, return an empty
list; otherwise, return an option with pairs of elements, one from each list.
Examples: 
     zip ([], [])		-> SOME []
     zip ([1,2,3], [0,2,4]) 	-> SOME [(1,0),(2,2),(3,4)]
     zip ([1,2,3], [0,2,4,5]) 	-> NONE

*)
fun zip(lst1, lst2) = 
    case (lst1, lst2) of
	([],[]) => SOME []
      | ((hd::tl), (hd1::tl1)) => if
				     (length(hd::tl))<> (length(hd1::tl1)) (*if the lengths are not equal*)
				 then NONE
				 else SOME (ListPair.zip(lst1, lst2))
					   

(* 17 pts 
Function 'unzip' is the inverse function of 'zip'. It takes one list of pairs
and returns a pair of two separate lists. If there is no pairs in the input list,
return a pair of two empty lists.
Example:
    unzip [(1,0),(2,2),(3,4)]  -> ([1,2,3],[0,2,4])
    unzip [] -> ([],[])
*)
(* hint: use case expression, val binding pattern matching and let expression *)
fun unzip 1 = 
    case 1
	     of nil => (nil, nil) 
	      | (a,b)::tl =>
		let val (l1, l2) = unzip tl
		in (a::l1, b::l2)
		end
		    


(* 14 pts 
Function 'firstn' takes an integer list and returns the first 'n' elements of
this list as a list. If n is greater than the length of the list, return the whole
list. If n is zero, return an empty list.
Example:
    firstn([1,2,3,4,5,6], 3)  -> [1,2,3]
    firstn([1,2,3,4,5,6], 0)  -> []
    firstn([1,2,3,4,5,6], 10) -> [1,2,3,4,5,6]

 *)
fun firstn([], n) = []
  | firstn (x::xs, n) =
    if n <= 0 then []
    else x::(firstn(xs, n-1))
		


(* 13 pts
Function 'lastn' takes an integer list and returns the last 'n' elements of
this list as a list. If n is greater than the length of the list, return the whole
list. If n is zero, return an empty list.
Example:
    lastn([1,2,3,4,5,6], 3)  -> [4,5,6]
    lastn([1,2,3,4,5,6], 0)  -> []
    lastn([1,2,3,4,5,6], 10) -> [1,2,3,4,5,6]
 *)
(* You can use 'rev', 'firstn' *)
fun lastn(lst, n) = 
    rev(firstn((rev lst), n))

(* 13 pts
Function 'greeting' takes a string 'option' and returns a string 
saying "Hello there, ...!" where the dots would be replaced by the 
string value of the argument option. 
If the argument is NONE then replace the dots with "you". 
Example:
    greeting NONE -> "Hi there, you!"
    greeting (SOME "Yan") -> "Hi there, Yan!"
*)
fun greeting NONE = "Hi there, you!"
  | greeting (SOME i) = "Hi there, " ^ i ^ "!"; 

(* 
Extra Credit: 20

Function 'repeat' takes two int list lst1 and lst2. Assume that lst2 only has nonnegative integers, repeats the integers in the first list lst1 according to the numbers indicated by the second list lst2. If both lists are empty, return an empty list. You may need a local function.
Example:   
     repeat ([1,2,3], [4,0,3]) -> [1,1,1,1,3,3,3]
 *)
fun repeat(lst1, lst2) =
    []




(* Test Cases *)
val member_result1 = member(7, [1,3,5,7]) (* true  *)
val member_result2 = member(10, [1,3,5,7]) (* false  *)
val subset_result1 = subset([],[1,2,3])	   (* true *)
val subset_result2 = subset([2,8],[1,2,3,7,8,9])	   (* true *)
val subset_result3 = subset([2,4],[1,2,3,7,8,9])	   (* false *)
val zip_result1 =  zip([],[])		(* SOME [] *)
val zip_result2 =  zip([1,2,3], [0,2,4]) 	(* SOME [(1,0),(2,2),(3,4)] *)
val zip_result3 =  zip([1,2,3], [0,2,4,5]) 	(* NONE *)
val unzip_result1 = unzip(valOf(zip_result1))	(* ([],[]) *)
val unzip_result2 = unzip(valOf(zip_result2))	(* ([1,2,3], [0,2,4] *)
val firstn_result1 = firstn([1,2,3,4,5,6,7], 3)  (* [1,2,3] *)
val firstn_result2 = firstn([1,2,3,4,5,6,7], 0)  (* [] *)
val firstn_result3 = firstn([1,2,3,4,5,6,7], 10)  (* [1,2,3,4,5,6,7] *)
val lastn_result1 = lastn([1,2,3,4,5,6,7], 3)  (* [5,6,7] *)
val lastn_result2 = lastn([1,2,3,4,5,6,7], 0)  (* [] *)
val lastn_result3 = lastn([1,2,3,4,5,6,7], 10)  (* [1,2,3,4,5,6,7] *)
val greeting_result1 = greeting(NONE)		(* "Hi there, you!" *)
val greeting_result2 = greeting(SOME "Yan")	(* "Hi there, Yan!" *)
val repeat_result1 = repeat([1,2,3], [4,0,3])	(* [1,1,1,1,3,3,3] *)
val repeat_result2 = repeat([1,2,3], [0,0,0])	(* [] *)
