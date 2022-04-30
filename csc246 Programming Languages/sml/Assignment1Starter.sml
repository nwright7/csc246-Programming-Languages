(* Nick Wright *)
(* Assignment1.sml*)
(* Implement the following  *)
(* functions as designated. *)

(* Function 'append' takes two lists of the same type, appends the 2nd list to the 1st list. It returns the concatenated list.
Example: append([3,4,2],[1]) -> [3,4,2,1]*)
fun append(lst1, lst2) = 
    if null lst1
    then lst2
    else
	(hd lst1)::append(tl lst1, lst2)
(* replace [] with your code *)


(* Function 'reverseList' takes a list, returns a new list with the reversed elements from 
the argument list.
Example:  reverseList [2,3,4] -> [4,3,2]*)
fun reverseList(lst) =
    if null lst
    then lst
    else
	append(reverseList(tl lst), hd(lst)::[])
                           (* define extra functions if needed *)


(* Function 'digits' takes an integer, returns an int list of all the digits in this integer
Example:  digits 4271 -> [4,2,7,1]
Hint: use div and mod functions with 10 as the divisor. *)
fun digits(m:int) =
    if m<10 then m::[]
    else
        let
	    val var1 = (m mod 10)
	    val var2 = (m div 10)
        in
            digits(var2)@[var1]
        end
    				(* define local function and extra functions if needed *)

(* 
Function 'sum' takes an integer list, returns the sum of all the integers in the list. 
Example:
   sum [2,3,1] -> 6
*)

fun sum(ints) = 
    if null ints
    then 0
    else
	(hd ints) + sum(tl ints)
		       
(* Consider the process of taking a number 'n', adding its digits, then adding the digits 
of the number derived from it, etc., until the remaining number has only one digit.
The final single digit number is called the digital root of 'n'.
Example:
    The sequence obtained from the starting number 6381 is (6381, 18, 9), so 6381 has 
    a digital root of 9.
Define a function named digitalRoot that takes a non-negative integer and returns the
digital root of this integer.*)
fun digitalRoot(i:int) = 
    if i < 10 then i
    else
	digitalRoot(sum(digits(i)))

(* Keep the following testing code. You can add more testing code if you want. *)
(* However, you can temporarily put them in comments while you implement your solutions. *)
(* You can highlight the code then press Alt+; This will comment out all the highlighted code.*)
val append_result = append([5,4,3,2], [1]) (* Result: [5,4,3,2,1] *)
val append_result2 = append([3,4,2],[1]) (* Result should be [3,4,2,1] *)
val reverseList_result = reverseList(append_result) (* Result: [1,2,3,4,5] *)
val reverseList_result2 = reverseList[2,3,4] (* Result should be [4,3,2] *)
val sum_result = sum reverseList_result (* Result: 15 *)
val sum_result2 = sum[2,3,1] (* Result should be 6 *)
val digit_results = digits sum_result (* Result: [1,5] *)
val digit_results2 = digits(4721) (* Result should be [4,2,7,1] *)
val digitalRoot_result = digitalRoot (sum(digit_results)) (* Result: 6 *)
val digitalRoot_result2 = digitalRoot(6381) (* Result should be 9 *)
