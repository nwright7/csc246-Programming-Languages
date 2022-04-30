(* Nick Wright *)

(* Given the following signature. Implement the module MyStack based upon the semantics *)
(* described in the comments in the IntStack signature. *)

(* List-based Stack *)
signature IntStack =
sig
    exception EmptyStackException
    (* This "new" should be an empty list used as the initial stack. *)
    (* The top of a stack is the left most element in the list. *)
    val new : int list

    (* Push takes one int and a stack, returns a new stack with the new int at the beginning. *)
    val push : int * int list -> int list

    (* Pop takes a stack and returns a pair with the stack's top as the first element and the *)
    (* the tail as the second element. Raise the EmptyStackException if the stack is empty.*)
    val pop : int list -> int * int list

    (* Top takes a stack and return its top as an option without removing it. If the stack is *)
    (* empty, return NONE. *)
    val top : int list -> int option
end

structure MyStack :> IntStack = 
struct
    exception EmptyStackException

    val new = []		(* Given to you *)

    (* Implement the following three functions *)
		  
    fun push(i, s) =
	case (i, s) of
	    (i, []) => i::[]
	 | (i, s) => i::s

    fun pop s =
	case s of
	    [] => raise EmptyStackException (*if list is empty, return error*)
	       | s::xs => (s,xs) 

    fun top s = 
	case s of
	    [] => NONE (*if empty return NONE*)
	 | (x::xs) => SOME x 
end

(* Test Cases *)

(* Press Alt + ; to uncomment the selected following code for testing. *)

val initStack = MyStack.new	                (* [] *)
val curStack1 = MyStack.push(3, initStack)	(* [3] *)
val curStack2 = MyStack.push(2, curStack1)	(* [2,3] *)
val curStack3 = MyStack.push(5, curStack2)	(* [5,2,3] *)
val curStack4 = MyStack.push(10, curStack3)	(* [10,5,2,3] *)

val (top1, curStack5) = MyStack.pop(curStack4)	(* (10, [5,2,3]) *)
val (top2, curStack6) = MyStack.pop(curStack5)	(* (5, [2,3]) *)
val (top3, curStack7) = MyStack.pop(curStack6)	(* (2, [3] *)
val (top4, curStack8) = MyStack.pop(curStack7)	(* (3, []) *)
(* Exception occurs in the following case, handle it and return (~1, []) *)
 val (top5, curStack9) = MyStack.pop(curStack8)	
 			handle EmptyStackException => (~1, []) 


val top1' = MyStack.top(curStack4)	(* SOME 10 *)
val top2' = MyStack.top(curStack5)	(* SOME 5 *)
val top3' = MyStack.top(curStack6)	(* SOME 2 *)
val top4' = MyStack.top(curStack7)	(* SOME 3 *)
val top5' = MyStack.top(curStack8)	(* NONE *)

