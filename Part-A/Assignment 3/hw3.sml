(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

			       
fun only_capitals xs =
    List.filter(fn s => Char.isUpper(String.sub(s,0)))  xs

fun longest_string1 xs =
    foldl (fn (x,y) => if String.size x > String.size y
		       then x
		       else y ) ""  xs
	       
fun longest_string2 xs =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y ) ""  xs

	  
fun longest_string_helper f xs =
    foldl (fn(x,y) => if f(String.size x , String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn(x,y) => x > y)
val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

	  
val longest_capitalized = fn xs => ( longest_string1 o only_capitals) xs

								      
fun rev_string s = ( implode o  rev o  explode ) s

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		   | SOME v => v 

				   
fun all_answers f xs =
    let
	fun helper (xs , acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME y => helper(xs' ,y @acc)
    in
	helper(xs, [])
    end
	
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p

fun count_some_var (s,p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p


fun check_pat p =
    let
	fun all_strings p =
	    case p of
		Variable s => [s]
	      | TupleP pl => foldl (fn (pt,v) => (all_strings pt) @ v) [] pl
	      | ConstructorP (s,ptr) => all_strings ptr
	      | _ => []

	fun list_exsist xs =
	    case xs of
		[] => false
	      | x::xs' => List.exists (fn s => x = s ) xs
    in
	(list_exsist o all_strings) p
    end
	
	    
fun match(v, p) =
	case (v, p) of
	  (_, Wildcard) => SOME []
	| (v, Variable s) => SOME [(s, v)]
	| (Unit, UnitP) => SOME []
	| (Const x, ConstP y) => if x = y then SOME [] else NONE
	| (Tuple valu_list, TupleP pattern_list) => if length valu_list = length pattern_list
		then all_answers match (ListPair.zip(valu_list, pattern_list)) else NONE
	| (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v,p) else NONE
	| (_, _) => NONE


fun first_match v pattern_list =
	SOME(first_answer(fn p => match(v, p)) pattern_list) handle NoAnswer => NONE
