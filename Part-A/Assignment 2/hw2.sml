(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s,xs)=
    case xs of
	[] => NONE
      | x::xs' => if same_string(x,s) then SOME xs' else
		  case all_except_option(s,xs') of
		      NONE => NONE
		    | SOME y => SOME (x::y) 

		    
fun get_substitutions1(substitutions, s)=
    case substitutions of
	[] => []
      | x::xs' => case all_except_option(s,x) of
		      NONE => get_substitutions1(xs',s)
		    | SOME y => y @ get_substitutions1(xs',s)

fun get_substitutions2(substitutions, s) =
    let fun aux(sub , acc) =
	    case sub of
		[] => acc
	      | x::xs' => case all_except_option(s,x) of
			      NONE => aux(xs' , acc)
			    | SOME y => aux(xs' , y @ acc)
    in
	aux(substitutions, [])
    end


fun similar_names(xs,full)=
    let
	val {first = f , middle = m , last = l} = full
	fun update_name(names)=
	    case names of
		[] => []
	      | x::xs' => {first = x ,  middle = m , last = l} :: update_name(xs')
    in
	full :: update_name(get_substitutions1(xs,f))
    end
	
			     

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(card)=
    case card of
	(Clubs , _ ) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red 

fun card_value(card)=
    case card of
	( _ , Num i) =>i
      | (_ , Ace) =>11
      | _ =>10 

		
fun remove_card(cs,c,exp)=
    case cs of
	[] => raise exp
      | hd::tail => if hd = c then tail else
		    hd :: remove_card(tail,c,exp)
				   
fun all_same_color(cs)=
    case cs of
	[] => true
      | x::[] => true
      | hd::(neck::tl) => card_color(hd)= card_color(neck)
			    andalso all_same_color(neck::tl)
				 
					   
fun sum_cards(cs)=
    let fun aux(cs,acc)=
	    case cs of
		[] => acc
	      | x::xs' => aux(xs',card_value(x)+acc)
    in
	aux(cs,0)
    end
	
					   
					   
fun score(cs , goal) =
    let
	val sum = sum_cards cs
	fun pri_score(sum)=
	    if sum > goal then ( 3 * (sum-goal)) else (goal-sum)
    in
	case cs of
	    [] => goal
	  | _ =>
	    case all_same_color(cs) of
		true => pri_score(sum) div 2
	      | false => pri_score(sum)
	
    end


			       
fun officiate(cs,ms,goal)=
    let fun helper(cs,ms,hs)=
	    case ms of
		[] => score(hs,goal)
	      | hd::tail =>   case  hd of
				  Discard c => (case hs of
						    [] => raise IllegalMove
						  | _ => helper(cs , tail , remove_card(hs,c,IllegalMove)))
			       | Draw => case cs of
					     [] => score(hs,goal)
					   | x::xs' => if sum_cards(x::hs) > goal then  score(x::hs,goal)
						       else
							   helper(xs' , tail , x::hs)

    in
	helper(cs,ms,[])
    end
	
			  
