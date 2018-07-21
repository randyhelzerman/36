clause((h:- a1)).
clause((h:- a2)).

axiom(a).
axiom(b).

dial(a11,a,1).
dial(a12,a,1).
dial(a1,a,1).
dial(a2,a,2).

dial(b1,b,1).
dial(b2,b,2).

dial(c1,c,1).
dial(c2,c,2).


clause((g:- a1,b1)).
clause((g:- a1,b2)).
clause((g:- a2,b2)).

clause((g:- b1,c1)).
clause((g:- b1,c2)).

clause((g:- a2,c1)).
clause((g:- a2,c2)).


prove((A ; B), R1,R2) :-
    (
        prove(A, R1,R2)
    ;
        prove(B, R1,R2)
    ).
    
prove((A,B), R1,R3) :-
    prove(A, R1,R2),
    prove(B, R2,R3).

prove(A, R,R) :- axiom(A).

prove(A, R1,R2) :- 
	clause((A:-B)),
	prove(B,R1,R2).


prove(A, R,R) :- 
      dial(A,N,V),
      member(N=V,R),!.

prove(A, R,[N=V|R]) :-
    dial(A,N,V),
    \+ member(N=_,R).


prove_under_all_scenarios([],_A).
prove_under_all_scenarios([Rs|Rss],A) :-
	prove(A,Rs,Rs),
	prove_under_all_scenarios(Rss,A).



get_all_other_scenarios(Rs,Rss) :-
	sort(Rs,Rs1),
	get_dials(Rs1,Ds),
	possible_scenarios_for_dials(Ds,Rss1),
	select(Rs1,Rss1,Rss).

get_dials(Rs,Ds) :-
	get_dials_from_scenarios(Rs,Ds1),
	sort(Ds1,Ds).


get_dials_from_scenarios([],[]).
get_dials_from_scenarios([D=_|Rs],[D|Ds]) :-
	get_dials_from_scenarios(Rs,Ds).


possible_scenarios_for_dials(Ds,Rss) :-
	possible_assignments_for_dials(Ds,As),
	findall(Rs,possible_scenario_for_dials(As,Rs), Rss).

possible_scenario_for_dials([],[]).
possible_scenario_for_dials([D|Ds],[A|As]) :-
	select(A,D,_),
	possible_scenario_for_dials(Ds,As).


possible_assignments_for_dials([],[]).
possible_assignments_for_dials([D|Ds],[Rs|Rss]) :-
	possible_assignments_for_dial(D,Rs),
	possible_assignments_for_dials(Ds,Rss).

possible_assignments_for_dial(D,Rs) :- setof(D=V,X^dial(X,D,V),Rs).

