/* David Kwak */

/* Exercise 1 */
redefine_system_predicate(when).

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

/* 1a */
/*  schedule/3 */
/*  
schedule(mary,P,T). 

P = owen102,
T = 10 ;

P = dear118,
T = 12.
*/
/*  
schedule(S, cov216, T). 

S = john,
T = 10 ;

S = jim,
T = 12.
*/
schedule(N, P, T) :- enroll(N, A), where(A, P), when(A, T).

/* 1b */
/*  usage/2 */
/*  
usage(cov216, T). 

T = 11 ;
T = 12 .
*/
/*  
usage(X, 12). 
X = dear118 ;
X = dear118 ;
X = cov216 .
*/
usage(C, T) :- where(A, C), when(A, T).

/* 1c */
/*  conflict/2 */
/*
conflict(275,X).
false
*/
conflict(N, X) :- where(N, A), where(X, A), when(N, B), when(X, B), N \= X.

/* 1d */
/*  meet/2 */
meet(A, B) :-   schedule(A, X, Y), schedule(B, X, Y),  A \= B;
			    schedule(A, X, Z), schedule(B, X, AA), A \= B, Z \== AA + 1.


/* Exercise 2 */

/* 2a */
rdup([],[]).
rdup([A|L],M) :- L = [A|_], rdup(L,M).
rdup([A|L],[A|M]) :- rdup(L,M).

/* 2b */
flat([], []).
flat([A|L], F) :- flat(A, B) , flat(L, M) , append(B, M, F).
flat(L, [L]).

/* 2c */