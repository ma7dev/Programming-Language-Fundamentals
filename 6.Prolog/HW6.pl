/* Exercise 1 */

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

/*
 * a) Define a predicate schedule/3 that gives for a student 
 * the classrooms and times of his or her taken classes, that is,
 * if you evaluate the goal schedule(mary,P,T), Prolog should 
 * give the following result.
 */

schedule(A,B,C) :- enroll(A, Z), where(Z, B), when(Z, C).

/*
 * b) Define a predicate usage/2 that gives for a classroom all 
 * the times it is used. For example, the goal usage(cov216,T) 
 * should yield the following result.
 */

usage(A,B) :- where(Z, A), when(Z, B).

/*
 * c) Define a predicate conflict/2 that can compute conflicts in
 *  the assignment of classes to classrooms. A conflict exists if 
 * two different classes are assigned to one classroom for the
 * same time. The arguments of the conflict predicate are two 
 * class names. You can use the goal conflict(275,X) (or 
 * conflict(X,275)) to find out any classes that are in 
 * conflict with the class 275.
 */

conflict(A,B) :- when(A, Z), when(B,Z), A\==B.

/*
 * d) Define a predicate meet/2 that can determine pairs of 
 * students that can meet in a classroom by either attending 
 * the same class or by having classes that are back to back 
 * in one classroom. The last condition means that a student 
 * Jim can meet any student who has a class that is in the 
 * same classroom and immediately follows Jim’s class. 
 * (Note that your definition of meet doesn’t have to be 
 * symmetric, that is, if students A and B can meet, then 
 * your implementation has to return Yes for meet(A,B) or 
 * meet(B,A), but not necessarily for both calls. You can 
 * ignore the case when students are enrolled in conflicting 
 * classes.)
 */

meet(A,B) :- enroll(A,X), enroll(B,X), A\==B.
meet(A,B) :- enroll(A,X), enroll(B,Y), when(X,Z), when(Y,Z+1), where(X, W), where(Y,W), A\==B.
meet(A,B) :- enroll(A,X), enroll(B,Y), when(X,Z+1), when(Y,Z), where(X, W), where(Y,W), A\==B.

/* Exercise 2 */

/*
 * a) Define a Prolog predicate rdup(L,M) to remove duplicates 
 * from an ordered list L. The resulting list should be bound 
 * to M. Note that M must contain each element of L exactly 
 * once and in the same order as in L. You can assume that L 
 * is an ordered list.
 */

rdup([X],[M]) :- X = M.
rdup([X,Y|Z],[A|B]) :- X \== Y,  X = A, rdup([Y|Z],B). 
rdup([X,Y|Z],M) :- X == Y, rdup([Y|Z],M). 

/*
 * b) Define a Prolog predicate flat(L,F) that binds to F the 
 * flat list of all elements in L (where L can be a possibly 
 * nested list). For example, flat([a,b,[c,d],[],[[[e]]],f],L) 
 * yields L = [a,b,c,d,e,f].
 */

flat([X],[L]) :- not(is_list(X)), L = X.
flat([X],[L]) :- is_list(X), flat(X,[L]).
flat([X|Y],[A|B]) :- not(is_list(X)), A = X, flat(Y,B).
flat([X|Y],L) :- is_list(X), append(Y, X, Z), flat(Z,L).

/*
 * c) Define a Prolog predicate project/3 that selects elements
 * from a list by their position and collects them in a result 
 * list. For example, the goal project([2,4,5],[a,b,c,d],L) 
 * should produce the answer L=[b,d]. You can assume that the 
 * numbers in the first list are strictly inreasing, that is, 
 * your implementation does not have to care about situations 
 * like project([1,1,2],...) or project([2,4,3],...).
 */

project(1,[X|_],L):- L = X.
project(P,[_|Y],L) :- integer(P), P > 1, NEXTP is P-1, Y \= [], project(NEXTP,Y,L).
project([P|_],B, []) :- integer(P), length(B, Q), Q < P.
project([P|[]],B, [L]) :- project(P,B,L).
project([P|[Pn|_]], B, [N]) :- integer(P), integer(Pn), length(B, Q), Q < Pn, project(P, B, N).
project([P|[Pn|X]], B, [N|L]) :- integer(P), length(B, Q), Q >= P, integer(Pn),  Q >= Pn,  project(P, B, N), project([Pn|X], B, L).
