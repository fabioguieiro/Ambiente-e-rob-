agente :-
   inicia,
   new(D,dialog('Agente')),
   send(D,append,bitmap(image('ambiente.bmp'))),
   send(D,display,new(@o1,box(20,20))),
   send(@o1,fill_pattern,colour(yellow)),
   send(@o1,move,point(25,25)),
   send(D,display,new(@o2,box(20,20))),
   send(@o2,fill_pattern,colour(green)),
   send(@o2,move,point(25,45)),
   send(D,display,new(@o3,box(20,20))),
   send(@o3,fill_pattern,colour(red)),
   send(@o3,move,point(25,65)),
   send(D,append,new(@a,bitmap(image('agente.bmp')))),
   send(@a,move,point(112, 266)),




   new(@t,timer(1)),
   send(@t,start),
   send(D,open).

:- dynamic pos/2, seg/1.

inicia :-
   forall(member(X,[@t,@a,@o1,@o2,@o3]),free(X)),
   retractall(pos(_,_)),
   retractall(seg(_)),
   assert(pos(agente,14)),
   assert(pos(1,1)),
   assert(pos(2,1)),
   assert(pos(3,1)).


porta(1,2,1).
porta(2,6,2).
porta(2,3,1).
porta(3,7,2).
porta(3,4,1).
porta(7,8,1).
porta(5,6,1).
porta(5,9,3).
porta(9,10,1).
porta(9,13,2).
porta(10,14,2).
porta(14,15,1).
porta(15,11,2).
porta(11,12,1).
porta(12,16,2).
porta(15,16,1).
porta(14,18,3).
porta(17,18,1).
porta(17,21,2).
porta(18,22,2).
porta(18,19,1).
porta(19,23,2).
porta(19,20,1).
porta(20,24,2).
porta(23,24,1).
porta(23,27,3).
porta(27,28,1).
porta(28,32,2).
porta(32,31,1).
porta(27,31,2).
porta(27,26,1).
porta(31,30,1).
porta(26,30,2).
porta(26,25,1).
porta(25,29,2).

passagem(X,Y) :- porta(X,Y,_).
passagem(X,Y) :- porta(Y,X,_).

rota(X,X,[X]) :- !.
rota(X,Y,[X|R]) :- passagem(X,Z), rota(Z,Y,R).

% mudar caixas de lugar





% comandos para o agente

ande(L) :-
   pos(agente,L), !.
ande(L) :-
   retract(pos(agente,P)),
   assert(pos(agente,L)),
   length(R,_),
   rota(P,L,R),
   siga(R), !.

pegue(O) :-
   seg(O), !.
pegue(O) :-
   pos(O,P),
   ande(P),
   retract(pos(O,_)),
   assert(seg(O)),
   get(@a,position,X),
   obj(O,No,_),
   send(No,move,X),
   ande(14),
   solte(O),!.


solte(O) :-
   not(seg(O)), !.
solte(O) :-
   pos(agente,P),
   not(member(P,[5,6])),
   retract(seg(O)),
   assert(pos(O,P)),
   get(@a,position,point(X,Y)),
   obj(O,No,Yo), X1 is X-10, Y1 is Y+Yo,
   send(No,move,point(X1,Y1)), !.

siga([]).
siga([S|R]) :- mova(S), send(@t,delay), siga(R).

mova(S) :-
   sala(S,X,Y),
   forall(seg(O),(obj(O,No,_),send(No,move,point(X,Y)))),
   send(@a,move,point(X,Y)).


setbox(A,S):- A=:=1,
   retract(pos(1,1)),
   assert(pos(A,S)),
   sala(S,X,Y),
   forall(seg(O),(obj(O,No,_),send(No,move,point(X,Y)))),
   X1 is X, Y1 is Y-20,
   send(@o1,move,point(X1,Y1)),!.

setbox(A,S):- A=:=2,
   retract(pos(2,1)),
   assert(pos(A,S)),
   sala(S,X,Y),
   forall(seg(O),(obj(O,No,_),send(No,move,point(X,Y)))),
   send(@o2,move,point(X,Y)),!.

setbox(A,S):- A=:=3,
   retract(pos(3,1)),
   assert(pos(A,S)),
   sala(S,X,Y),
   forall(seg(O),(obj(O,No,_),send(No,move,point(X,Y)))),
    X1 is X, Y1 is Y+20,

   send(@o3,move,point(X1,Y1)),!.


busca() :-
	pegue(1),
	pegue(2),
	pegue(3),
	ande(15),!.


% assert(pos(A,S)).


sala(1,  36,  38).
sala(2, 112,  38).
sala(3, 188,  38).
sala(4, 264, 38).
sala(5,  36, 114).
sala(6, 112, 114).
sala(7, 188, 114).
sala(8, 264, 114).
sala(9, 36, 190).
sala(10, 112, 190).
sala(11, 188, 190).
sala(12, 264, 190).
sala(13, 36, 266).
sala(14, 112, 266).
sala(15, 188, 266).
sala(16, 264, 266).
sala(17, 36, 342).
sala(18, 112, 342).
sala(19, 188, 342).
sala(20, 264, 342).
sala(21, 36, 418).
sala(22, 112, 418).
sala(23, 188, 418).
sala(24, 264, 418).
sala(25, 36, 494).
sala(26, 112, 494).
sala(27, 188, 494).
sala(28, 264, 494).
sala(29, 36, 570).
sala(30, 112, 570).
sala(31, 188, 570).
sala(32, 264, 570).


obj(1, @o1, -20).
obj(2, @o2,   0).
obj(3, @o3, +20).







