/* Se interoghează run(E), unde E este o expresie formată după regulile:
   <expr> -> <număr> | <var> |
             (<var> -> <expr>) |
             <expr> $ <expr> |
             <expr> + <expr>
   <var>  -> <atom din Prolog>
   
   Pentru exemplul din model:
   ?- run(x -> (y -> (x -> x + z) $ ((x -> x $ x) $ (x + y) + x)) $
      (z -> x $ y $ z)).
*/


:- op(200,  yfx, $).


/* legaturi(Exp, L)
   Descriere: L conține variabilele indexate din Exp, asociate cu una dintre
   stările 'libera', 'de_legatura', 'legata(X, I)'.
*/
legaturi(N, []) :- integer(N).
legaturi((X, I), [((X, I), libera)]).

legaturi(X + Y, L) :- legaturi(X, L1),
                      legaturi(Y, L2),
                      append(L1, L2, L).

legaturi(X $ Y, L) :- legaturi(X, L1),
                      legaturi(Y, L2),
                      append(L1, L2, L).

legaturi((X, I) -> Exp, [((X, I), de_legatura) | L]) :-
    legaturi(Exp, L1),
    legate_de(X, I, L1, L).


/* legate_de(X, I, L1, L2)
   Descriere: L2 este L1 în care elementele ((X, J), libera) sunt înlocuite cu
   ((X, J), legata(X, I)).
*/
legate_de(_, _, [], []).
legate_de(X, I, [((X, J), libera) | T1], [((X, J), legata(X, I)) | T2]) :-
    legate_de(X, I, T1, T2), !.
legate_de(X, I, [((Y, J), Stare) | T1], [((Y, J), Stare) | T2]) :-
    legate_de(X, I, T1, T2).


/* numerotare(I, J, E, E1)
   Descriere: E1 este E în care atomii X sunt înlocuiți, de la stânga la
   dreapta, cu perechi (X, K), pentru întregi consecutivi K între I și J
   inclusiv.
*/
numerotare(I, I, N, N) :- integer(N).
numerotare(I, J, X, (X, I)) :- atom(X), J is I + 1.
numerotare(I, J, E + F, E1 + F1) :- numerotare(I, K, E, E1),
                                    numerotare(K, J, F, F1).
numerotare(I, J, E $ F, E1 $ F1) :- numerotare(I, K, E, E1),
                                    numerotare(K, J, F, F1).
numerotare(I, J, X -> E, (X, I) -> E1) :- K is I + 1,
                                          numerotare(K, J, E, E1).


run(Exp) :- numerotare(1, _, Exp, E),
            legaturi(E, L),
            afis(L).

afis([]).
afis([((X, I), libera) | T]) :- write(X), write(I),
                                write(': libera\n'),
                                afis(T).
afis([((X, I), de_legatura) | T]) :- write(X), write(I),
                                     write(': de legatura\n'),
                                     afis(T).
afis([((X, I), legata(X, J)) | T]) :- write(X), write(I),
                                      write(': legata de '), write(X), write(J),
                                      nl, afis(T).
