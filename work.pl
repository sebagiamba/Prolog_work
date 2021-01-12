% Para limpiar la consola.
cls :- write('\33\[2J').

% Unidades
% Este predicado indica cuáles son las unidades militares disponibles.
unidad(lancero).
unidad(arquero).
unidad(jinete).
unidad(guerrillero).

% Edificios
% Este predicado indica cuáles son los edificios que se pueden construir.
edificio(cuartel).
edificio(arqueria).
edificio(establo).

% Entrenamiento
% Este predicado indica en qué edificio se entrena cada unidad.
entrena(lancero,      cuartel).
entrena(arquero,      arqueria).
entrena(guerrillero,  arqueria).
entrena(jinete,       establo).

% Costos
% Este predicado indica el costo de cada unidad y de cada edificio.
costo(lancero,      80).
costo(arquero,      90).
costo(guerrillero,  70).
costo(jinete,       120).
costo(cuartel,      300).
costo(arqueria,     330).
costo(establo,      400).

% Ej 1 : costo para listas (tanto de batallones como de edificios)
% costo ( +L , -C )
% Enunciado: Extender el predicado "costos" para que funcione con listas, tanto de batallones como de edificios.

costo([],0).

%lista de edificios
costo([E | L], C) :- edificio(E), costo(E, C1), costo(L, C2), C is C1 + C2.

%lista de batallones
costo([(U, CANT) | L], C) :- costo(U, C1), costo(L, C2), C is C1 * CANT + C2.


% Ej 2 : instanciar un ejército arbitrario
% ejercito ( -E )

ejercito(E) :- var(E), desde(1,X), ejercitoDeNUnidades(E,X).
ejercito(E) :- nonvar(E), desde(1,X), ejercitoDeNUnidades(E,X), !.

%ejercitoDeNUnidades(?B , +C)
ejercitoDeNUnidades([],0).
ejercitoDeNUnidades([(U, CANT) | L], C) :- between(1,C,CANT), unidad(U), C2 is C-CANT, ejercitoDeNUnidades(L,C2).

%desde(X+, Y-)
desde(X,X).
desde(X,Y) :- N is X + 1, desde(N,Y).

% Reversibilidad: 
% ejercito(E) no es reversible debido a que tiene árbol de búsqueda infinito originado por el predicado desde(1,X).
% Entonces, si bien ejercitoDeNUnidades es reversible en su primer parámetro, si E viene instanciado en un ejército válido, 
% va a dar true eventualmente, pero si se piden más resultados no va a terminar. 
% Por otro lado, si E viene instanciado con otra cosa que no sea un ejército válido, la búsqueda nunca termina.

% Una opción para hacerlo reversible podría ser chequear si E viene instanciado (usando var y nonvar). 
% Para el caso nonvar se podría simplemente chequear que el ejercito recibido sea válido, es decir, que cumpla las condiciones 
% que hacen a un ejercito. De esta forma se evitaría entrar en una búsqueda.

% Ej 3 : instancia una lista de edificios necesarios para el ejército
% edificiosNecesarios ( +Ej , -Ed )

edificiosNecesarios([], []).
edificiosNecesarios([(U, _) | L], Ed) :- entrena(U, E1), edificiosNecesarios(L, E2), union([E1], E2, Ed).

% Reversibilidad: 
% Ej no es reversible. Si bien la ejecución del predicado con Ej sin instanciar no arroja error, ocurre que Ej no se va a instanciar
% con todos los posibles ejércitos (como uno esperaría si el predicado fuera reversible).
% Esto se debe a que el predicado entrena(U,E1) de la regla recursiva unifica con el primer hecho, instanciando a U con lancero, y 
% luego se llama recursivamente. Como L está sin instanciar, el llamado recursivo se comporta igual al llamado anterior, instanciando
% U con lancero. El proceso se repite para todos los llamados recursivos.
% Además, la variable que representa la cantidad de unidades de cada batallón no se unifica con un valor, y entonces queda variable.

% Ed no es reversible, debido a que no nos interesa el orden de la lista. Dependiendo del orden en la lista instanciada en Ed, el predicado puede dar resultados diferentes.


%(c)
% edificiosNecesarios2( -Ej, -Ed )
edificiosNecesarios2(Ej, Ed) :- ejercito(Ej), edificiosNecesarios(Ej, Ed).

%Debido a lo explicado en el ejercicio 2, y como se usa el predicado ejercito(Ej), este predicado no es reversible en el primer parámetro.


% Ej 4 : índice de superioridad para unidades
% ids ( +A , +B , -I )
% Enunciado:
  % Se cuenta con el predicado "ids" que calcula el IdS de una unidad sobre otra instanciando dicho valor en el tercer parámetro.
  % Sin embargo, este sólo funciona en algunos casos particulares.
  % Completar y/o modificar la implementación de este predicado para que:
  % a) funcione cuando los primeros dos argumentos corresponden a la misma unidad.
  % En este caso se debe instanciar el tercer parámetro en 1.
  % b) funcione cuando el par de los primeros dos argumentos se corresponde a uno de los ya contemplados pero en el orden inverso.
  % En este caso se debe instanciar el tercer parámetro con el inverso multiplicativo del caso contemplado.
  % c) no se cuelgue ni genere soluciones repetidas.
idsAux(jinete,       arquero,      1.5).
idsAux(jinete,       guerrillero,  0.5).
idsAux(lancero,      jinete,       2).
idsAux(lancero,      arquero,      0.6).
idsAux(guerrillero,  lancero,      1.1).
idsAux(guerrillero,  arquero,      2).

ids(A,B,I) :- idsAux(A,B,I).

%(a)
ids(A, A, 1) :- unidad(A).

%(b)
ids(A, B, I) :- idsAux(B, A, J), J \= 0, I is 1/J.

% Reversibilidad:
% El predicado es reversible. Esto se debe a que las variables unifican siempre con argumentos de hechos y en el caso de la última
% regla, J se instancia previamente antes de ejecutarse la operación aritmética 1/J.
% ACLARACIÓN: no se está teniendo en cuenta la extensión de la regla ids del ejercicio 5 para el análisis de reversibilidad.

% Ej 5
% ids ( +A , +B , -I )
ids((UA,CA),(UB,CB),Ib) :- ids(UA,UB,Iu), Ib is Iu * (CA / CB).

% gana ( +A , +B )
gana(A,B) :- ids(A,B,I), I >= 1.
gana(_,[]) :- !.
gana([A|AS],[B|BS]) :- gana(A,B), gana([A|AS],BS), !.
gana([A|AS],[B|BS]) :- gana(B,A), gana(AS,[B|BS]), !.

% ganaA ( ?A , +B , ?N )

%caso B es batallon 
ganaA((U, N), (Un, C), N) :- between(1, C, N), unidad(U), gana((U, N), (Un, C)).

%caso B es ejercito
ganaA(A,B,N) :- nonvar(N), ejercitoDeNUnidades(A,N), gana(A,B).
ganaA(A,B,N) :- var(N), cantidadUnidades(B,CB), between(1,CB,N), ganaA(A,B,N).

%cantidadUnidades ( +L , -N )
cantidadUnidades([], 0).
cantidadUnidades([(_,C)|L], CU) :- cantidadUnidades(L,N2), CU is N2 + C.

% ¿Usaron "ejercito"? ¿por qué?
% NO porque ejercito genera todos los posibles ejércitos, y solo se está buscando los que tengan una determinada cantidad de 
% unidades (aun así, usamos ejercitoDeNUnidades que es una auxiliar hecha para ejercito). 
% El motivo principal es que ejercito nunca termina, entonces si usamos ejercito se cuelga.
% Formalmente sería un error de uso del esquema generate and test, dado que estaríamos generando un universo de búsqueda infinito
% para luego quedarnos con finitos resultados.

% Ej 6 : instancia un pueblo para derrotar a un ejército enemigo
% puebloPara ( +En , ?A , -Ed , -Ej )
puebloPara(En, A, Ed, Ej) :- ganaA(Ej, En, _), edificiosNecesarios(Ej, Ed), costo(Ej, C1), costo(Ed, C2), A2 is ceiling((C1+C2)/50), esMenor(A2,A).

%esMenor( +X, ?Y ).
esMenor(X,Y) :- between(X,inf,Y), !.
%Si Y está instanciado, entonces X =< Y. Si Y no está instanciado, lo instancia al valor de X y solo a ese valor.


% Ej 7 : pueblo óptimo (en cantidad de aldenos necesarios)
% puebloOptimoPara( +En , ?A , -Ed , -Ej )

puebloOptimoPara(En, A, Ed, Ej) :- puebloPara(En, A, Ed, Ej), not(hayPuebloMasOptimo(En, A)).

hayPuebloMasOptimo(En, A) :- puebloPara(En,A2,_,_), A2 < A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsCosto(10).
testCosto(1) :- costo([(arquero, 2)], 180).
testCosto(2) :- costo([cuartel], 300).
testCosto(3) :- costo([establo, cuartel], 700).
testCosto(4) :- costo([establo, cuartel, arqueria], 1030).
testCosto(5) :- costo([(lancero, 5), (arquero, 2)], 580).
testCosto(6) :- costo([(guerrillero, 7), (jinete, 1)], 610).
testCosto(7) :- costo([], 0).
testCosto(8) :- costo([cuartel, arqueria], 630).
testCosto(9) :- costo([(lancero, 1), (arquero, 77), (jinete, 2), (arquero, 8)], 7970).
testCosto(10) :- costo([(guerrillero, 2),(lancero, 3), (guerrillero, 4), (jinete, 5)], 1260).

cantidadTestsEjercito(5).
testEjercito(1) :- ejercito([(lancero, 1), (jinete, 3)]), !.
testEjercito(2) :- ejercito([(jinete, 5)]), !.
testEjercito(3) :- ejercito([(guerrillero, 4), (guerrillero, 2)]), !.
testEjercito(4) :- ejercito([(arquero, 1)]), !.
testEjercito(5) :- ejercito([(arquero, 4), (guerrillero, 3), (jinete, 12), (lancero, 5)]), !.

cantidadTestsEdificios(5).
testEdificios(1) :- edificiosNecesarios([(arquero, 2), (guerrillero, 2)], [arqueria]).
testEdificios(2) :- edificiosNecesarios([(arquero, 11)], [arqueria]).
testEdificios(3) :- edificiosNecesarios([(guerrillero, 3), (lancero, 3)], Ed), mismos(Ed,[arqueria, cuartel]).
testEdificios(4) :- edificiosNecesarios([(guerrillero, 3), (lancero, 3)], Ed), mismos(Ed, [cuartel, arqueria]).
testEdificios(5) :- edificiosNecesarios([(lancero, 1), (jinete, 10)], Ed), mismos(Ed, [establo, cuartel]).

% Auxiliar para chequear si tienen los mismos elementos
mismos(A,B) :- inc(A,B), inc(B,A).
inc([],_).
inc([A|As],Bs) :- member(A,Bs), inc(As,Bs).

cantidadTestsIdS(8).
testIdS(1) :- ids(jinete, jinete, X), X=:=1.
testIdS(2) :- ids(jinete, lancero, X), X=:=0.5.
testIdS(3) :- ids(lancero, jinete, X), X=:=2.
testIdS(4) :- ids(guerrillero, guerrillero, X), X=:=1.
testIdS(5) :- ids(lancero, guerrillero, X), X=:=0.9090909090909091.
testIdS(6) :- ids(arquero, lancero, X), X=:=1.6666666666666667.
testIdS(7) :- ids(arquero, guerrillero, X), X=:=0.5.
testIdS(8) :- ids(lancero, lancero, X), X=:=1.

cantidadTestsGanaA(5).
testGanaA(1) :- ganaA(E, (jinete, 3), 3), gana(E, (jinete, 3)), !.
testGanaA(2) :- not(ganaA(_, (guerrillero, 7), 6)).
testGanaA(3) :- ganaA(E, [(arquero, 1), (jinete, 1), (lancero, 1)], 2), gana(E, [(arquero, 1), (jinete, 1), (lancero, 1)]), !.
testGanaA(4) :- not(ganaA((guerrillero, 2),[(arquero, 2), (lancero, 4), (jinete, 6)], 2)).
testGanaA(5) :- not(ganaA([(arquero, 2), (jinete, 2), (guerrillero, 2)], [(lancero, 6)], 6)).

cantidadTestsPueblo(4).
testPueblo(1) :- En=[(jinete, 3)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej, En), !.
testPueblo(2) :- En=[(arquero, 1), (lancero, 4)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.
testPueblo(3) :- En=[(guerrillero, 5)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.
testPueblo(4) :- En=[(jinete, 1), (lancero, 1), (guerrillero, 2), (arquero, 2)],
  puebloPara(En, A, Ed, Ej),
  costo(Ej, Ced), costo(Ej, Cej), C is Ced+Cej, A*50 >= C,
  edificiosNecesarios(Ej, Ed), gana(Ej,En), !.

cantidadTestsPuebloOptimo(5).
testPuebloOptimo(1) :- En=[(jinete,2)], puebloOptimoPara(En,8,[cuartel],[(lancero,1)]), !.
testPuebloOptimo(2) :- En=[(jinete,2)], puebloOptimoPara(En,8,[arqueria],[(guerrillero,1)]), !.%Si no optimizan recursos.
testPuebloOptimo(3) :- En=[(arquero,2)], puebloOptimoPara(En,8,[arqueria],[(guerrillero,1)]), !.
testPuebloOptimo(4) :- En=[(guerrillero, 2), (arquero, 3)], puebloOptimoPara(En, 10, [arqueria], [(guerrillero, 2)]), !.
testPuebloOptimo(5) :- En=[(arquero,4)], not(puebloOptimoPara(En,5,_,_)).

tests(costo) :- cantidadTestsCosto(M), forall(between(1,M,N), testCosto(N)).
tests(ejercito) :- cantidadTestsEjercito(M), forall(between(1,M,N), testEjercito(N)).
tests(edificios) :- cantidadTestsEdificios(M), forall(between(1,M,N), testEdificios(N)).
tests(ids) :- cantidadTestsIdS(M), forall(between(1,M,N), testIdS(N)).
tests(ganaA) :- cantidadTestsGanaA(M), forall(between(1,M,N), testGanaA(N)).
tests(pueblo) :- cantidadTestsPueblo(M), forall(between(1,M,N), testPueblo(N)).
tests(puebloOptimo) :- cantidadTestsPuebloOptimo(M), forall(between(1,M,N), testPuebloOptimo(N)).

tests(todos) :-
  tests(costo),
  tests(ejercito),
  tests(edificios),
  tests(ids),
  tests(ganaA),
  tests(pueblo),
  tests(puebloOptimo).

tests :- tests(todos).
