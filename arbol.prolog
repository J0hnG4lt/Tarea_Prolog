cola([],[]) :- !.
cola([_ | XS], XS) :- !.

cabeza([], _) :- fail.
cabeza([X | _], X) :- !.

extraerNodo(arista(_, Nodo), Nodo).

extraerEtiquetaDeNodo(nodo(R, _), R).

extraerEtiquetaDeArista(arista(EA, _), EA).

bienEtiquetado(nodo(_, [])) :-!.
bienEtiquetado(nodo(EtiquetaNodoPadre, Aristas)) :-
    cabeza(Aristas, Arista),
    cola(Aristas, RestoAristas),
    extraerEtiquetaDeArista(Arista, EtiquetaArista),
    extraerNodo(Arista, NodoHijo),
    extraerEtiquetaDeNodo(NodoHijo, EtiquetaNodoHijo),
    Diferencia is EtiquetaNodoPadre - EtiquetaNodoHijo, 
    EtiquetaArista is abs(Diferencia),
    bienEtiquetado(NodoHijo),
    bienEtiquetado(nodo(EtiquetaNodoPadre, RestoAristas)).

longitud([], 0).
longitud([_ | XS], L) :- longitud(XS, L2), L is L2 + 1.
%longitud([_ | XS], L) :- L2 is L + 1, longitud(XS, L2).

extraerNiveles(esq(Niveles), Niveles).


verificarNiveles([], _) :- !.
verificarNiveles([X | XS], NumeroMaxHijos) :-
    verificarNumeroHijos(X, NumeroMaxHijos),
    verificarNiveles(XS, NumeroMaxHijos).

verificarNumeroHijos([], _) :- !.
verificarNumeroHijos([X | XS], NumeroMaxHijos) :-
    X =< NumeroMaxHijos,
    verificarNumeroHijos(XS, NumeroMaxHijos).


numeroHijos(nodo(_, Aristas), NumeroHijos) :- 
    longitud(Aristas, NumeroHijos).


extraerN_Elem(0, [X | _], X) :- !.
extraerN_Elem(Numero, [_ | XS], R) :- 
    Numero_2 is Numero - 1,
    extraerN_Elem(Numero_2, XS, R).



etiquetamiento(Esqueleto, Arbol) :-
    bienEtiquetado(Arbol),
    numeroNiveles(Esqueleto, NumeroNiveles),
    construirListaNiveles(NumeroNiveles, [_ | RestoNiveles]),
    numeroHijos(Arbol, NumHijosRaiz),
    extraerNiveles(Esqueleto, Niveles),
    extraerN_Elem(0, Niveles, [NumHijosRaiz]),
    corresponden(Esqueleto, Arbol, RestoNiveles, [[0, 1]], _, _).


numeroNiveles(esq(Niveles), NumeroNiveles) :-
    longitud(Niveles, NumeroNiveles).

construirListaNiveles(0, _) :- !.
construirListaNiveles(CantidadNiveles, Resultado) :-
    CantidadNiveles2 is CantidadNiveles -1,
    construirListaNiveles(CantidadNiveles2, ListaParesNumNivelVSNumNodosVistos),
    append(ListaParesNumNivelVSNumNodosVistos, [[CantidadNiveles2, 0]], Resultado),!.

corresponden(_, nodo(_, []), NivelesPorVer, NivelesVistos, NivelesPorVer, NivelesVistos) :- !.
corresponden(esq(Niveles), nodo(E, [arista(_, Hijo) | Aristas]), [ [NumNivel, NumNodosVistosEnNivel] | NivelesPorVer], NivelesVistos, NivPV,NivV) :-
    NumNodosVistosEnNivelNuevo is NumNodosVistosEnNivel +1,
    extraerN_Elem(NumNivel, Niveles, ListaNodosNivelN),
    extraerN_Elem(NumNodosVistosEnNivelNuevo, ListaNodosNivelN, NumHijosNodoActualEsqueleto),
    numeroHijos(Hijo, NumeroAristasHijo),
    NumeroAristasHijo = NumHijosNodoActualEsqueleto,
    corresponden(esq(Niveles), Hijo, NivelesPorVer, [[NumNivel, NumNodosVistosEnNivelNuevo] | NivelesVistos], NivPV_2, NivV_2),
    corresponden(esq(Niveles), nodo(E, Aristas), NivPV_2, NivV_2, NivPV, NivV).

numeroElementosTotalesEsq([], 0) :- !.
numeroElementosTotalesEsq([Nivel | Niveles], Cantidad) :-
    longitud(Nivel, NumeroNodos),
    numeroElementosTotalesEsq(Niveles, Cantidad2),
    Cantidad is NumeroNodos +Cantidad2.

esqueleto(0, NumeroMaxHijosPorNodo, esq([])) :- NumeroMaxHijosPorNodo > -1.
esqueleto(CantidadTotalNodos, NumeroMaxHijosPorNodo, Esqueleto) :-
    extraerNiveles(Esqueleto, Niveles),
    numeroElementosTotalesEsq(Niveles, CantidadTotalNodos),
    verificarNiveles(Niveles, NumeroMaxHijosPorNodo).


esqEtiquetables(R, N, ListaArboles) :-
    esqueleto(N, R, Esqueleto),
    findall(Arbol, etiquetamiento(Esqueleto, Arbol), ListaArboles).





