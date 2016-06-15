
%% Autor: Georvic Tur
%% Carnet: 12-11402
%% Correo: alexanderstower@gmail.com



cola([],[]) :- !.
cola([_ | XS], XS) :- !.

cabeza([], _) :- fail.
cabeza([X | _], X) :- !.

extraerNodo(arista(_, Nodo), Nodo).

extraerEtiquetaDeNodo(nodo(R, _), R).

extraerEtiquetaDeArista(arista(EA, _), EA).



listaEtiquetas(nodo(E,[]), [E]).
listaEtiquetas(nodo(E,[arista(_, Nodo) | Aristas]), L):-
    listaEtiquetas(Nodo, L2),
    listaEtiquetas(nodo(E,Aristas), L3),
    append(L2,L3,L), !.

noHayRepetidas([]).
noHayRepetidas([X|XS]) :-
    \+ member(X,XS),
    noHayRepetidas(XS).

listaEtiquetasAristas(nodo(E,[]), []).
listaEtiquetasAristas(nodo(E2,[arista(E, Nodo) | Aristas]), [E|L]):-
    listaEtiquetasAristas(Nodo, L2),
    listaEtiquetasAristas(nodo(E2,Aristas), L3),
    append(L2,L3,L), !.

etiquetasUnicasAristas(Arbol) :-
    listaEtiquetasAristas(Arbol, Lista),
    noHayRepetidas(Lista).

etiquetasUnicasNodos(Arbol) :-
    listaEtiquetas(Arbol, Lista),
    noHayRepetidas(Lista).


bienEtiquetado(Arbol) :-
    bienEtiquetado2(Arbol),
    etiquetasUnicasAristas(Arbol),
    etiquetasUnicasNodos(Arbol).

bienEtiquetado2(nodo(_, [])) :-!.
bienEtiquetado2(nodo(EtiquetaNodoPadre, Aristas)) :-
    cabeza(Aristas, Arista),
    cola(Aristas, RestoAristas),
    extraerEtiquetaDeArista(Arista, EtiquetaArista),
    extraerNodo(Arista, NodoHijo),
    extraerEtiquetaDeNodo(NodoHijo, EtiquetaNodoHijo),
    Diferencia is EtiquetaNodoPadre - EtiquetaNodoHijo, 
    EtiquetaArista is abs(Diferencia),
    bienEtiquetado(NodoHijo),
    bienEtiquetado(nodo(EtiquetaNodoPadre, RestoAristas)),!.

longitud([], 0).
longitud([_ | XS], L) :- longitud(XS, L2), L is L2 + 1,!.
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
    extraerN_Elem(Numero_2, XS, R), !.


/*
etiquetamiento(Esqueleto, Arbol) :-
    bienEtiquetado(Arbol),
    numeroNiveles(Esqueleto, NumeroNiveles),
    construirListaNiveles(NumeroNiveles, [_ | RestoNiveles]),
    numeroHijos(Arbol, NumHijosRaiz),
    extraerNiveles(Esqueleto, Niveles),
    extraerN_Elem(0, Niveles, [NumHijosRaiz]),
    corresponden(Esqueleto, Arbol, RestoNiveles, [[0, 1]], _, _).
*/

numeroNiveles(esq(Niveles), NumeroNiveles) :-
    longitud(Niveles, NumeroNiveles).

construirListaNiveles(0, _) :- !.
construirListaNiveles(CantidadNiveles, Resultado) :-
    CantidadNiveles2 is CantidadNiveles -1,
    construirListaNiveles(CantidadNiveles2, ListaParesNumNivelVSNumNodosVistos),
    append(ListaParesNumNivelVSNumNodosVistos, [[CantidadNiveles2, 0]], Resultado),!.


corresponden(_, nodo(_, []), NivelesPorVer, NivelesVistos, NivelesPorVer, NivelesVistos) :- !.
corresponden(esq(Niveles), nodo(E, [arista(_, Hijo) | Aristas]), [ [NumNivel, NumNodosVistosEnNivel] | NivelesPorVer], NivelesVistos, ResNiPV,NivV) :-
    NumNodosVistosEnNivelNuevo is NumNodosVistosEnNivel +1,
    extraerN_Elem(NumNivel, Niveles, ListaNodosNivelN),
    extraerN_Elem(NumNodosVistosEnNivel, ListaNodosNivelN, NumHijosNodoActualEsqueleto),
    numeroHijos(Hijo, NumeroAristasHijo),
    NumeroAristasHijo =:= NumHijosNodoActualEsqueleto,
    corresponden(esq(Niveles), Hijo, NivelesPorVer, [[NumNivel, NumNodosVistosEnNivelNuevo] | NivelesVistos], NivPV_2, [C|NivV_2]),
    (
    ( corresponden(esq(Niveles), nodo(E, Aristas), [C|NivPV_2], NivV_2, NivPV, [D|NivV]),
    ResNiPV=[D|NivPV])
    ;
    ( corresponden(esq(Niveles), nodo(E, Aristas), [C|NivPV_2], NivV_2, NivPV, NivV),
    ResNiPV=NivPV)),!.
    

numeroElementosTotalesEsq([], 0) :- !.
numeroElementosTotalesEsq([Nivel | Niveles], Cantidad) :-
    longitud(Nivel, NumeroNodos),
    numeroElementosTotalesEsq(Niveles, Cantidad2),
    Cantidad is NumeroNodos +Cantidad2.

/*
esqueleto(0, NumeroMaxHijosPorNodo, esq([])) :- NumeroMaxHijosPorNodo > -1.
esqueleto(CantidadTotalNodos, NumeroMaxHijosPorNodo, Esqueleto) :-
    extraerNiveles(Esqueleto, Niveles),
    numeroElementosTotalesEsq(Niveles, CantidadTotalNodos),
    verificarNiveles(Niveles, NumeroMaxHijosPorNodo).
*/

/*
generar_nivel(_,_,0,_).
generar_nivel(0, Aridad, NumElemsAPoner, [0 | Elems]) :- 
    NumElemsAPoner > 0,
    NumElemsAPoner2 is NumElemsAPoner -1,
    generar_nivel(0, Aridad, NumElemsAPoner2, Elems).
generar_nivel(NumNodosDisp,Aridad, NumElemsAPoner,[X | Elems]) :-
    NumElemsAPoner > 0,
    NumElemsAPoner2 is NumElemsAPoner -1,
    ((Aridad =< NumNodosDisp,
      X = Aridad,
      NumNodosDisp2 is NumNodosDisp - Aridad)
      ;
     (Aridad > NumNodosDisp,
      X = NumNodosDisp,
      NumNodosDisp2 is 0)
    ),
    generar_nivel(NumNodosDisp2, Aridad, NumElemsAPoner2, Elems).


sumar_nivel([], 0).
sumar_nivel([X|Nivel],NumElems) :-
    sumar_nivel(Nivel, NumElems2),
    NumElems is NumElems2 + X.

generar_niveles(NumTotalNodosDisponibles, Aridad, NumElemsAPoner, [Nivel]) :-
    NumElemsAPoner > NumTotalNodosDisponibles,
    generar_nivel(NumTotalNodosDisponibles,Aridad, NumElemsAPoner, Nivel).

generar_niveles(NumTotalNodosDisponibles, Aridad, NumElemsAPoner, [[X]|Niveles]) :-
    NumElemsAPoner = 1,
    NumElemsAPoner =< NumTotalNodosDisponibles,
    ((Aridad < NumTotalNodosDisponibles,
      X is Aridad,
      NodosRestantes is NumTotalNodosDisponibles - Aridad)
      ;
      (Aridad >= NumTotalNodosDisponibles,
      X is NumTotalNodosDisponibles,
      NodosRestantes is 0
    )),
    generar_niveles(NodosRestantes, Aridad, X, Niveles).

generar_niveles(NumTotalNodosDisponibles, Aridad, NumElemsAPoner, Niveles) :-
    NumElemsAPoner > 1,
    NumElemsAPoner =< NumTotalNodosDisponibles,
    generar_nivel(NumTotalNodosDisponibles,Aridad, NumElemsAPoner, Nivel),
    sumar_nivel(Nivel, NumNodosUsados),
    NodosRestantes2 is NumTotalNodosDisponibles -NumNodosUsados,
    ((
      NodosRestantes2 >= 0,
      NodosRestantes = NodosRestantes2)
      ;
     (NodosRestantes2 < 0,
      NodosRestantes = 0)
    ),
    generar_niveles(NodosRestantes, Aridad, NumNodosUsados, Niveles2),
    append(Niveles2, Nivel, Niveles).
*/


gen_nivel(_, _, 0, 0,[]).
gen_nivel(NodosDisp, Aridad, TamNivel, 0,[0|Nivel2]) :-
    TamNivel > 0,
    NodosDisp =< Aridad,
    NodosDisp < 0,
    TamNivel2 is TamNivel -1,
    gen_nivel(0, Aridad, TamNivel2,_, Nivel2).
gen_nivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv,[NodosDisp|Nivel2]) :-
    TamNivel > 0,
    NodosDisp =< Aridad,
    NodosDisp >= 0,
    TamNivel2 is TamNivel -1,
    gen_nivel(0, Aridad, TamNivel2, Num, Nivel2),
    NumElemsProxNiv is Num + NodosDisp.
gen_nivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv,[Aridad|Nivel2]) :-
    TamNivel > 0,
    NodosDisp > Aridad,
    NodosDisp2 is NodosDisp - Aridad,
    TamNivel2 is TamNivel -1,
    gen_nivel(NodosDisp2, Aridad, TamNivel2, Num, Nivel2),
    NumElemsProxNiv is Num + Aridad.


crearNivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv, Nivel) :-
    NodosDisp2 is NodosDisp - TamNivel,
    gen_nivel(NodosDisp2, Aridad, TamNivel, NumElemsProxNiv, Nivel).




%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%

gen_niveles2(_, _, 0, []) :- !.
gen_niveles2(0, _, _, []) :- !.
gen_niveles2(NodosDisp, Aridad, 1, [Nivel | Niveles]) :-
    NodosDisp >= 0,
    genInteger(Aridad,Aridad2),
    crearNivel(NodosDisp, Aridad2, 1, NumElemsProxNiv, Nivel),
    NodosDisp2 is NodosDisp -1,
    gen_niveles2(NodosDisp2, Aridad, NumElemsProxNiv, Niveles).
gen_niveles2(NodosDisp, Aridad, TamNivel, [Nivel]) :-
    NodosDisp < TamNivel,
    crearNivel(NodosDisp, Aridad, NodosDisp, _, Nivel),!.
gen_niveles2(NodosDisp, Aridad, TamNivel, [Nivel|Niveles]) :-
    NodosDisp >= TamNivel,
    crearNivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv, Nivel),
    NodosDisp2 is NodosDisp -TamNivel,
    genInteger(NumElemsProxNiv,NumElemsProxNiv2),
    gen_niveles2(NodosDisp2, Aridad, NumElemsProxNiv2, Niveles).

/*
genInteger(Y,X) :-
    range(X,0,Y).

range(High, Low, High) :- Low >= 0.
range(Out,Low,High) :- NewHigh is High-1, Low < NewHigh, range(Out, Low, NewHigh).
*/

genInteger(Y,Y).
genInteger(Y,X) :-
    Y2 is Y-1, 0 < Y2, genInteger(Y2,X).



esBuenEsqueleto(esq(Nivel)) :-
    buenEsqueleto(Nivel,1).

buenEsqueleto([], 0).
buenEsqueleto([[X]|N], 1) :-
    longitud(R,1),
    buenEsqueleto(N, X),!.
buenEsqueleto([A|N], X) :-
    longitud(A,Len),
    X=:=Len,
    sumar_nivel(A, Y),
    buenEsqueleto(N, Y),!.

sumar_nivel([], 0).
sumar_nivel([X|Nivel],NumElems) :-
    sumar_nivel(Nivel, NumElems2),
    NumElems is NumElems2 + X.



buenosEsqueletos([], []).
buenosEsqueletos([Esq|Esqueletos],Otros) :-
    \+ esBuenEsqueleto(Esq),
    buenosEsqueletos(Esqueletos, Otros).
buenosEsqueletos([Esq|Esqueletos],[Esq | Otros]) :-
    esBuenEsqueleto(Esq),
    buenosEsqueletos(Esqueletos, Otros).


esqueleto2(N,R,esq(Niveles)) :-
    gen_niveles2(N,R,1,Niveles).

esqueleto(N,R,E) :- 
    setof(X,(esqueleto2(N,R,X)),L2),
    buenosEsqueletos(L2, L), !,
    member(E,L).

/*
gen_niveles3(_, _, 0, []) :- !.
gen_niveles3(0, _, _, []) :- !.
gen_niveles3(N,R,1,[[X]|Niveles]):-
    N > 0,
    genInteger(R,X),
    N2 is N-X,
    gen_niveles(N2,R,X,Niveles).
gen_niveles3(N,R,M,[Nivel]):-
    N < M,
    crearNivel(N, R, M, _, Nivel),!.
gen_niveles3(N,R,M,[Nivel|Niveles]):-
    N >= M,
    N2 is R*M,
    genInteger(N2,X),
    X > M,
    crearNivel(X, R, M, M2, Nivel),
    N3 is N -X,
    N3 > 0,
    N4 is N - M + N3,
    gen_niveles3(N4,R,M2,Niveles).




esqueleto3(N,R,esq(Niveles)) :-
    gen_niveles3(N,R,1,Niveles).
*/

generarListaEtiquetas(N,L) :-
    findall(X,(genInteger(N,X)),L).

%%%



genArbol(_, nodo(E, []),NumNivel,0,[E|ETS],ETS).
genArbol(esq(Niveles), nodo(E, LA),NumNivel,NumHijos,[E|ETS],ETS3) :-
    NumHijos > 0,
    NumNivel >= 0,
    extraerN_Elem(NumNivel, Niveles, ListaNodosNivelN),
    longitud(ListaNodosNivelN, LongLista),
    NumNodosVistosEnNivelNuevo is LongLista -NumHijos,
    extraerN_Elem(NumNodosVistosEnNivel, ListaNodosNivelN, NumHijosHijo),%write(' '),write(NumHijosHijo),nl,
    NumHijos2 is NumHijos -1,
    NumNivel2 is NumNivel +1,
    ((genArbol(esq(Niveles), Hijo,NumNivel2,NumHijosHijo,ETS,ETS2),
    genArbol(esq(Niveles), Nodo,NumNivel,NumHijos2,[E|ETS2],ETS3),
    extraerAristas(Nodo, Aristas),
    LA=[arista(0, Hijo) | Aristas]);(Aristas=[],LA=[])),!.



obtenerCantNodos([],0).
obtenerCantNodos([Nivel|Niveles], Acum) :-
    longitud(Nivel, Cant),
    obtenerCantNodos(Niveles, Acum2),
    Acum is Cant + Acum2.

construirArbol(esq([[X]|Niveles]), Arbol) :-
    obtenerCantNodos([[X]|Niveles], N),
    generarListaEtiquetas(N, Etiquetas),
    genArbol(esq([[X]|Niveles]), Arbol, 1,X,Etiquetas,_).

construirArboles(esq([[X]|Niveles]), Arbol) :-
    obtenerCantNodos([[X]|Niveles], N),
    generarListaEtiquetas(N, Etiquetas),!,
    permutation(Etiquetas,Etiquetamiento),
    genArbol(esq([[X]|Niveles]), Arbol2, 1,X,Etiquetamiento,_),
    calcularAristas(Arbol2, Arbol).


etiquetamiento(Esqueleto, Arbol) :-
    construirArboles(Esqueleto, Arbol),
    etiquetasUnicasAristas(Arbol).


calcularAristas(nodo(E,[]),nodo(E,[])).
calcularAristas(nodo(E1,[arista(_,Nodo)|Aristas]), nodo(E1, [arista(E2,Nodo2)|Aristas2])) :-
    extraerEtiquetaDeNodo(Nodo, E3),
    E2 is abs(E1-E3),
    calcularAristas(Nodo, Nodo2),
    calcularAristas(nodo(E1, Aristas), Nodo3),
    extraerAristas(Nodo3, Aristas2).


extraerAristas(nodo(_, Aristas), Aristas).

%%%
/*
esqEtiquetables(R, N) :-
    esqueleto(N, R, Esqueleto),
    findall(Esqueleto, etiquetamiento(Esqueleto, Arbol), ListaArboles),
    longitud(ListaArboles, CantArboles),
    CantArboles > 0.
*/

esqEtiquetables(R, N) :-
    findall(Esqueleto, esqueleto(N, R, Esqueleto), Esqueletos),
    verificarEtiquetables(Esqueletos).

verificarEtiquetables([]).
verificarEtiquetables([Esqueleto|Esqueletos]) :-
    findall(Esqueleto, etiquetamiento(Esqueleto, Arbol), ListaArboles),
    longitud(ListaArboles, CantArboles),
    CantArboles > 0,
    verificarEtiquetables(Esqueletos).



