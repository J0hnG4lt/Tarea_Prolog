
%% Autor: Georvic Tur
%% Carnet: 12-11402
%% Correo: alexanderstower@gmail.com


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Primera Parte: bienEtiquetado%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%True si el Arbol esta bien etiquetado
bienEtiquetado(Arbol) :-
    bienEtiquetado2(Arbol), %Verifica las diferencias del Arbol
    etiquetasUnicasAristas(Arbol),
    etiquetasUnicasNodos(Arbol).


% True si las diferencias entre las etiquetas de los extremos corresponden
%con las etiquetas de las aristas
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


etiquetasUnicasAristas(Arbol) :-
    listaEtiquetasAristas(Arbol, Lista),
    noHayRepetidas(Lista).


etiquetasUnicasNodos(Arbol) :-
    listaEtiquetas(Arbol, Lista),
    noHayRepetidas(Lista).


% Extrae las etiquetas de los nodos
listaEtiquetas(nodo(E,[]), [E]).
listaEtiquetas(nodo(E,[arista(_, Nodo) | Aristas]), L):-
    listaEtiquetas(Nodo, L2),
    listaEtiquetas(nodo(E,Aristas), L3),
    append(L2,L3,L), !.


% Extrae las etiquetas de las aristas
listaEtiquetasAristas(nodo(E,[]), []).
listaEtiquetasAristas(nodo(E2,[arista(E, Nodo) | Aristas]), [E|L]):-
    listaEtiquetasAristas(Nodo, L2),
    listaEtiquetasAristas(nodo(E2,Aristas), L3),
    append(L2,L3,L), !.


% True si la lista no contiene elementos repetidos
noHayRepetidas([]).
noHayRepetidas([X|XS]) :-
    \+ member(X,XS),
    noHayRepetidas(XS).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Segunda Parte: esqueleto%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% True si E es un esqueleto de N nodos y aridad R
% Se generan todas las posibilidades de E dados N y R
esqueleto(N,R,E) :- 
    setof(X,(esqueleto2(N,R,X)),L2), %Se descartan repetidos
    buenosEsqueletos(L2, L), !, %Han de estar bien definidos
    member(E,L). % Se generan las posibilidades

%Auxiliar
esqueleto2(N,R,esq(Niveles)) :-
    gen_niveles2(N,R,1,Niveles).

% Genera Los Niveles en el ultimo campo
% Por construccion cumplen con ser no crecientes.
% Para generar todas las variaciones de la lista de niveles, se disminuye
% en cada backtracking la cantidad de elementos a poner en el siguiente nivel
% y se compenza de manera adecuada a la cantidad de nodos disponibles.
% NodosDisp: Mientras voy agregando nodos a un nivel, disminuyo la cantidad
%  de nodos disponibles para poner más adelante.
% TamNivel: Cantidad de elementos de la lista a crear para este nivel
gen_niveles2(_, _, 0, []) :- !.
gen_niveles2(0, _, _, []) :- !.
gen_niveles2(NodosDisp, Aridad, 1, [Nivel | Niveles]) :-
    NodosDisp >= 0,
    genInteger(Aridad,Aridad2), %Genero las variaciones de la raiz
    crearNivel(NodosDisp, Aridad2, 1, NumElemsProxNiv, Nivel),
    NodosDisp2 is NodosDisp -1,
    gen_niveles2(NodosDisp2, Aridad, NumElemsProxNiv, Niveles).
gen_niveles2(NodosDisp, Aridad, TamNivel, [Nivel]) :- % Caso Base
    NodosDisp < TamNivel,
    crearNivel(NodosDisp, Aridad, NodosDisp, _, Nivel),!.
gen_niveles2(NodosDisp, Aridad, TamNivel, [Nivel|Niveles]) :-
    NodosDisp >= TamNivel,
    crearNivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv, Nivel),
    NodosDisp2 is NodosDisp -TamNivel,
    genInteger(NumElemsProxNiv,NumElemsProxNiv2), % Variacion de tam del siguiente nivel
    gen_niveles2(NodosDisp2, Aridad, NumElemsProxNiv2, Niveles).


% Dado Y, genero en X todos los enteros entre 1 y Y. Si Y=0, luego X=0.
genInteger(Y,Y).
genInteger(Y,X) :-
    Y2 is Y-1, 0 < Y2, genInteger(Y2,X).


%Auxiliar
crearNivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv, Nivel) :-
    NodosDisp2 is NodosDisp - TamNivel,
    gen_nivel(NodosDisp2, Aridad, TamNivel, NumElemsProxNiv, Nivel).


% Genero un nuevo nivel del esqueleto.
gen_nivel(_, _, 0, 0,[]).
gen_nivel(NodosDisp, Aridad, TamNivel, 0,[0|Nivel2]) :- %Sin nodos disponibles para el siquiente nivel
    TamNivel > 0,
    NodosDisp =< Aridad,
    NodosDisp < 0,
    TamNivel2 is TamNivel -1,
    gen_nivel(0, Aridad, TamNivel2,_, Nivel2).
gen_nivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv,[NodosDisp|Nivel2]) :- %Ultimos nodos disponibles
    TamNivel > 0,
    NodosDisp =< Aridad,
    NodosDisp >= 0,
    TamNivel2 is TamNivel -1,
    gen_nivel(0, Aridad, TamNivel2, Num, Nivel2),
    NumElemsProxNiv is Num + NodosDisp.
gen_nivel(NodosDisp, Aridad, TamNivel, NumElemsProxNiv,[Aridad|Nivel2]) :- %Suficientes Nodos disponibles
    TamNivel > 0,
    NodosDisp > Aridad,
    NodosDisp2 is NodosDisp - Aridad,
    TamNivel2 is TamNivel -1,
    gen_nivel(NodosDisp2, Aridad, TamNivel2, Num, Nivel2),
    NumElemsProxNiv is Num + Aridad.




%True si el esqueleto cumple con la definicion
buenosEsqueletos([], []).
buenosEsqueletos([Esq|Esqueletos],Otros) :-
    \+ esBuenEsqueleto(Esq),
    buenosEsqueletos(Esqueletos, Otros).
buenosEsqueletos([Esq|Esqueletos],[Esq | Otros]) :-
    esBuenEsqueleto(Esq),
    buenosEsqueletos(Esqueletos, Otros).

%Auxiliar
esBuenEsqueleto(esq(Nivel)) :-
    buenEsqueleto(Nivel,1).

% True si los elementos de cada nivel dan la informacion correcta
% sobre el numero de elementos del siguiente.
buenEsqueleto([], 0).
buenEsqueleto([[X]|N], 1) :-
    longitud(R,1),
    buenEsqueleto(N, X),!.
buenEsqueleto([A|N], X) :-
    longitud(A,Len),
    X=:=Len,
    sumar_nivel(A, Y),
    buenEsqueleto(N, Y),!.

%Suma elementos de la lista
sumar_nivel([], 0).
sumar_nivel([X|Nivel],NumElems) :-
    sumar_nivel(Nivel, NumElems2),
    NumElems is NumElems2 + X.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tercera Parte: etiquetamiento%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% True si Arbol es un buen etiquetamiento de Esqueleto.
% Dado Esqueleto, se generan todos los posibles Arboles bien etiquetados
% que correspondan con esqueleto.
etiquetamiento(Esqueleto, Arbol) :-
    construirArboles(Esqueleto, Arbol),
    etiquetasUnicasAristas(Arbol). %Definida Arriba


% Dado un Esqueleto, se generan todos los posibles arboles.
% Los arboles tienen nodos con etiquetas unicas por construccion.
% Las aristas no necesariamente son unicas.
construirArboles(esq([[X]|Niveles]), Arbol) :-
    obtenerCantNodos([[X]|Niveles], N), % Cantidad Total de Nodos
    generarListaEtiquetas(N, Etiquetas),!, % Lista de Etiquetas
    permutation(Etiquetas,Etiquetamiento), % Variaciones para cada arbol
    genArbol(esq([[X]|Niveles]), Arbol2, 1,X,Etiquetamiento,_),
    calcularAristas(Arbol2, Arbol). % Se computan las etiquetas de las aristas

% Auxiliar
construirArbol(esq([[X]|Niveles]), Arbol) :-
    obtenerCantNodos([[X]|Niveles], N),
    generarListaEtiquetas(N, Etiquetas),
    genArbol(esq([[X]|Niveles]), Arbol, 1,X,Etiquetas,_).


% Construyo lista de numeros distintos
generarListaEtiquetas(N,L) :-
    findall(X,(genInteger(N,X)),L).

% La salida es el segundo campo y el ultimo.
% Dado un esqueleto, el numero de nivel actual, numero de hijos y lista
% de etiquetas, se genera un arbol junto con la lista de etiquetas sobrantes.
genArbol(_, nodo(E, []),NumNivel,0,[E|ETS],ETS). % Caso Base
genArbol(esq(Niveles), nodo(E, LA),NumNivel,NumHijos,[E|ETS],ETS3) :-
    NumHijos > 0,
    NumNivel >= 0,
    extraerN_Elem(NumNivel, Niveles, ListaNodosNivelN),
    longitud(ListaNodosNivelN, LongLista),
    NumNodosVistosEnNivelNuevo is LongLista -NumHijos,
    extraerN_Elem(NumNodosVistosEnNivel, ListaNodosNivelN, NumHijosHijo),
    NumHijos2 is NumHijos -1,
    NumNivel2 is NumNivel +1,
    ((genArbol(esq(Niveles), Hijo,NumNivel2,NumHijosHijo,ETS,ETS2), % Si no es el ultimo nodo
    genArbol(esq(Niveles), Nodo,NumNivel,NumHijos2,[E|ETS2],ETS3),
    extraerAristas(Nodo, Aristas),
    LA=[arista(0, Hijo) | Aristas]);(Aristas=[],LA=[])),!. % Si es el ultimo nodo


%Suma de la cantidad de elementos de cada sub lista
obtenerCantNodos([],0).
obtenerCantNodos([Nivel|Niveles], Acum) :-
    longitud(Nivel, Cant),
    obtenerCantNodos(Niveles, Acum2),
    Acum is Cant + Acum2.


% Dado un arbol de nodos etiquetados, se calculan las etiquetas de las aristas.
calcularAristas(nodo(E,[]),nodo(E,[])).
calcularAristas(nodo(E1,[arista(_,Nodo)|Aristas]), nodo(E1, [arista(E2,Nodo2)|Aristas2])) :-
    extraerEtiquetaDeNodo(Nodo, E3),
    E2 is abs(E1-E3),
    calcularAristas(Nodo, Nodo2),
    calcularAristas(nodo(E1, Aristas), Nodo3),
    extraerAristas(Nodo3, Aristas2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cuarta Parte: esqEtiquetables%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% True si todos los esqueletos de N nodos y Aridad R son bien etiquetables
esqEtiquetables(R, N) :-
    findall(Esqueleto, esqueleto(N, R, Esqueleto), Esqueletos),
    verificarEtiquetables(Esqueletos).

% Se verifica para todos los esqueletos que exista un arbol bien etiquetado
% que sea equivalente
verificarEtiquetables([]).
verificarEtiquetables([Esqueleto|Esqueletos]) :-
    findall(Esqueleto, etiquetamiento(Esqueleto, Arbol), ListaArboles),
    longitud(ListaArboles, CantArboles),
    CantArboles > 0, % Al menos ha de haber un arbol para que se cumpla el predicado
    verificarEtiquetables(Esqueletos).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Quinta Parte: describirEtiquetamiento%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados Auxiliares %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

extraerAristas(nodo(_, Aristas), Aristas).

cola([],[]) :- !.
cola([_ | XS], XS) :- !.

cabeza([], _) :- fail.
cabeza([X | _], X) :- !.

extraerNodo(arista(_, Nodo), Nodo).

extraerEtiquetaDeNodo(nodo(R, _), R).

extraerEtiquetaDeArista(arista(EA, _), EA).

longitud([], 0).
longitud([_ | XS], L) :- longitud(XS, L2), L is L2 + 1,!.

extraerNiveles(esq(Niveles), Niveles).








extraerN_Elem(0, [X | _], X) :- !.
extraerN_Elem(Numero, [_ | XS], R) :- 
    Numero_2 is Numero - 1,
    extraerN_Elem(Numero_2, XS, R), !.






























