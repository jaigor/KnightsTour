%-------------------------------
% Representación de términos
%-------------------------------
/**
 * - Casilla: F y R son dos enteros que representan el 
 * número de columna (file) y de fila (rank) respectivamente
 * s(F,R)
 * - Nodo: nodo del espacio de búsqueda de soluciones (node) se 
 * representa por la función con cinco argumentos, que a su vez 
 * son términos Prolog:
 * node(Tamanyo_Board,Long_Path_recorrido,Board,Square_actual,Path_recorrido).
 */

/** 
 * Construye un tablero vacío de nxn en una ejecución
 */
tableroVacio(N,Board):- ct_aux(1,N,Board).

ct_aux(M,N,[]):- M > N, !.
ct_aux(M,N,[File|RF]):-
    columnaVacia(N,File), M1 is M+1, ct_aux(M1,N,RF).

columnaVacia(1,[true]):- !.
columnaVacia(N,[true|CV]):- N1 is N-1, columnaVacia(N1,CV).

/** 
 * Comprueba si en un nodo del espacio de búsqueda hay, o no,
 * una solución, si contiene, o no, un camino completo 
 * (que recorra todo el tablero). 
 */
fullPath(node(N,L,_,_,_)):- (N*N) =:= L.

/** 
 * Define el nodo inicial a partir del tamaño del
 * tablero, N, y una casilla desde la que el caballo inicia su 
 * recorrido, s(F,R), en un tablero vacío.
 */
initBoard(N,s(F,R),node(N,1,Board,s(F,R),[s(F,R)])):-
    tableroVacio(N,Board0),
    inBoard(N,s(F,R)),!,
    visitSquare(s(F,R),Board0,Board).

/** 
 * Comprueba si la casilla s(F,R) está dentro de un tablero 
 * de tamaño N.
 */
inBoard(N,s(F,R)):- 
    N >= F, N >= R,
    F >= 1, R >= 1.

/** 
 * Dada una casilla (s(F,R)), cambia su
 * valor a falso en un tablero dado (Board).
 */
visitSquare(s(F,R),Board,BoardSol):- markRank(Board,R,BoardSol,F).

/**
 * Función principal, entero representa el tamaño del tablero, 
 *                    casilla inicial del caballo 
 * devuelve camino que recorre todo el tablero comenzando
 *                     por la casilla indicada. 
 *                     Si no hubiera solución, camino devuelto == vacío.
 */
knightTravel(N,s(F,R)):-
    initBoard(N,s(F,R),node(N,1,Board,s(F,R),[s(F,R)])),
    recorrerBT(node(N,1,Board,s(F,R),[s(F,R)])),!. 
% si se encuentra camino no se ejecuta el camino vacío
knightTravel(_,_):-
    write([]).

/**
 * Búsqueda de soluciones en profundidad con vuelta
 * atrás con una llamada recursiva. 
 * Condición de parada, encontrar un nodo solución (fullPath).
 */
recorrerBT(node(N,L,_,_,Path)):-
    fullPath(node(N,L,_,_,_)),
    write(Path),!.
recorrerBT(node(N,L,Board,s(F,R),Path)):-
    jump(node(N,L,Board,s(F,R),Path),NNuevo),
    recorrerBT(NNuevo).

/** 
 * Comprueba que haya nodos sucesores a partir de Node y
 * construye los movimientos posibles, de uno en uno, comprobando
 * si son casillas válidas (validSquare) y por lo tanto movimientos
 * válidos que generan un nodo sucesor (NNode).
 */
jump(node(N,L,Board,s(F,R),Path), node(N,NL,BoardNuevo,s(NF,NR),NPath)):-
    knightMove(F,R,NF,NR),
    visita(N,s(NF,NR),Board,BoardNuevo),
    NL is L+1,
    append(Path,[s(NF,NR)],NPath).

/**
 * Evita recorrer el tablero dos veces al mover el caballo a una
 * casilla s(F,R), que se sabe que está en el tablero (inBoard). 
 * Con la solución pedida, hay que recorrer el tablero una vez 
 * para comprobar si está libre (freeSquare) y otra vez para colocar
 * false (visitSquare).
 */
visita(N,s(F,R),Board,BoardNuevo):-
    inBoard(N,s(F,R)),
    visitSquare(s(F,R),Board,BoardNuevo).

%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliar predicates %
%%%%%%%%%%%%%%%%%%%%%%%
/** 
 * Marca la casilla como false, generandola  
 * en un nuevo tablero, tanto con las mismas filas
 * (markRank), como columnas (markFile)
 */
markRank([],_,[],_):- !.
markRank([H|T],1,[HF|Rest],F):-
    markFile(H,F,HF),
    markedRank(T,Rest).
markRank([H|T],R,[H|Rest],F):-
    R > 1,
    NR is R-1,
    markedFile(H,H),
    markRank(T,NR,Rest,F).
      
markedRank([],_):-!.
markedRank([H|T],[H|Rest]):-
    markedFile(H,H),
    markedRank(T,Rest).

% una vez que se ha marcado el File se continua igual
% con el resto del Board  
markedFile([],_):-!.
markedFile([H|T],[H|Rest]):-
    markedFile(T,Rest).

markFile([],_,_):-!.
markFile([true|T],1,[false|Rest]):-
    markedFile(T,Rest).
markFile([H|T],F,[H|Rest]):-
    F > 1,
    NF is F-1,
    markFile(T,NF,Rest).
% Posibles movimientos del caballo
knightMove(F,R,NF,NR):- NF is F+2, NR is R+1.
knightMove(F,R,NF,NR):- NF is F+2, NR is R-1.
knightMove(F,R,NF,NR):- NF is F+1, NR is R+2.
knightMove(F,R,NF,NR):- NF is F+1, NR is R-2.
knightMove(F,R,NF,NR):- NF is F-1, NR is R+2.
knightMove(F,R,NF,NR):- NF is F-1, NR is R-2.
knightMove(F,R,NF,NR):- NF is F-2, NR is R+1.
knightMove(F,R,NF,NR):- NF is F-2, NR is R-1.
