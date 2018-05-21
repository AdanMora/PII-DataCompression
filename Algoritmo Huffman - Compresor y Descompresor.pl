%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Instituto Tecnológico de Costa Rica
% Escuela de Computación
% Lenguajes de Programación
% Tarea Programado 2: Compresión de archivos
% Integrantes:
%	      Adán Mora Fallas
%	      Brandon Dinarte
%	      Adrián Barboza
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Para comprimir o descomprimir, haga la siguiente pregunta:
%
%	comprimir(X).
%	descomprimir(X).
%	% Si el archivo a comprimir es "texto.txt", X = "texto"
%	% (sin la extension ".txt")
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Para limpiar la base de hechos y poder comprimir o descomprimir otro
%  archivo, ejecute la siguiente pregunta.
%
%	retractall(dicEntry(_,_)), retracall(nodo(_,_,_,_)),
%	retractall(codigoHuff(_,_)).
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic dicEntry/2.
:- dynamic nodo/4.
:- dynamic codigoHuff/2.


% obtiene el codigo de caracter y lo agrega a la lista
read_list([Char | Resto]):-
    get0(Char),
    Char =\= -1,
    read_list(Resto).
read_list([]).

% recibe un listado de codigos de caracter y los convierte a su caracter
% correspondiente y los agrega a la lista
char_to_List([],X,X).
char_to_List([CharCode|CharList],List,Out):-
    char_code(Char, CharCode),
    append(List,[Char],NewList),
    char_to_List(CharList,NewList,Out).

% Lee el archivo de texto y devuelve una lista con los caracteres como
% elementos.
read_archive(Name, NewList):-
    see(Name),
    read_list(CharList),
    char_to_List(CharList,[],NewList),
    seen.

% param 1: nombre del archivo a escribir
% param 2: contenido para escribir
writeFile(Nombre,Contenido):-
	open(Nombre,write,S),
	write(S,Contenido),nl(S),
	close(S).

% param1: lista con los caracteres del archivo
% param2: entra vacio, un string acumulador
% param3: entra vacia, lista acumuladora
% param4: lista con los tokens separados y repetidos
obtenerTokens([],'',Acc,Acc).
obtenerTokens([Char1|Rest],Buffer,ListAcc,ResultFinal):-
    atom_chars(" <>=-+[]()°|&%$#!?¿¡/\\*@,;.\n\t\r\s",Especiales),
    not(member(Char1,Especiales)),
    atom_concat(Buffer,Char1,NewBuffer),
    obtenerTokens(Rest,NewBuffer,ListAcc,ResultFinal).
obtenerTokens([Char1|Resto],Buffer,ListAcc,ResultFinal):-
    append(ListAcc,[Buffer],Acc2),
    append(Acc2,[Char1],NuevaLista),
    obtenerTokens(Resto,'',NuevaLista,ResultFinal).

% Convierte de String a Lista de String
stringToList(X,F):- split_string(X,"•","",F).

% Convierte de Lista de Strings a un String
listToString(S,L):- atomics_to_string(S,"",L).

% contarApariciones determnina cuantas veces aparece un token en una lista de tokens.
contarApariciones([],_,0).
contarApariciones([C|R], C, N):- contarApariciones(R, C, X), N is X + 1.
contarApariciones([_C|R], A, N):- contarApariciones(R, A, N).

% Pregunta si un token es una entrada en diccionario.
estaEnDicc(R):- dicEntry(T,_), R == T.

% param1 : lista con los tokens del archivo
% efecto secundario : crea el histograma de apariciones con hechos
crearHechosDicc([]).
crearHechosDicc([C|Lista]):- estaEnDicc(C),crearHechosDicc(Lista).
crearHechosDicc([C|Lista]):- contarApariciones(Lista,C,X),
	N is X + 1,
	assert(dicEntry(C,N)),
	crearHechosDicc(Lista).

% param1 : instancia la lista de las palabras del diccionario
% deja un respaldo de la lista original para posterior uso
obtenerLista(List) :- findall(X, dicEntry(X,_), List).

% param1 : lista
% param2 : var para instanciacion
minimo([],nil).
minimo([M],M).
minimo([P,S|R],M) :- dicEntry(P,Cant1), dicEntry(S,Cant2),
	Cant1<Cant2, minimo([P|R],M).
minimo([P,S|R],M) :- dicEntry(P,Cant1), dicEntry(S,Cant2),
	Cant1>=Cant2, minimo([S|R],M).

% param1: token, param2: cantidad, param3: codigo del nodo(1v0).
% revisa que no exista un nodo con el mismo nombre antes de crearlo
asrtNotExstnt(E1,C1) :- not(nodo(E1,C1,_,_)),
	assert(nodo(E1,C1,nil,nil)).
asrtNotExstnt(E1,C1) :- nodo(E1,C1,_,_).

% param1: minimo1
% param2: minimo2
% param3: lista origen
% param4: variable para instanciar
paso2(E1, E2, ListInput, ListOut) :- dicEntry(E1, C1),
	dicEntry(E2, C2), atom_concat(E1,E2,NewName),
	NewCant is C1+C2, assert(dicEntry(NewName,NewCant)),
	asrtNotExstnt(E1,C1), asrtNotExstnt(E2,C2),
	assert(nodo(NewName,NewCant,E1,E2)),
	append(ListInput,[NewName],ListOut).
% en el predicado anterior hay posibilidad d agregar retract para
% disminuir la base de hechos

% param1: lista con los tokens del archivo, luego de agregados al
% histograma de apariciones
% param2: variable libre para la raiz del arbol
creacionArbol([RaizArbol], RaizArbol).
creacionArbol(Lista, RaizArbol) :- minimo(Lista, Min1),
	delete(Lista,Min1,TempList), minimo(TempList, Min2),
	delete(TempList,Min2,ListFinal),
	paso2(Min1, Min2, ListFinal, ListOut),
	creacionArbol(ListOut, RaizArbol).

% param1: raiz del arbol de huffman
% se encarga de crear los hechos con la codificacion
creacionCodigos(RaizHuff) :-
	nodo(RaizHuff, _, Hi, Hd),
	visitarHojas(Hi,0), visitarHojas(Hd,1).

% recursivamente visita el arbol, acumulando el recorrido de 0 y 1, y
% cuando encuentra una hoja, crea el codigo para ese token de hoja
visitarHojas(NodoActual, CodeAcc) :-
	nodo(NodoActual,_,Hi,Hd),
	Hi = nil, Hd = nil, assert(codigoHuff(NodoActual, CodeAcc)).
visitarHojas(NodoActual, CodeAcc) :-
	nodo(NodoActual,_,Hi,Hd),
	Hi \== nil, Hd \== nil, atom_concat(CodeAcc, 0, CodI),  atom_concat(CodeAcc, 1, CodD),
	visitarHojas(Hi,CodI), visitarHojas(Hd,CodD).

% param1: lista compuesta de los tokens repetidos del texto
% param2: acumulador string vacio.
% param3: string resultado con la codificacion
codificar([], R, R).
codificar([Elem1|Resto], Acc, Result) :-
	codigoHuff(Elem1, Codigo),
	concat(Acc, Codigo, NewAcc),
	codificar(Resto, NewAcc, Result).

% una dos listas, donde la nueva lista contiene los elementos de las
% otras dos intercalados. Se usa para crear el string q contiene el
% diccionario.
intercalarListas([],[],R,R).
intercalarListas([C1|R1],[C2|R2], Acc, String):-
	concat(Acc,C1,Temp),
	concat(Temp,•, Temp1),
	concat(Temp1,C2,Temp2),
	concat(Temp2,•,Temp3),
	concat(Temp3,'\n',Temp4),
	concat(Temp4,•,Temp5),
	intercalarListas(R1,R2,Temp5, String).

% convierte los hechos de codigos huffman, en una sola cadena para su
% almacenamiento en el archivo compreso. Instancia dicha cadena.
diccionarioToString(String) :-
	findall(Token, codigoHuff(Token,_), L1),
	findall(Codigo, codigoHuff(_,Codigo), L2),
	intercalarListas(L1,L2,'',String).

% recibe el nombre del archivo sin extension, lo comprime y crea un
% nuevo archivo con el mismo nombre pero extension .compressed
comprimir(NombreArchivo) :-
	concat(NombreArchivo,'.txt',NA),
	read_archive(NA, Chars),
	obtenerTokens(Chars,'',[],TokensRepetidos),
	delete(TokensRepetidos, '',TR),
	crearHechosDicc(TR),
	obtenerLista(ListaTokens),
	creacionArbol(ListaTokens, Raiz),
	creacionCodigos(Raiz),
	codificar(TR, '', CadenaCompresa),
	diccionarioToString(DiccString),
	concat(NombreArchivo,'.compressed',NN),
	concat(NombreArchivo,'.dicc',ND),
	writeFile(NN, CadenaCompresa),
	writeFile(ND, DiccString).

% elimina el ultimo elemento de una lista
deleteLast([_], []).
deleteLast([Elem|ElemSig], [Elem|Nueva]) :-
    deleteLast(ElemSig, Nueva).

% para la descompresion, recibe a lista con el diccionario y lo vuelve
% hechos
crearDiccionario([]).
crearDiccionario([Token,Codigo,_CambioLinea|Resto]) :-
	atom_codes(Code, Codigo), atom_codes(Tok, Token),
	assert(codigoHuff(Tok,Code)),
	crearDiccionario(Resto).

% para cada elemento de la lista de tokens del archivo, agrega a una
% nueva lista un elemento equivalente al codigo
decodeEach([], _, R, R).
decodeEach([C|Resto], Buffer, Acc, Result):-
	concat(Buffer,C,NewBuf),
	codigoHuff(Token, NewBuf),
	append(Acc,[Token],NewAcc),
	decodeEach(Resto,'',NewAcc,Result).
decodeEach([C|Resto], Buffer, Acc,Result) :-
	concat(Buffer,C,NewBuf),
	not(codigoHuff(_, NewBuf)),
	decodeEach(Resto,NewBuf,Acc,Result).

% recibe un string con la codificacion y devuelve un string con la
% traduccion
decodificar(Codificacion, NewString) :-
	string_to_list(Codificacion, CharList),
	char_to_List(CharList,[], Codes),
	decodeEach(Codes, '', [], NewList),
	listToString(NewList,NewString).

% ejecuta la descompresion del nombre del archivo sin extension que se
% ingrese
descomprimir(NombreArchivo) :-
	concat(NombreArchivo,'.compressed',NA),
	concat(NombreArchivo,'.dicc',ND),
	read_archive(NA,Chars),
	deleteLast(Chars, Codificacion),
	read_archive(ND, DiccChars),
	deleteLast(DiccChars, Diccionario),
	stringToList(Diccionario, LD),
	deleteLast(LD,ListaDiccionario),
	crearDiccionario(ListaDiccionario),
	decodificar(Codificacion, NewString),
	concat(NombreArchivo,'_Descomprimido.txt',NN),
	writeFile(NN,NewString).

% Este predicado limpia toda la base de hechos para descomprimir o
% volver a comprimir.
limpiarTodo():- retractall(dicEntry(_,_)), retractall(nodo(_,_,_,_)), retractall(codigoHuff(_,_)).
