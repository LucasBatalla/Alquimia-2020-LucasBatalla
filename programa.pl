% Aquí va el código.

tiene(ana,agua).
tiene(ana,vapor).
tiene(ana,tierra).
tiene(ana,hierro).

tiene(cata,agua).
tiene(cata,fuego).
tiene(cata,tierra).
tiene(cata,aire).


tiene(beto, Elemento):-
    tiene(ana, Elemento).

haceFalta([agua,tierra], pasto).
haceFalta([fuego, agua, tierra], hierro).
haceFalta([pasto, agua], huesos).
haceFalta([hierro, vapor], presion).
haceFalta([agua, fuego], vapor).
haceFalta([silicio,hierro, plastico], playStation).
haceFalta([tierra], silicio).
haceFalta([huesos, presion], plastico).

elemento(Elemento):-tiene(_,Elemento).
elemento(Elemento):-requireElemento(Elemento,_).
elemento(Elemento):-requireElemento(_,Elemento).

requireElemento(ElementoCompuesto, Elemento):-
    haceFalta(Elementos, ElementoCompuesto),
    member(Elemento, Elementos).

herramienta(ana, circulo(50,3)).
herramienta(ana, cuchara(40)).
herramienta(beto, circulo(20,1)).
herramienta(beto, libro(inerte)).
herramienta(cata, libro(vida)).
herramienta(cata, circulo(100,5)).

tieneIngredientesPara(Elemento, Persona):-
    tiene(Persona,_),
    haceFalta(Elementos, Elemento),
    findall(ElementoPersona, tiene(Persona,ElementoPersona), ElementosPersona),
    forall(member(E,Elementos), member(E, ElementosPersona)).

tieneIngredientesParaV2(Elemento, Persona):-
    tiene(Persona,_),
    haceFalta(_, Elemento),
    forall(member(E,Elemento),tiene(Persona, E)).    

estaVivo(agua).
estaVivo(fuego).

estaVivo(Elemento):-
    requireElemento(Elemento, Material),
    estaVivo(Material).


puedeConstruir(Persona, Elemento):-
   tieneIngredientesPara(Elemento, Persona),
   tieneHerramientasPara(Elemento, Persona).  
    
tieneHerramientasPara(Elemento, Persona):-
    estaVivo(Elemento),
    herramienta(Persona, libro(vida)).


tieneHerramientasPara(Elemento, Persona):-
    noEstaVivo(Elemento),
    herramienta(Persona, libro(inerte)).


tieneHerramientasPara(Elemento, Persona):-
    cantidadDeIngredientes(Elemento, CantIngredientes),
    herramienta(Persona, Herramienta),
    soporta(Herramienta,CantIngredientes).

noEstaVivo(Elemento):-
    haceFalta(_,Elemento),
    not(estaVivo(Elemento)).

soporta(circulo(Diametro,Niveles), CantIngredientes):-
    CantIngredientes =< (Diametro / 100) * Niveles.

soporta(cuchara(Longitud), CantIngredientes):-
    CantIngredientes =< Longitud / 10.

cantidadDeIngredientes(Elemento, Cant):- 
    haceFalta(Elementos, Elemento),
    length(Elementos, Cant).
       

todoPoderoso(Persona):-
    tieneLosElementosPrimitivos(Persona),
    puedeConstruirLoQueNoTiene(Persona).


tieneLosElementosPrimitivos(Persona):-
    tiene(Persona,_),
    forall(esPrimitivo(Elemento),tiene(Persona,Elemento)).

puedeConstruirLoQueNoTiene(Persona):-
    tiene(Persona,_),
    forall(noTiene(Persona,Elemento),tieneHerramientasPara(Elemento,Persona)).


esPrimitivo(Elemento):-
    elemento(Elemento),
    not(requireElemento(Elemento,_)).

noTiene(Persona, Elemento):-
    elemento(Elemento),
    not(tiene(Persona,Elemento)).

quienGana(Ganador):-
    tiene(Ganador,_),
    forall(contrincante(Ganador,Contrincante), puedeConstruirMasElementos(Ganador,Contrincante)).


contrincante(Persona, Contrincante):-
    tiene(Persona,_),
    tiene(Contrincante,_),
    Persona \= Contrincante.

puedeConstruirMasElementos(Persona, Contrincante):-
    tiene(Persona,_),
    tiene(Contrincante,_),
    findall(Elemento, puedeConstruir(Persona,Elemento),MasElementos),
    findall(Elemento, puedeConstruir(Contrincante,Elemento), MenosElementos),
    length(MasElementos, Mayor),
    length(MenosElementos, Menor),
    Mayor > Menor.
    



puedeLlegarATener(Elemento, Persona):-
    tiene(Persona,Elemento).


puedeLlegarATener(Elemento, Persona):-
   tieneHerramientasPara(Elemento,Persona),
   forall(requireElemento(Elemento, Ingrediente), puedeLlegarATener(Ingrediente,Persona)).
