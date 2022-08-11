%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Código Inicial
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% popularidad(Personaje, Popularidad)
popularidad(kitty, 5).
popularidad(keroppi, 2).
popularidad(melody, 3).
popularidad(cinnamoroll, 4).
popularidad(pompompurin, 4).
popularidad(littleTwinStars, 2).
popularidad(badtzMaru, 2).
popularidad(gudetama, 1).

% consiguio(Persona, NroFigurita, Medio)

/*
Ejemplo de uso de consiguio/3:

?- consiguio(flor, Figurita, Medio).
Figurita = 5,
Medio = paquete(1) ;
Figurita = 5,
Medio = paquete(2) ;
Figurita = 4,
Medio = canje(andy, [1]) ;
Figurita = 7,
Medio = canje(andy, [1]) ;
Figurita = 2,
Medio = canje(bobby, [4, 6]).

*/
consiguio(andy, 2, paquete(1)).
consiguio(andy, 4, paquete(1)).
consiguio(andy, 7, paquete(2)).
consiguio(andy, 6, paquete(2)).
consiguio(andy, 6, paquete(3)).
consiguio(andy, 1, paquete(3)).
consiguio(andy, 4, paquete(3)).

consiguio(flor, 5, paquete(1)).
consiguio(flor, 5, paquete(2)).

consiguio(bobby, 3, paquete(1)).
consiguio(bobby, 5, paquete(1)).
consiguio(bobby, 7, paquete(2)).
consiguio(bobby, 5, paquete(2)).

consiguio(Persona, Figurita, canje(Canjeante, ACambio)):-
  cambiaron(Persona, ACambio, Canjeante, FiguritasQueRecibio), 
  member(Figurita, FiguritasQueRecibio).
consiguio(Persona, Figurita, canje(Canjeante, ACambio)):-
  cambiaron(Canjeante, FiguritasQueRecibio, Persona, ACambio), 
  member(Figurita, FiguritasQueRecibio).

% cambiaron/4: Predicado auxiliar para evitar repetir información en la definición de consiguio/3,
% ya que ambas personas consiguen las figuritas que da la otra mediante un canje.
% Relaciona a una persona con la lista de sus figuritas que dio en un canje
% con la otra persona que participó del canje y las figuritas que dio a cambio la otra persona.
% 
% No usar directamente en la implementación de los requerimientos.
% Vale extenderlo para agregar datos de prueba.

cambiaron(andy, [4,7], flor, [1]).
cambiaron(bobby, [2], flor, [4, 6]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Punto 2
%% figurita(2, brillante(TipoDeImagen, Personaje))
%% figurita(5, basica([ListaDePersonajes])).
%% figurita(6, rompecabezas(nombreDeRompecabezas, [ListaDeFiguritas])).

figurita(1, basica([kitty, keroppi])).
figurita(2, brillante(metalizado, kitty)).
figurita(3, brillante(metalizado, melody)).
figurita(4,basica([])).
figurita(5, rompecabezas(kittyYCompania, [5, 6 , 7])).
figurita(6, rompecabezas(kittyYCompania, [5, 6, 7])).
figurita(7, rompecabezas(kittyYCompania, [5, 6 , 7])).
figurita(8, basica([kitty, melody, keroppi,cinnamoroll, pompompurin, littleTwinStars, badtzMaru, gudetama])).

% todosLosPersonajes = [kitty, melody, keroppi,cinnamoroll, pompompurin, littleTwinStars, badtzMaru, gudetama]. Probablemente un findall ??? 

%% Punto  1
tieneFiguritaRepetida(Persona, NumeroFigurita):-
  consiguio(Persona, NumeroFigurita, Medio1),
  consiguio(Persona, NumeroFigurita, Medio2),
  Medio1 \= Medio2.

%% Punto 3

% A partir de esta nueva información, 
% definir un predicado que permita saber si una figurita es valiosa, 
% que se cumple si es rara (son raras aquellas figuritas que nadie tiene repetidas) o si el nivel de atractivo de su imagen es mayor a 7.

esValiosa(NumeroFigurita):-
  figurita(NumeroFigurita, _),
  esRara(NumeroFigurita).

esValiosa(NumeroFigurita):-
  figurita(NumeroFigurita, TipoFigurita),
  nivelDeAtractivo(TipoFigurita, Atractivo),
  Atractivo > 7.

esRara(NumeroFigurita):-
  forall(consiguio(Persona,NumeroFigurita,_), not(tieneFiguritaRepetida(Persona,NumeroFigurita))).

nivelDeAtractivo(brillante(_,Personaje),Atractivo):-
  popularidad(Personaje, NivelDePopularidad),
  Atractivo is NivelDePopularidad * 5.

nivelDeAtractivo(rompecabezas(_,ListaDePartes), 2):-
  length(ListaDePartes, Cantidad),
  Cantidad =< 2.
  
nivelDeAtractivo(rompecabezas(_,ListaDePartes), 0):-
  length(ListaDePartes, Cantidad),
  Cantidad > 2.

%% nivelDeAtractivo(basico(ListaDePersonajes), Atractivo).

%% Punto 4 Relacionar a una persona con la imagen más atractiva de las figuritas que consiguió.

imagenMasAtractivaQueTiene(Persona, ImagenMasAtractiva):-
  consiguio(Persona, _,_),
  consiguio(_,NroFiguritaMasAtractiva,_),
  figurita(NroFiguritaMasAtractiva, ImagenMasAtractiva),
  forall((consiguio(Persona, NroFigurita,_), NroFigurita \= NroFiguritaMasAtractiva), tieneLaImagenMasAtractiva(NroFiguritaMasAtractiva, NroFigurita)).


tieneLaImagenMasAtractiva(NroFiguritaMasAtractiva, NroFigurita):-
  figurita(NroFiguritaMasAtractiva, ImagenMasAtractiva),
  figurita(NroFigurita, Imagen),
  nivelDeAtractivo(ImagenMasAtractiva, Atractivo1),
  nivelDeAtractivo(Imagen, Atractivo2),
  Atractivo1 > Atractivo2,
  NroFiguritaMasAtractiva \= NroFigurita.

  





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pruebas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests de ejemplo sobre código base
:- begin_tests(consiguio).

%% Testeo de consultas que esperan que sean ciertas
test(florConsiguioLa5EnSuPrimerPaquete, nondet):-
  consiguio(flor, 5, paquete(1)).
test(florConsiguioLa2EnUnCanjeConBobby, nondet):-
  consiguio(flor, 2, canje(bobby, _)).

%% Testeo de consultas que esperan que sean falsas
test(florConsiguioLa5EnSuPrimerPaquete, fail):-
  consiguio(flor, 2, paquete(1)).

%% Testeo de consultas existenciales con múltiples respuestas => inversibilidad
test(figuritasQueConsiguioFlor, set(Figurita == [5, 4, 7, 2])):-
  consiguio(flor, Figurita, _).

%% Test basado en condiciones más complejas
test(cuandoDosPersonasCambianFiguritasAmbasConsiguenFiguritasPorCanjeConLaOtraIncluyendoLasQueLeDieronALaOtra, nondet):-
  consiguio(andy, _, canje(flor, [4,7])),
  consiguio(flor, _, canje(andy, [1])).

:- end_tests(consiguio).

:- begin_tests(esValiosa).
%% Testeo de consultas que esperan que sean ciertas