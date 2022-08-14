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

% Para testeo
consiguio(flor, 2, paquete(4)).

consiguio(andy, 3, paquete(4)).
consiguio(andy, 5, paquete(4)).

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
%% Nuevos predicados implementados
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Punto 1
tieneFiguritaRepetida(Persona, NumeroFigurita):-
  consiguio(Persona, NumeroFigurita, Medio1),
  consiguio(Persona, NumeroFigurita, Medio2),
  Medio1 \= Medio2.

%% Punto 2
%% figurita(Numero, brillante(Fondo, PersonajePrincipal))
%% figurita(Numero, rompecabezas(Nombre, [FiguritasQueLoConforman])).
%% figurita(Numero, basica([PersonajesIncluidos])).

figurita(1, basica([kitty, keroppi])).
figurita(2, brillante(metalizado, kitty)).
figurita(3, brillante(metalizado, melody)).
figurita(4, basica([])).
figurita(Numero, rompecabezas(kittyYCompania, Figuritas)):-
  between(5, 7, Numero),
  findall(Figurita, between(5, 7, Figurita), Figuritas).
figurita(8, basica(TodosLosPersonajes)):-
  findall(Personaje, popularidad(Personaje, _), TodosLosPersonajes).

%% Punto 3

esValiosa(NumeroFigurita):-
  figurita(NumeroFigurita, _),
  esRara(NumeroFigurita).

esValiosa(NumeroFigurita):-
  figurita(NumeroFigurita, ImagenFigurita),
  nivelDeAtractivo(ImagenFigurita, Atractivo),
  Atractivo > 7.

esRara(NumeroFigurita):-
  forall(consiguio(Persona,NumeroFigurita,_), not(tieneFiguritaRepetida(Persona,NumeroFigurita))).

nivelDeAtractivo(brillante(_, PersonajePrincipal), Atractivo):-
  popularidad(PersonajePrincipal, NivelDePopularidad),
  Atractivo is 5 * NivelDePopularidad.

nivelDeAtractivo(basica(PersonajesIncluidos), Atractivo):-
  findall(Popularidad, (member(Personaje, PersonajesIncluidos), popularidad(Personaje, Popularidad)), Popularidades),
  sum_list(Popularidades, Atractivo).

nivelDeAtractivo(rompecabezas(_, Partes), 2):-
  length(Partes, Cantidad),
  Cantidad =< 2.

nivelDeAtractivo(rompecabezas(_, Partes), 0):-
  length(Partes, Cantidad),
  Cantidad > 2.

%% Punto 4

imagenMasAtractivaQueTiene(Persona, ImagenMasAtractiva):-
  consiguio(Persona, NumeroFiguritaMasAtractiva,_),
  figurita(NumeroFiguritaMasAtractiva, ImagenMasAtractiva),
  forall((consiguio(Persona, OtroNumeroFigurita,_), OtroNumeroFigurita \= NumeroFiguritaMasAtractiva), esMasAtractivaQue(NumeroFiguritaMasAtractiva, OtroNumeroFigurita)).

esMasAtractivaQue(NumeroFiguritaMasAtractiva, OtroNumeroFigurita):-
  figurita(NumeroFiguritaMasAtractiva, ImagenMasAtractiva),
  figurita(OtroNumeroFigurita, OtraImagen),
  NumeroFiguritaMasAtractiva \= OtroNumeroFigurita,
  nivelDeAtractivo(ImagenMasAtractiva, Atractivo1),
  nivelDeAtractivo(OtraImagen, Atractivo2),
  Atractivo1 > Atractivo2.

%% Punto 5

hizoNegocio(Persona, canje(Canjeante, ACambio)):-
  consiguio(Persona, Figurita, canje(Canjeante, ACambio)),
  esValiosa(Figurita),
  forall(member(FiguritaQueDio, ACambio), not(esValiosa(FiguritaQueDio))).

%% Punto 6

necesita(Persona, NumeroFigurita):-
  consiguio(Persona, _, _),
  figurita(NumeroFigurita, _),
  not(consiguio(Persona, NumeroFigurita, _)),
  forall((figurita(OtroNumeroFigurita, _), OtroNumeroFigurita \= NumeroFigurita), consiguio(Persona, OtroNumeroFigurita, _)).

necesita(Persona, NumeroFigurita):-
    consiguio(Persona, _, _),
    figurita(NumeroFigurita, rompecabezas(Nombre, Partes)),
    not(consiguio(Persona, NumeroFigurita, _)),
    figurita(OtroNumeroFigurita, rompecabezas(Nombre, Partes)),
    consiguio(Persona, OtroNumeroFigurita, _).

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

:- begin_tests(figuritasRepetidas).

test(andyTieneRepetidaLaFigurita1, nondet):-
  tieneFiguritaRepetida(andy, 1).

test(andyNoTieneRepetidaLaFigurita7, fail):-
  tieneFiguritaRepetida(andy, 7).

test(personaDetieneFiguritaRepetidaEsInversible, set(Persona == [flor, bobby])):-
  tieneFiguritaRepetida(Persona, 5).

test(numeroFiguritaDetieneFiguritaRepetidaEsInversible, set(NumeroFigurita == [5])):-
  tieneFiguritaRepetida(bobby, NumeroFigurita).

:- end_tests(figuritasRepetidas).

:- begin_tests(esValiosa).

test(laFigurita7EsValiosaPorSerRara, nondet):-
  esValiosa(7).

test(laFigurita2EsValiosaPorSerAtractiva, nondet):-
  esValiosa(2).

test(laFigurita6NoEsValiosa, fail):-
  esValiosa(6).

test(esValiosaEsInversible, set(NumeroFigurita == [3, 7, 8, 2])):-
  esValiosa(NumeroFigurita).

:- end_tests(esValiosa).

:- begin_tests(imagenMasAtractiva).

test(laImagenMasAtractivaQueTieneAndyEsUnaBrillante, nondet):-
  imagenMasAtractivaQueTiene(andy, brillante(metalizado, kitty)).

test(laImagenMasAtractivaQueTieneAndyNoEsUnRompecabezas, fail):-
  imagenMasAtractivaQueTiene(andy, rompecabezas(kittyYCompania, [5, 6, 7])).

test(personaDeimagenMasAtractivaQueTieneEsInversible, set(Persona == [andy, flor])):-
  imagenMasAtractivaQueTiene(Persona, brillante(metalizado, kitty)).

test(imagenMasAtractivaDeimagenMasAtractivaQueTieneEsInversible, set(ImagenMasAtractiva == [brillante(metalizado, kitty)])):-
  imagenMasAtractivaQueTiene(andy, ImagenMasAtractiva).

:- end_tests(imagenMasAtractiva).

:- begin_tests(hizoNegocio).

test(florHizoNegocioConElCanjeConBobby, nondet):-
  hizoNegocio(flor, canje(bobby, [4, 6])).

test(bobbyNoHizoNegocioConElCanjeConFlor, fail):-
  hizoNegocio(bobby, canje(flor, [1])).

test(personaDehizoNegocioEsInversible, set(Persona == [flor])):-
  hizoNegocio(Persona, canje(bobby, [4, 6])).

test(medioDehizoNegocioEsInversible, set(Medio == [canje(andy, [1]), canje(bobby, [4, 6])])):-
  hizoNegocio(flor, Medio).

:- end_tests(hizoNegocio).

:- begin_tests(necesitaFigurita).

test(andyNecesitaLa8PorqueYaConsiguioTodasLasOtrasFiguritasDelAlbum, nondet):-
  necesita(andy, 8).

test(florNecesitaLa6PorqueLeFaltaYFormaParteDeUnRompecabezasQueYaComenzo, nondet):-
  necesita(flor, 6).

test(andyNoNecesitaLa3, fail):-
  necesita(andy, 3).

test(personaDenecesitaEsInversible, set(Persona == [andy])):-
  necesita(Persona, 8).

test(numeroFiguritaDenecesitaEsInversible, set(NumeroFigurita == [8])):-
  necesita(andy, NumeroFigurita).

:- end_tests(necesitaFigurita).