+++
title = "Yoneda lemma"
date = "2017-02-17"
tags = [ "math" ]
topics = [ "math" ]
+++

# Lema de Yoneda

Sea \\(G : {\cal C} \longrightarrow \mathtt{Set}\\) un funtor covariante. Fijado \\(A \in obj({\cal C})\\), tenemos una biyección entre las transformaciones naturales del funtor \\(Hom(A,-)\\) a \\(G\\) y los elementos del conjunto \\(G(A)\\):

\\[ y : Nat(Hom_{\cal C}(A,-),G) \longrightarrow G(A) \\]

Que viene dada por \\(y(\tau) = \tau_A(1_A)\\), la imagen de la identidad por la transformación natural.


## Demostración

Dado cualquier \\(p\\) crearemos la única transformación natural que cumple \\(\eta_A(1_A) = p\\). Por definición de transformación natural, sabemos que debe cumplir el siguiente diagrama conmutativo:

![img](//raw.githubusercontent.com/M42/m42.github.io/images/yonedaproof1.jpeg)

Lo que deja determinado a cualquier \\(\eta_B(f)\\), y por tanto a toda la función:

\\[\eta_B(f) = \eta_B(f\circ id) = Gf(\eta_A(id_A)) = Gf(p) \\]

Nos falta comprobar que la función así construida es de hecho una transformación natural. Es decir, que cumple el siguiente diagrama conmutativo:

![img](//raw.githubusercontent.com/M42/m42.github.io/images/yonedaproof2.jpeg)

Y de hecho, dado cualquier elemento \\(f \in Hom(A,B)\\) tenemos:

\\[Gg\circ \eta(f) = Gg \circ Gf(p) = G(g\circ f)(p) = \eta(g\circ f)\\]


# Lema de Yoneda (caso contravariante)

Si aplicamos Yoneda sobre \\(\mathcal{C}^{op}\\), dado \\(G : {\cal C} \longrightarrow \mathtt{Set}\\) **contravariante** y fijado \\(A \in obj({\cal C})\\); existe una biyección entre las transformaciones naturales del funtor \\(Hom(-,A)\\) a \\(G\\) y los elementos del conjunto \\(G(A)\\):

\\[ y : Nat(Hom_{\cal C}(-,A),G) \longrightarrow G(A) \\]

Que viene de nuevo dada por \\(y(\tau) = \tau_A(1_A)\\).


# Referencias y enlaces

[1] J. Rotman, An Introduction to Homological Algebra.

[2] Bartosz Milewski's Programming Cafe. [The Yoneda Lemma](https://bartoszmilewski.com/2015/09/01/the-yoneda-lemma/)

[3] The Catsters. [Representables and Yoneda 3](https://www.youtube.com/watch?v=TLMxHB19khE)
