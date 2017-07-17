+++
title = "Distribuciones discretas con mónadas"
date = "2016-12-10"
tags = [ "haskell" ]
topics = [ "haskell" ]
+++

# El modelo

Como parte de una serie de ejemplos sobre uso de mónadas, he escrito un poco de código para modelar distribuciones discretas usando mónadas.

Por un lado, usa un [generador congruencial lineal](https://math.dartmouth.edu/archive/m20f11/public_html/RANDOMNESS_LCG.pdf) para generar números aleatorios; y por otro, usa la mónada [State](https://wiki.haskell.org/State_Monad) para pasar una semilla aleatoria de una función a otra que me permita seguir generado números aleatorios.

Por último, aporta un método que deriva [Show](http://www.zvon.org/other/haskell/Outputprelude/Show_c.html) para probar las distribuciones y dibujar un histograma de cualquiera de ella.


# Componiendo distribuciones

Lo más útil de esta idea para mí ha sido el poder generar unas distribuciones a partir de otras. La primera que genero es un dado de `n` caras usando la semilla. Aquí es donde se implementa el generador congruencial:

```haskell
dice :: Int -> Distribution Int
dice n = state (\s -> (s `mod` n + 1, 16807*s `mod` 2147483647))
```

Que funciona como una distribución uniforme:

```
>>> dice 6

1:       ################
2:       ################
3:       ################
4:       ################
5:       ################
6:       ################
```

Pero desde ella puedo generar fácilmente otras usando funciones que compongan distribuciones. Un ejemplo es usar `(⊕) = liftM2 (+)` para sumar dados:

```
>>> dice 6 ⊕ dice 6

2:       #####
3:       ##########
4:       ###############
5:       ####################
6:       ##########################
7:       ##############################
8:       #########################
9:       ####################
10:      ###############
11:      ##########
12:      #####
```


# Otras distribuciones

Y puedo crear otras distribuciones similares a partir de ellas, como la distribución de **Bernoulli** y la **Binomial** usando funciones para composición de mónadas:

```haskell
bernoulli :: Double -> Distribution Int
bernoulli p = do
  sample <- dice 1000000
  if (fromIntegral sample / 1000000.0 < p)
    then return 1
    else return 0

binomial :: Int -> Double -> Distribution Int
binomial k p = sum <$> replicateM k (bernoulli p)
```

Lo que me gusta de este código es que dejamos a la estructura de mónada encargarse internamente de el paso de la semilla de aleatoriedad y podemos componer distribuciones más simples para crear distribuciones más complejas.


# El código

El código completo se presenta aquí. Es un pequeño código que dejé de ejemplo pero que seguramente pueda mejorarse bastante; lo dejo aquí como guión para esta idea:

<script src="https://gist.github.com/M42/7d9d2ecf174bad73804e67fd5cb8de72.js"></script>
