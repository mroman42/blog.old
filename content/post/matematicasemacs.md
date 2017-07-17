+++
title = "Matemáticas en emacs"
date = "2016-09-26"
tags = [ "emacs" ]
topics = [ "emacs" ]
+++

# Apuntes a ordenador

Tomar apuntes de matemáticas con el ordenador es una tarea difícil por la notación (símbolos, letras en otros alfabetos, índices&#x2026;) y la velocidad necesaria. Además, los diagramas, dibujos, flechas o anotaciones no textuales que tomamos parecen imposibles de transcribir fielmente a un fichero.

**Latex** permite notación matemática pero no facilita la velocidad (al fin y al cabo, Latex estaría pensado para edición de libros, no para tomar apuntes). **Markdown** simplifica mucho el poder escribir, pero pierde mucha potencia respecto a Latex. Así que mi solución es usar Emacs con **org-mode**.


# Ventajas de org-mode

**org-mode** tiene internamente un lenguaje de marcado similar al de markdown, con la ventaja para el usuario de Emacs de que está adaptado precisamente para Emacs. Permite escribir fórmulas en Latex y exportar luego a `.tex` y `.pdf`, controlando las opciones de Latex.

Una fórmula en Latex puede escribirse directamente en **org-mode** incluyéndola entre `\\( ... \\)`, si está dentro de una línea de texto (como en \\(i \ast x = x\\)); o entre `\\[ ... \\]`, cuando queremos que se muestre aparte del texto. Ejemplo:

\\[ \sum_{n=0}^\infty \frac{1}{2^n} \\]

Y cuando terminamos de escribirla podemos [previsualizarla](http://orgmode.org/worg/org-tutorials/org-latex-preview.html) directamente con `C-c C-x C-l`. Así vamos comprobando que hemos escrito las fórmulas correctamente a costa de acercarnos a un editor [WYSIWYG](https://es.wikipedia.org/wiki/WYSIWYG).


# Zoom

Un problema menor (y quizá sólo mío) al configurar todo esto fue que las fórmulas previsualizadas me parecían demasiado pequeñas. Aunque estén en proporción con el texto, cuesta más leerlas; y cuando aumentamos el tamaño del texto con `C-x C-+`, las fórmulas no se amplían con él. Para conseguir que lo hagan hay que incluir otro pequeño truco, mezcla de dos respuestas de [thisirs y Mark](http://emacs.stackexchange.com/questions/3387/how-to-enlarge-latex-fragments-in-org-mode-at-the-same-time-as-the-buffer-text) en Stack Overflow:

```lisp
(defun update-org-latex-fragment-scale ()
  (let ((text-scale-factor
         (expt text-scale-mode-step text-scale-mode-amount)))
    (plist-put org-format-latex-options
               :scale (* 1.2 text-scale-factor)))
)
(add-hook
 'text-scale-mode-hook
 'update-org-latex-fragment-scale)
```


# Aumentando la velocidad de escritura

Mi objetivo principal con todo esto era escribir matemáticas más rápidamente, así que [pregunté sobre autocompletado](http://emacs.stackexchange.com/questions/26322/math-autocompletion-in-org-mode) y concluí en usar `latex-math-mode`. Esto permite incluir comandos de Latex con atajos de teclado. En su configuración original usa el caracter `` ` `` para acceder a ellos, así que `` `-a `` escribe `\alpha`. Yo he decidido cambiar el acento invertido, que ya cuesta dos pulsaciones en el teclado en español, por la `ç`, que no la suelo usar. Además de los que incluye el paquete por defecto, se pueden escribir atajos propios.

Por otro lado, empecé a usar **yasnippets** en Emacs. Son cómodos, fáciles de programar, y me permiten simplificar tareas como escribir diagramas conmutativos o complejos simpliciales en pocos pasos.


# Diagramas conmutativos

Ahora estoy escribiendo sobre álgebra homológica y teoría de categorías, así que la mayoría de lo que escribo usa secuencias exactas y diagramas conmutativos.

Para las secuencias exactas, por ejemplo, tengo simplemente una plantilla con `yasnippet`, que me deja incluirlas escribiendo `complex_` y pulsando `<tab>`:

```text
# -*- mode: snippet -*-
# name: complex
# key: complex_
#--
\\[ $1 \overset{$6}\longrightarrow 
$2 \overset{$7}\longrightarrow 
$3 \overset{$8}\longrightarrow 
$4 \overset{$9}\longrightarrow 
$5 \\]
```

Para los diagramas conmutativos, la solución es un poco más compleja. El paquete **tikz** de Latex es muy útil para escribirlos pero es demasiado recargado en sintaxis, así que existe **tikz-cd**, que simplifica su sintaxis para centrarla en diagramas conmutativos. Para usarlo, hay que empezar por incluir en el archivo de configuración `init.el` las siguientes líneas:

```lisp
(add-to-list
 'org-latex-packages-alist '("" "tikz" t))
(eval-after-load "preview"
  '(add-to-list
    'preview-default-preamble
    "\\PreviewEnvironment{tikzpicture}"
    t))
```

Y además, en mi caso, tuve que cambiar el programa con el que generaba las imágenes. Por lo menos a mí, me parece funcionar sólo **imagemagick**:

```lisp
(setq org-latex-create-formula-image-program 'imagemagick)
```


# Cabeceras en Latex

Cuando necesitamos funcionalidad adicional que ofrece Latex en bibliotecas aparte, como usar `tikz-cd`, podemos incluirlas en la cabecera del archivo org como:

```
#+latex_header: \usepackage{amsthm}
#+latex_header: \usepackage{amsmath}
#+latex_header: \usepackage{tikz-cd}
```


# Ensanchar secciones

Para evitar tener que repetir varias veces la misma cabecera en varios archivos, podemos usar un sólo archivo para escribir matemáticas y fraccionarlo en secciones temáticas. Cuando necesitamos tratar una sección, podemos usar la funcionalidad de [org](https://www.gnu.org/software/emacs/manual/html_node/emacs/Narrowing.html) para tratar sólo una sección.
