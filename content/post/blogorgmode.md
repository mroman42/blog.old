+++
title = "Blog con org-mode y hugo"
date = "2017-07-17"
tags = [ "emacs" ]
topics = [ "emacs" ]
+++

Este blog se genera a partir de un sólo archivo de org-mode. Eso hace que sea más cómodo escribirlo y actualizarlo después. Usa internamente

-   una versión de [hugo-lithium-theme](https://github.com/yihui/hugo-lithium-theme) modificada por [Yihui Xie](https://github.com/yihui);
-   y el método que se describe en [este post](http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/) para implementar la exportación de subárboles independientes de org-mode.

Sólo ha sido necesario un pequeño cambio para conseguir que las fórmulas matemáticas en MathJax funcionen. Concretamente he añadido al código estas líneas

```emacs-lisp
(save-excursion 
 (goto-char (point-min)) (replace-string "\\\\(" "\\\\\\(")
 (goto-char (point-min)) (replace-string "\\\\)" "\\\\\\)")
 (goto-char (point-min)) (replace-string "\\\\[" "\\\\\\[")
 (goto-char (point-min)) (replace-string "\\\\]" "\\\\\\]")
)
```

Anteriormente usaba org-page, que funcionó siempre bastante bien salvo algún ligero problema con los temas y lo complejo que era en ocasiones actualizarlo o escribir algo nuevo.
