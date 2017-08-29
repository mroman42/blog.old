+++
title = "Try Mikrokosmos!"
date = "2017-08-28"
tags = [ "haskell", "lambda" ]
topics = [ "haskell" ]
+++

<script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.29.0/codemirror.min.js"></script>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.29.0/codemirror.css">
<script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.29.0/addon/mode/simple.min.js"></script>
<script src="https://m42.github.io/mikrokosmos-js/codemirrormkr.js"></script>
<script language="javascript" src="https://m42.github.io/mikrokosmos-js/rts.js"></script>
<script language="javascript" src="https://m42.github.io/mikrokosmos-js/lib.js"></script>
<script language="javascript" src="https://m42.github.io/mikrokosmos-js/out.js"></script>
<script language="javascript" src="https://m42.github.io/mikrokosmos-js/runmain.js"></script>
<script language="javascript" src="https://m42.github.io/mikrokosmos-js/mikrobox.js" defer></script>

<style>
.CodeMirror {
  border: 1px solid #eee;
  height: auto;
  overflow-y: hidden;
}
.CodeMirror-scroll {
  height: auto;
  overflow-y: hidden;
  overflow-x: auto;
}
.mikrojs-console {
  height: auto;
}
pre {
    box-shadow: 0px 0px 1px #eee;
}
</style>

Thanks to [GHCJS](https://github.com/ghcjs/ghcjs) and [Codemirror](http://codemirror.net/), you can now try the [Mikrokosmos](https://github.com/M42/mikrokosmos) lambda interpreter in your browser. Press the **evaluate** button below!

<div class="mikrojs-console">
<script type="text/mikrokosmos">
# Lambda expressions are written with \ or λ, as in
(λx.x)
(\x.\y.x)(\x.x)

# Libraries available
plus 2 3
sum (cons 1 (cons 2 (cons 3 nil)))

# Untyped, but also simply-typed λ-calculus
:types on
swap = \m.(snd m, fst m)
swap

# Gentzen-style deduction trees
@@ \z.(snd z,fst z)</script>
</div>

With this interpreter on my toolkit, I plan to write tutorials on the &lambda;-calculus and the Curry-Howard isomorphism soon. Stay tuned!
