# dash
A toy lambda calculus implementation that will be simply typed one day.

## Syntax
**Types aren't yet implemented**, so there is no type syntax yet.

There's also no syntax for binding persistence, yet, but `letrec` works to label
expressions locally.

For highlighting dash code, tell your editor to highlight it as Scheme. This
will give you a rough enough approximation that it is useful.

Check out the `examples/` directory for an idea of what dash code looks like
(hint: it's like Scheme except uglier). If you're too lazy to do that, I
completely understand, and I don't blame you. Here are a few more examples:

#### Playing with logic and letrec.
```scheme
(letrec [$x=true $y=false]
  (if $x
      (letrec [$ohnoes="ohnoes"]
        $ohnoes)
      (if $y
          "This shouldn't show"
          "nor should this, but it would if $x were false")))
```

#### Infinite loop
```scheme
(letrec [$x=(λy. ($x $y))] ($x 3))
```

## REPL

There's a repl included, called `dashrepl`. It's fun and you should use it.

# License
BSD-3, but it's a toy and you probably don't want it.
