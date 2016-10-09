# dash
A toy lambda calculus implementation that will be simply typed one day.

## Silly examples

(Highlighted as ocaml, because I'm not sure which language is closest to
dash's Skye syntax).

### Function definition

```ocaml
fun x:nat y:nat := x:nat.
```

### Function application

```ocaml
(fun x:nat := x:nat) | 3   (* 3 *)
```

### Global context insertion

* (NOTE: Does not yet exist)
* (NOTE 2: Later on, with proper type inference, a lot of this syntax will clean
  up)

```ocaml
let a:nat := 1.
let const:nat=>nat=>nat := (fun x:nat y:nat := x:nat).
```

### Local context insertion

* (NOTE: Does not yet exist)
* (NOTE 2: Later on, with proper type inference, a lot of this syntax will clean
  up)

```ocaml
let constThree:nat=>nat := (fun x:nat := a:nat) where a:nat := 3.
```

# License
BSD-3, but it's a toy and you probably don't want it.
