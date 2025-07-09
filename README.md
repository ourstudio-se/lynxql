# Lynx – A Minimal DSL for Discrete Optimisation (July 2025)

**Lynx** lets you model MILP/SAT-style problems with just a few orthogonal

concepts: primitive values, type definitions, five logical combinators,

and a single optimisation call. Everything you write is guaranteed to

compile to a **linear/MILP** model; if you accidentally introduce

non-linear logic, Lynx aborts at compile-time.

---

## 0. Comment syntax

```
// single-line comment
/* 
multi
line 
comment
*/
```

---

## 1. Primitives and value ranges

| Kind | Syntax | Notes |
| --- | --- | --- |
| Boolean decision | `Bool()` | Built‑in 0 / 1 variable; shorthand: `x = 0..1` is equivalent to `x = Bool()` |
| Bounded integer | `Integer(min, max)` | Decision ∈ [min,max]. |
| Named range | `alias Week = Integer<1,52>` | Pure type synonym. |
| Sub-range variable | `Week(lo, hi)` | Bounds tightened at declaration time. |
| Protected bounds | *every* var exposes`lower`,`upper` | After solve:`lower == upper`. |

---

## 2. Defining types

Before the syntax, here is **what the example domain means**:

- **Color** — a single Boolean decision (pick or don’t pick). It carries one *attribute* `tag` so we can refer to colours symbolically.
- **Wheel** — another Boolean decision with attributes `tag` (e.g. `front`, `back`) and `weight`. Weight is numeric so we can penalise heavy wheels later.
- **Price** — a composite variable of type `All`; it is true only if **both** of its children are true (`colors`, `wheel`). It holds a single scalar attribute `price`. Modellers can hang an *unbounded* set of `Price` nodes under a bicycle, one per colour–wheel combination.
- **Bicycle** — the top‑level decision we want to optimise. It is also an `All`, meaning the bike exists iff its `colors` and `wheels` rules are satisfied. Extra attributes:
    - `@totalPrice` — linear aggregate of all active `Price` nodes; safe for the MILP objective.
    - `@heavyPen` — linear penalty based on any wheel weighing > 3 kg.
    - `@nlMetric` — a deliberately non‑linear attribute, computed **after** the solve for reporting.

With the semantics clear, here’s how that maps to Lynx syntax:

```
// leaf types
type Color : Bool {
    @tag   : str,
}

type Wheel : Bool {
    @tag    : str,
    @weight : float,
}

type Price : All {
    colors : Any[Color],
    wheel  : Wheel,
    @price : float,
}

// composite type
type Bicycle : All {
    colors      : ColorRule,
    wheels      : WheelRule,
    prices      : Free[Price],

    @totalPrice : float = (Bicycle b) -> sum(b.prices.price),
    @heavyPen   : float = (Bicycle b) -> 50 * Any(filter(b.wheels, w -> w.weight > 3.0)),
    @nlMetric   : float = (Bicycle b) -> sum(b.prices.price) - 1.2 * b.heavyPen,
}

```

- **`type`** declares either a *leaf* (Bool / Integer) or a *composite*(logical operator after the colon).
- Fields are separated by **commas** so you may keep everything on oneline if you like.
- Attributes are marked only at the declaration site with `@`; youaccess them via normal dot notation (`wheel.weight`).
- A right-hand side may be:
    - a **literal**
    - a **lambda** that resolves to a constant at call-time (compile-time
        
        for attributes, run-time for relationships).
        

### 2.1 Logical combinators (run-time)

| Name | Set form | Variadic form | Meaning |
| --- | --- | --- | --- |
| `All` | `All[S]` | `All(x1,…,xn)` | AND – true iff**every**child true |
| `Any` | `Any[S]` | `Any(x1,…,xn)` | OR – true iff**at least one**child true |
| `Not` | `Not[S]` | `Not(x1,…,xn)` | NOR – true iff**all**children false |
| `Exactly<k>` | `Exactly<k>[S]` | `Exactly<k>(x)`⇔`x == k` | Cardinality / equality |
| `AtMost<k>` | `AtMost<k>[S]` | `AtMost<k>(x)` | ≤ k |
| `AtLeast<k>` | `AtLeast<k>[S]` | `AtLeast<k>(x)` | ≥ k |
| `Free` | `Free[S]` | `Free(x1,…,xn)` | Children are**unconstrained**; truth values propagate as-is |

Each call returns a **Boolean variable**. Inside `suchThat` you simply provide the expression; Lynx implicitly locks it to **true** (`1`). You never write `== 1`; inside

`suchThat` Lynx *implicitly* forces the given Boolean to true.

---

## 3 Compile-time helpers

| Helper | Purpose | Allowed operators |
| --- | --- | --- |
| `filter(context, λ)` | static subset;`context`may be`*`(root) or any set expression | `&&`, ` |
| `firstTrue()` | on any set; returns the first member whose solved value is true | – |

A `lambda` is any `(param) -> expr` expression. Inside `filter` it must

be **static** (no decision values). Inside attributes it may reference

other attributes but **must stay linear** if the attribute itself is used

in `objective` or `suchThat`.

### 3.2 Spread operator `...`

`...expr` **unpacks** a static set (or list) into positional arguments when calling a constructor. Common use-case: turning the result of `filter` into the child list expected by `Any`, `All`, `Exactly`, or a rule constructor.

```
reds = filter(*, c -> c.tag == "red")
rule = ColorRule(...reds)   // same as ColorRule(red1, red2, …)

```

The operator works only on compile-time contexts (e.g., inside constructor calls or in default field values) and is a no-op after expansion: the compiler replaces it with the individual elements.

### 3.1 Detecting non-linearity

If an `@attribute` is referenced in the optimisation layer and the

compiler cannot linearise the expression, Lynx aborts with a clear error

message. Non-linear attributes are still computed **post-solve** for

reporting.

---

## 4 Optimisation call

```
solution = minimize(
    objective = <scalar expr>,      // MILP-safe
    suchThat  = <Boolean expr>,     // implicitly = true
    options   = { timeLimit = 300 } // optional
)

```

- `maximize` mirrors `minimize`.
- Build the Boolean with the combinators; wrap with `Not( … )` to force false.
- Named keys inside the Boolean (`bicycle = myBike`) create *local
references* for readability (identical to using the expression
directly).

---

## 5 End-to-end example

```
// leaf types
type Color : Bool { @tag : str, }
type Wheel : Bool { @tag : str, @weight : float }

type Price : All {
    colors : Any[Color],
    wheel  : Wheel,
    @price : float,
}

alias ColorRule = Exactly<1>[Color]
alias WheelRule = Exactly<1>[Wheel]

// composite
type Bicycle : All {
    colors      : ColorRule,
    wheels      : WheelRule,
    prices      : Free[Price],
    @totalPrice : float = (Bicycle b) -> sum(b.prices.price),
}

// data
red   = Color(tag = "red")
front = Wheel(tag = "front", weight = 3.2)

price1 = Price(colors = Any(red), wheel = front, price = 150)

// compile-time sets
aRed   = filter(*, c -> c.tag == "red")
frontW = filter(*, w -> w.tag == "front")

myBike = Bicycle(
    colors = ColorRule(...aRed),
    wheels = WheelRule(...frontW),
    prices = Free(price1),
)

alias Week = Integer<1,52>
productionWeek = Week(4,32)

solution = minimize(
    objective = myBike.totalPrice,
    suchThat  = All(
        myBike,
        Exactly<12>(productionWeek),
        Has(frontW.tag, "front"),
    ),
)

chosenColour = solution.colors.firstTrue()

```

The solver returns a *new, solved instance* of `Bicycle`; you drill down

through dot access, `firstTrue()`, or `filter(..., v -> v.lower == 1)` to

inspect any decision.

---

## 6 Key guarantees

- **Linear-safety** – optimisation layer refuses non-linear expressions.
- **No hidden operators** – only five combinators and the helpers above.
- **Immutable modelling** – optimisation call never mutates your originalobjects; results are returned as new solved instances.

Enjoy modelling with **Lynx**—minimal syntax, maximal clarity.