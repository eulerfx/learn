# Learn

This is a library for machine learning in F#. 

- A learning algorithm is a tuple `(P,i,u,r)` consisting of a parameter space `P`, a parameterized implementation function `P×A → B`, an update rule `P×A×B → P`, and backward propagation rule `P×A×B → A`, together forming a category `Learn`.
- Parameterized functions on Hilbert spaces, `P×A → B` with function composition as morphism composition form a category `Para`.
- Neural networks are morphisms with composition as network concatenation (layering) form a category `NNet`.
- A functor `L : Para → Learn` sends parameterized functions to learning algorithms using gradient descent and backpropagation given an error function (aka objective function) and a (learning) step size.
- A functor `I : NNet → Para` mapping the size of the network to the dimensionality of a space, and networks to parameterized functions give an activation function.
- Composing functors `I` and `L` gives us functor `NNet → Learn` which states that a neural network defines a learning algorithm, given an error function, step size and activation function.
- Differentiation is performed using [Automatic Differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation).
- Probability spaces are modeled using the [Giry monad](https://ncatlab.org/nlab/show/Giry+monad).


## Neural Networks

A neural network layer of type `(n1,n2):Nat × Nat` is `C ⊆ {1...n1} × {1...n2}` wherein `(i,j) ∈ C` denotes a connection between nodes `i` and `j`. The values `n1` and `n2` represent the number of nodes on each side of the layer and `|C|` is the number of connections. Given an activation function `σ : R → R`, a neural network layer defines a parameterized function of the form `P×A → B` where `P = R^(|C| + n2)`, `A = R^n1`, `B = R^n2`.

For example, the layer `C = {(1,1),{2,1},(2,2)} ⊆ [2]×[2]` has 3 connections and defines a parameterized function:

```fsharp
/// R^5 x R^2 → R^2
fun (((w11,w21,w22),(w1,w2)),a1,a2) → s(w11 * a1 + w1), s(w21 * a1 + w22 * a2 + w2)
```

A neural network is a sequence of layers `{(n0,n1),(n1,n2),...}` and the composition of these layers defines a composite parameterized function `P x R^n0 → R^nk`.

Categorically, a neural network is a method for defining parameterized functions. Smooth parameterized functions form a category `Para` where the objects are spaces, morphisms are functions and composition of morphisms is function composition. A learning algorithm based on a parameterized function is determined on the basis of a functor `Para → Learn` using gradient descent, given an error function `e` and step size `δ`.

## References

- [Beautiful Differentiation](http://conal.net/papers/beautiful-differentiation/beautiful-differentiation-long.pdf)
- [Backprop as Functor: A compositional perspective on supervised learning](https://arxiv.org/abs/1711.10455)
- [A Compositional Framework for Markov Processes](https://arxiv.org/abs/1508.06448)
- [Probabilistic Functional Programming in Haskell](https://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf)