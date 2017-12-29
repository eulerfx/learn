module Learn

/// A learner for function type 'a -> 'b.
type Learner<'p, 'a, 'b> = {  
  
  /// An function parameterized by 'p implementing 'a -> 'b.
  i : 'p * 'a -> 'b
  
  /// Updates the parameter 'p based on training pair ('a,'b).
  u : 'p * 'a * 'b -> 'p
  
  /// Requests an input 'a based on parameter 'p and training pair ('a,'b).
  r : 'p * 'a * 'b -> 'a

}

/// Defines the category Learn of learners.
module Learner =

    /// The identity learner.
    let id<'p, 'a> : Learner<'p, 'a, 'a> =
      {
        i = fun (p,a) -> a
        u = fun (p,a,b) -> p
        r = fun (p,a,b) -> a
      }

    /// Creates a learner for 'a -> 'c given learners for 'a -> 'b and 'b -> 'c.
    let compose (g:Learner<'q, 'b, 'c>) (f:Learner<'p, 'a, 'b>) : Learner<'p * 'q, 'a, 'c> =
      {
        i = fun ((p,q),a) -> 
              let b = f.i (p,a)        // feed inputs to f
              let c = g.i (q,b)        // feed outputs of f into g
              c
        
        u = fun ((p,q),a,c) -> 
              let b' = f.i (p,a)        // produce training pair (b',c) : ('b * 'c) for g     
              let q' = g.u (q,b',c)     // train g
              let b'' = g.r (q,b',c)    // request b'' : 'b to form pair (a,b'') : ('a * 'b) to train f ; backpropagation
              let p' = f.u (p,a,b'')    // train f          
              (p',q')
        
        r = fun ((p,q),a,c) -> 
              let b' = f.i (p,a)        // produce pair (b',c) : ('b * 'c)
              let b'' = g.r (q,b',c)    // backpropagate from g
              let a' = f.r (p,a,b'')    // backpropagate from f
              a'
      }

    /// A cartesian product learner.
    let product (l1:Learner<'p, 'a, 'b>) (l2:Learner<'q, 'c, 'd>) : Learner<'p * 'q, 'a * 'c, 'b * 'd> =
      {
          i = fun ((p,q),(a,c)) -> l1.i (p,a), l2.i (q,c)
          u = fun ((p,q),(a,c),(b,d)) -> l1.u (p,a,b), l2.u (q,c,d)
          r = fun ((p,q),(a,c),(b,d)) -> l1.r (p,a,b), l2.r (q,c,d)
      }

    let braid (l:Learner<'p, 'a * 'b, 'c>) : Learner<'p, 'b * 'a, 'c> =
        {
            i = fun (p,(b,a)) -> l.i (p,(a,b))
            u = fun (p,(b,a),c) -> l.u (p,(a,b),c)
            r = fun (p,(b,a),c) -> l.r (p,(a,b),c) |> (fun (a,b) -> b,a)
        }