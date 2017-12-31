namespace Learn

/// A supervised learning algorithm for function type 'a -> 'b paremeterized by 'p.
/// Often times, 'p, 'a, 'b are Hilbert spaces, or more generally, spaces with an 
/// inner product, wherein functions are smooth.
type Learner<'p, 'a, 'b> = {  
  
  /// An function parameterized by 'p implementing 'a -> 'b.
  i : 'p * 'a -> 'b
  
  /// Updates the parameter 'p based on training pair ('a,'b) resulting
  /// in a new approximation to 'a -> 'b given by fun a -> i (p',a)
  u : 'p * 'a * 'b -> 'p
  
  /// Requests an input 'a based on parameter 'p and training pair ('a,'b).
  /// This embodies backpropagation as it propagates an indication of the value
  /// "backwards" across the layers.
  /// Example: Choose a desired output b for function 'a -> 'b. For any a, i(p,r(p,a,b),b)
  /// is closer to b then i(p,a,b).
  r : 'p * 'a * 'b -> 'a

}

/// Defines the category Learn of learners, where the objects are parameter spaces
/// and morphisms are learning algorithm. Composition of morphisms is defined via
/// composition of learners.
module Learner =

    /// The identity learner.
    let id<'a> : Learner<unit, 'a, 'a> =
      {
        i = fun (_,a) -> a
        u = fun ((),_,_) -> ()
        r = fun (_,a,_) -> a
      }

    /// Creates a learner for 'a -> 'c given learners for 'a -> 'b and 'b -> 'c.
    /// Sequential composition.
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

    /// Monoidal product (*) - aka parallel composition.
    /// l (*) id = l /\ id (*) l = l
    let product (l1:Learner<'p, 'a, 'b>) (l2:Learner<'q, 'c, 'd>) : Learner<'p * 'q, 'a * 'c, 'b * 'd> =
      {
          i = fun ((p,q),(a,c)) -> l1.i (p,a), l2.i (q,c)
          u = fun ((p,q),(a,c),(b,d)) -> l1.u (p,a,b), l2.u (q,c,d)
          r = fun ((p,q),(a,c),(b,d)) -> l1.r (p,a,b), l2.r (q,c,d)
      }

    let braid (l:Learner<'p, 'a * 'b, 'c>) : Learner<'p, 'b * 'a, 'c> =
        let swap (a,b) = b,a
        {
            i = fun (p,ba) -> l.i (p,swap ba)
            u = fun (p,ba,c) -> l.u (p,swap ba,c)
            r = fun (p,ba,c) -> l.r (p,swap ba,c) |> swap
        }

    /// Action of functor `Para -> Learn` on morphism in `Para`.
    /// Example:
    ///     Quadratic Error 
    ///     e(x,y) := 0.5 (x-y)^2
    ///     ∂e/∂x(x,-) = fun y -> x - y
    ///     Ei(p,a,b) = 0.5 (i(p,a) - b)^2
    ///     ∇pEi = ∑(i(p,a)-b)*∂i/∂p
    ///     
    let inline paramToLearn
        (rate:float)                       // learning rate
        (e:float * float -> float)         // error function
        (eg:float -> float)                // error function gradient
        (e_inv:float -> float)             // inverse error function
        (i:float[] * float[] -> float[])   // parameterized function
        (iga:float[] * float[] -> float[]) // partial derivative of i wrt a 
        (igp:float[] * float[] -> float[]) // partial derivative of i wrt p 
                                            : Learner<float[], float[], float[]> =
        let eip (p,a,b) = 
            let b' = i (p,a)
            (b,b') ||> Seq.zip |> Seq.sumBy e
        
        /// The gradient of eip. Exists because e is differentiable.
        let ei_gp (p,a,b) : float[] = failwith "gradient of eip wrt p"
        let sp s v : float[] = failwith "scalar product"
        let vm v1 v2 : float[] = failwith "vector substraction"
        let ei_ga (p,a,b) : float[] = failwith "gradient of eip wrt a" 
        let i_gp : float[] -> float[] = failwith "gradient of i"
        {
            i = i

            /// Gradient descent.
            u = fun (p,a,b) -> vm p (sp rate (ei_gp (p,a,b)))

            /// Backpropagation.
            r = fun (p,a,b) -> ei_ga (p,a,b) |> Array.map (fun a -> a |> eg |> e_inv)
        } 

    /// Bimonoid
    let inline mult (l1:Learner<unit, 'a, 'a>) (l2:Learner<unit, 'a, 'a>) : Learner<unit, 'a * 'a, 'a> =
        {
            i = fun ((),(a1,a2)) -> l1.i ((),a1) + l2.i ((),a2)
            u = fun _ -> ()
            r = fun ((),(a1,a2),a3) -> (a3 - a2),(a3 - a2)
        }
    
    let inline comult (l:Learner<unit, 'a * 'a, 'a>) : Learner<unit, 'a, 'a> =
        {
            i = fun ((),a) -> l.i ((),(a,a))
            u = failwith ""
            r = failwith ""// fun ((),(a1,a2) -> l.r ((),a1,s3
        }


/// The category of parameterized differentiable functions on Euclidean spaces.
/// The objects are Euclidean spaces and the morphisms are functions.
module Para =

    let id : float[] * float[] -> float[] =
        fun (_,a) -> a

    let compose (g:float[] * float[] -> float[]) (f:float[] * float[] -> float[]) : float[] * float[] -> float[] =
        fun (p,a) ->g (p,f (p,a))

