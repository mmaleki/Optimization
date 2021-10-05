import Data.Function
import Data.List


diff :: (Float -> Float) -> Float -> Float -> Float
diff f x h = ((f (x+h)) -(f (x-h)))/(2 * h)




{- Dynamic of an ODE is x'(t) = f(x(t)). Euler's method recursively approximate the solution as follow
x[n+1] = x[n] + h * f(x[n])
x[0] = x0
-}
euler1 ::(Float -> Float) -> Float -> Float -> Float
euler1 f x0 h = x0 + h * (f x0)

euler :: (Float -> Float) -> Float -> Float -> Int -> [Float]
euler f x0 h 0 = (euler1 f x0 h):[]
euler f x0 h n = (euler1 f x0 h):(euler f (euler1 f x0 h) h (n -1))

{- Dynamic of an ODE is x'(t) = f(x(t)). Heun's method first do a prediction of the next step by Euler's method
then correct this prediction by making an average of the predictor and the current value of the function.
predictor[n] = x[n] + h * f(x[n])
x[n+1] = x[n] + h/2 * (f(x[n]) + f(predictor))
x[0] = x0
-}


heun1 ::  (Float -> Float) -> Float -> Float -> Float
heun1 f x0 h = let z = (euler1 f x0 h) in (x0 + h / 2 * ((f x0) + (f z)))


heun :: (Float -> Float) -> Float -> Float -> Int -> [Float]
heun f x0 h 0 = (heun1 f x0 h):[]
heun f x0 h n = (heun1 f x0 h):(heun f (heun1 f x0 h) h (n -1))

-- *Main> heun (\ x -> x ** 2) 2 0.01 5
-- [2.040808,2.0833156,2.1276312,2.1738727,2.2221684,2.2726583]
-- *Main> euler (\ x -> x ** 2) 2 0.01 5
-- [2.04,2.081616,2.124947,2.1701012,2.2171946,2.266354]

