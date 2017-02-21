# Gaussian Elimination

This implements the [Gaussian elimination algorithm](https://en.wikipedia.org/wiki/Gaussian_elimination) in PureScript, and can be used for solving [systems of linear equations](https://en.wikipedia.org/wiki/System_of_linear_equations).

## Usage

Let's say you have the following system of linear equations:

```
x      - z = 0
x  + y     = 1
-x + y + z = 4
```

Filling in the coefficients and the constant results in the following augmented matrix:

```purescript
let m = Matrix
 [ [  1.0, 0.0, -1.0, 0.0 ]
 , [  1.0, 1.0,  0.0, 1.0 ]
 , [ -1.0, 1.0,  1.0, 4.0 ]
 ]
```

Pass this to the `gauss` function defined in `Data.Gaussian`, and the result will be:
```purescript
> gauss m
[ -1.0, 4.0, -1.0 ]
```
wrapped in an `Either`.
