# `math`
[🔗](https://github.com/erlang/otp/blob/master/lib/stdlib/src/math.erl#L22)

Mathematical functions.

This module provides an interface to a number of mathematical functions.

# `acos`

```erlang
-spec acos(X) -> float() when X :: number().
```

Returns the arc cosine of `X` in radians.

## Examples

```erlang
1> math:acos(1.0).
0.0
```

# `acosh`

```erlang
-spec acosh(X) -> float() when X :: number().
```

Returns the inverse hyperbolic cosine of `X`.

## Examples

```erlang
1> math:acosh(1.0).
0.0
```

# `asin`

```erlang
-spec asin(X) -> float() when X :: number().
```

Returns the arc sine of `X` in radians.

## Examples

```erlang
1> math:asin(0.0).
0.0
```

# `asinh`

```erlang
-spec asinh(X) -> float() when X :: number().
```

Returns the inverse hyperbolic sine of `X`.

## Examples

```erlang
1> math:asinh(0.0).
0.0
```

# `atan2`

```erlang
-spec atan2(Y, X) -> float() when Y :: number(), X :: number().
```

Returns the arc tangent of `Y`/`X` in radians, using the signs of both
arguments to determine the quadrant of the return value.

## Examples

```erlang
1> math:atan2(0.0, -10.0).
3.141592653589793
```

# `atan`

```erlang
-spec atan(X) -> float() when X :: number().
```

Returns the arc tangent of `X` in radians.

## Examples

```erlang
1> math:atan(0.0).
0.0
```

# `atanh`

```erlang
-spec atanh(X) -> float() when X :: number().
```

Returns the inverse hyperbolic tangent of `X`.

## Examples

```erlang
1> math:atanh(0.0).
0.0
```

# `ceil`
*since OTP 20.0* 

```erlang
-spec ceil(X) -> float() when X :: number().
```

Returns the ceiling of `X`.

## Examples

```erlang
1> math:ceil(7.5).
8.0
2> math:ceil(-5.5).
-5.0
3> math:ceil(1.0).
1.0
```

# `cos`

```erlang
-spec cos(X) -> float() when X :: number().
```

Returns the cosine of `X` in radians.

## Examples

```erlang
1> math:cos(0.0).
1.0
```

# `cosh`

```erlang
-spec cosh(X) -> float() when X :: number().
```

Returns the hyperbolic cosine of `X`.

## Examples

```erlang
1> math:cosh(0.0).
1.0
```

# `erf`

```erlang
-spec erf(X) -> float() when X :: number().
```

Returns the error function of `X`.

See [Error function](https://en.wikipedia.org/wiki/Error_function) (Wikipedia).

## Examples

```erlang
1> math:erf(0.0).
0.0
2> math:erf(10.0).
1.0
```

# `erfc`

```erlang
-spec erfc(X) -> float() when X :: number().
```

Returns `1.0` - [`erf(X)`](`erf/1`), computed using methods
that avoid cancellation for large `X`.

## Examples

```erlang
1> math:erfc(0.0).
1.0
```

# `exp`

```erlang
-spec exp(X) -> float() when X :: number().
```

Returns *e* raised to the power of `X`.

## Examples

```erlang
1> math:exp(0).
1.0
2> trunc(100 * math:exp(1)).
271
```

# `floor`
*since OTP 20.0* 

```erlang
-spec floor(X) -> float() when X :: number().
```

Returns the floor of `X`.

## Examples

```erlang
1> math:floor(9.1).
9.0
2> math:floor(-1.5).
-2.0
3> math:floor(1.0).
1.0
```

# `fmod`
*since OTP 20.0* 

```erlang
-spec fmod(X, Y) -> float() when X :: number(), Y :: number().
```

Returns the floating point remainder `X` divided by `Y`.

## Examples

```erlang
1> math:fmod(10.5, 8.0).
2.5
```

# `log2`
*since OTP 18.0* 

```erlang
-spec log2(X) -> float() when X :: number().
```

Returns logarithm of `X` to base 2.

## Examples

```erlang
1> math:log2(1.0).
0.0
2> math:log2(2.0).
1.0
3> math:log2(64).
6.0
```

# `log10`

```erlang
-spec log10(X) -> float() when X :: number().
```

Returns logarithm of `X` to base 10.

## Examples

```erlang
1> math:log10(1.0).
0.0
2> math:log10(10.0).
1.0
3> math:log10(100).
2.0
```

# `log`

```erlang
-spec log(X) -> float() when X :: number().
```

Returns the natural logarithm of `X`.

## Examples

```erlang
1> math:log(1.0).
0.0
2> math:log(2.718281828459045).
1.0
```

# `pi`

```erlang
-spec pi() -> float().
```

Returns the ratio of the circumference of a circle to its diameter.

## Examples

```erlang
1> math:pi().
3.141592653589793
```

# `pow`

```erlang
-spec pow(X, N) -> float() when X :: number(), N :: number().
```

Raise `X` to the power `N`.

## Examples

```erlang
1> math:pow(2, 6).
64.0
2> math:pow(10.0, 3.0).
1000.0
```

# `sin`

```erlang
-spec sin(X) -> float() when X :: number().
```

Returns the sine of `X` in radians.

## Examples

```erlang
1> math:sin(0.0).
0.0
```

# `sinh`

```erlang
-spec sinh(X) -> float() when X :: number().
```

Returns the hyperbolic sine of `X`.

## Examples

```erlang
1> math:sinh(0.0).
0.0
```

# `sqrt`

```erlang
-spec sqrt(X) -> float() when X :: number().
```

Returns the non-negative square root of `X`.

## Examples

```erlang
1> math:sqrt(2).
1.4142135623730951
2> math:sqrt(100.0).
10.0
```

# `tan`

```erlang
-spec tan(X) -> float() when X :: number().
```

Returns the tangent of `X` in radians.

## Examples

```erlang
1> math:tan(0.0).
0.0
```

# `tanh`

```erlang
-spec tanh(X) -> float() when X :: number().
```

Returns the hyperbolic tangent of `X`.

## Examples

```erlang
1> math:tan(0.0).
0.0
```

# `tau`
*since OTP 26.0* 

```erlang
-spec tau() -> float().
```

Returns the ratio of the circumference of a circle to its radius.

This constant is equivalent to a full turn when described in radians.

## Examples

```erlang
1> math:tau().
6.283185307179586
2> math:tau() == 2 * math:pi().
true
```

---

*Consult [api-reference.md](api-reference.md) for complete listing*
