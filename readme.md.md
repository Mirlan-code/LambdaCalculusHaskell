# Lambda Calculus Compiler
## Hussain Kara Fallah - Almir Mullanorov

The goal was to build an interpreter for applied lambda calculus. 

### BNF

```
<Lang> = <LetExpression> | <LambdaExpression>
<LambdaExpression> = <Applicable> (<Applicable> | <Nothing>)
<Applicable> = <LambdaAbstraction> | <Arithmetic>
<LambdaAbstraction>= "Abs " <var> "." <LambdaExpression>
<Arithmetic> = <Term> ( <Operation> <Arithmetic> | <Nothing> )
<Term> = <single> | ( <leftbrack> <LambdaExpression> <Rightbrack> )
<Single> = <Var> | <Number> | <Boolean>
```

### AST
![](https://i.imgur.com/DphpZOc.png)


### Basic Syntax:

Let operations are used to write global definitions for the whole program.

```
Let IDENTIFIER = LAMBDA_EXPRESSION ;
```

Identifier must be an UpperCase Word
Must have a valid lambda expression on the right hand.


Lambda expressions are wrote in the following way:

```
\x . x + 1 $ 10
```

The dot separates the variable from the body
The dollar declares the end of the body
Other examples could be:

```
\x.\y.y$$ ((\x.x$) (\y.y$))
\x y . x + y $ (\z.z+1 $ 10) (2 + 9)
```

## Samples:


#### Sample1:

$f(x,y,z)=10$

```
\x y z . 10 $ 500 250
```

This lamba expression is reduced 10


#### Sample2:

A function that checks that at least 2 of 3 variables are true

```
\x y z . (x & y) | (x & z) | (y & z) $ True False False
```

#### Sample 3:

f(x,y,z) = (x + 1) * (y + 2) * (z + 3)

```
\x y z . (\a.a+1 $ x) * (\a.a+2 $ y) * (\a.a+3 $ z) $ 0 0 0
```


#### Sample4

$f(x,y,z) = Inc(x) * Dec(y) * (z + 2)$
$Inc(x) = x+1$
$Dec(x) = x-1$
$f(1,3,0) = 2*2*2=8$

```
let INC = \x.x+1$; 
let DEC = \x.x-1$;
let T = True;
\x y z . (INC x) * (DEC y) * (z + 2) $ (0 + 1) 3 0
```

#### Sample5

$(\lambda x. \lambda y . y) \, ( \, ( \lambda x.x ) \, ( \lambda y.y ) )$

This one is equivalent to:

$\lambda y.y$

```
\x.\y.y$$ ((\x.x$) (\y.y$))
```

### Sample 6:

$(\lambda x \, y \, z . x \, y \, z) (\lambda x. x \, x) (\lambda y.y)$

This one is eqivalent to:

$x$

```
(\x y z . x y z $) (\x . x x $) (\x . x $) x
```