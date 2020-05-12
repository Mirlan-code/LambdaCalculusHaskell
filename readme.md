Better readme
https://hackmd.io/_L-hJmJRRnauuN_ENEFeOA

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

## 2nd iteration

## Main evaluation rules
$$\frac{}{(\lambda x.e)\, v \rightarrow  e\,[v/x]}$$
$$\frac{e_1 \rightarrow e_1'}{e_1\,e_2 \rightarrow e_1'\,e_2}$$
$$\frac{e_2 \rightarrow e_2'}{v \,e_2 \rightarrow v\,e_2'}$$
$$\frac{}{((\lambda x.e_1) \oplus c) \, e_2 \rightarrow (\lambda x.e_1 \oplus c) \, e_2}$$

## Type checking rules

$$\frac{\Gamma,x:\tau_1 \vdash e : \tau_2}{\Gamma \vdash (\lambda x.e) : \tau_1 \rightarrow \tau_2}$$

$$\frac{\Gamma \vdash e_1:\tau_2 \rightarrow \tau_1 \qquad \Gamma\vdash e_2 : \tau_2}{\Gamma \vdash e_1 \, e_2: \tau_1}$$


$$\frac{\Gamma \vdash e_1:\tau_3 \rightarrow \tau_2 \qquad \Gamma\vdash e_2 : \tau_1 \rightarrow \tau_3}{\Gamma \vdash e_1 \, e_2: \tau_1 \rightarrow \tau_2}$$


## Recursion
The recursion is done using the fix-point combinator. I won't explain the whole theory I will just show how it magically works.
For good understanding check this nice talk:
[Ruby Conf 12 - Y Not- Adventures in Functional Programming by Jim Weirich](https://www.youtube.com/watch?v=FITJMJjASUs)

$fix \;\lambda f . \lambda n. (body)$

Let's see how an infinite function looks like 

$fix \;\lambda f . \lambda n. n * f \>(n-1)$

This is equivalent to : $n*(n-1)*(n-2)*(n-3)...$

I have used such function for simplicity

Now here's evaluation rules:

$$\frac{e \rightarrow e'}{fix \; e \rightarrow fix \; e'}$$

$$\frac{}{fix \;\lambda f .e \rightarrow e\, [fix \;\lambda f .e / f]}$$


Let's try it:

$(fix \;\lambda f . \lambda n. n * f \>(n-1)) \, (3)$ ~ $\lambda n. n * f \>(n-1) [fix \;\lambda f .e / f] \rightarrow$
$(\lambda n. n * (fix \;\lambda f . \lambda m. m * f \>(m-1))(m-1)) (3)$ ->
$3 * (fix \;\lambda f . \lambda m. m * f \>(m-1))  (2)$ ->
$3* (\lambda m. m *  (fix \;\lambda f . \lambda k. k * f \>(k-1)) \>(m-1))(2)$ -> 
$3*2*  (fix \;\lambda f . \lambda k. k * f \>(k-1)) \>(1)$
You should have seen the recursion by now)

The compiler was upgraded and now it supports the following:
1. If/else and boolean clauses evaluation
2. Currying
3. Higher order functions
4. Static Type system (Curch & Curry)
5. Lambda recursion

Now you would need to give a type to the parameter of each lambda and the compiler will check the type before accepting/evaluating the instruction.

## Samples

### Sample1

simple max function evaluation test
max(3,5) must be evaluated to 5
```
 it "max evaluation test" $
            runeval Map.empty "\\x : Int y : Int . if x > y ? x : y $ 3 5" `shouldBe` (Literal (XInt 5))
```

### Sample 2
equality check function applied to 2 integers
3 == 3 must evaluate to True
```
it "equality test" $
            runeval Map.empty "\\x : Int y : Int . if x = y ? True : False $ 3 3" `shouldBe` (Literal (XBool True))
```

### Sample 3
Type checking for the previous equality function (not applied to anything though)
our function must be:
Int -> Int -> Bool
```
it "if with 2 normal clauses" $
            checktest "\\x : Int . \\ y : Int . if x = y ? True : False $ $ " `shouldBe` (Right $ TArr TInt (TArr TInt TBool))
```

### Sample 4
Type check error when a doing and between 2 integers
f(x : int ,y : int) = x & y 
should result in mismatch error 
expected bool received int

```
it "and beteween 2 integers should result in error" $
            checktest "\\x : Int y : Int . x & y $ 4 5" `shouldBe` (Left $ TypeMismatch TInt TBool)
```

### Sample 5
a lambda with if statement that may return 2 different  types

The function is equivalent to
```
f x 
    | x == 5 = x + 1
    | otherwise = True
```
```
it "if with 2 different return clauses" $
            checktest "\\x : Int . if x = 5 ? x + 1 : True $ 5" `shouldBe` (Left $ TypeMismatch TInt TBool)
```

### Sample 6
a sum function applied to one argument yields a function that accepts one argument

```
f :: Int -> Int -> Int
f x y = x + y
:t (f 4)
```

```
it "sum of 2 values applied to one is int->int" $
            checktest "\\x : Int . \\y : Int . x + y $ $ 4" `shouldBe` (Right $ TArr TInt TInt)
```

### Sample 7
Factorial recursion
5! = 120
```
(rec \\f : Int . \\n : Int . if n=0 ? 1 : (if n = 1 ? 1 : n * (f (n-1)))  $ $) 5
```


## Sample program 1:

```
f :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
f xx yy = xx . yy

g :: (Int -> Int) -> (Int -> Int)
g = f (\x -> x + 1)

h :: Int -> Int
h = g (\x -> x * 2)

a :: Int -> (Int -> Int)
a x = (\y -> y + x)

z :: Int -> Int
z = a 5

```
```
let F = (\x : Int -> Int . \y : Int -> Int . x y $ $);
let G = F (\x : Int . x + 1 $);
let H = G (\x : Int . x * 2 $);
let XX = G (\x : Int . True $);
let HH = F (\x : Int . x + 1 $) (\x : Int . x * 2 $);
H 5
let A = (\x : Int . \y : Int . x + y $ $);
let Z = A 5;
Z 1
Z 2
```

## Sample program 2:

```
f :: Int -> Int -> Int
f n m = 
    | m == 1 = n
    | otherwise = n * (f n (m - 1))

-- check type and do evals

g :: Int -> Int
g = f 2

-- check type and do evals
```

```
let P = (rec \f : Int -> Int -> Int . \n : Int . \m : Int . if m=1 ? n : n * (f n (m-1))  $ $ $) ;
let G = P 2;
G 3
G 4
```