---
title: What Python Can Learn from Haskell
...
---

```
        /|                          \ \
     /|^||     A Sighten Brown Bag   \ \ ____
  /|^|| ||     by Razzi Abuissa      / /\ ___
  ||_||_||                          / /  \
```

(watch this as a slideshow via using [patat](https://github.com/jaspervdj/patat/): `patat README.md`)

---


This talk has 3 parts:

- Intro to Haskell

> - Applying Haskell thinking to Python

> - Haskell & The Software Industry

---

# Intro to Haskell

Haskell is a programming language. Here is hello world.

```haskell
main = putStrLn "Hello World"
```

Let's compile and run it.

---

```sh
ghc hello.hs
./hello
Hello World
```

Haskell is compiled.

Fortunately, ghc comes with an interactive shell, ghci.

Let's experiment with it.

---

The syntax for calling functions in haskell is:

```haskell
function arg1 arg2
```

For example

```haskell
length "razzi53"

reverse "GBO"

take 5 [1..]
```

---

Haskell functions are defined by providing patterns and values.

```haskell
square x = x * x

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel 'y' = True
isVowel _ = False
```

Does anybody know the formula for the nth Fibonacci number?

---

## A simple program

```sh
$ ./squares
1, 4, 9, 16, 25, 36, 49, 64, 81, 100
```

---

We can write functions in any order, just about anywhere.

This allows us to write programs in a declarative way.


> - The whole program can be written as function composition

> - We could also use a list comprehension

---

## Characteristics of Haskell: Functional

```haskell
map sqrt [1..10]

filter even [1..10]

sum = foldl (+) 0
```

> - Haskell is not object oriented

> - Haskell is not imperative

---

## Haskell is Statically Typed

We haven't written any types yet, because Haskell can infer types.

Let's add some now.

---

## Haskell has Algebraic Data Types

```haskell
data Boolean = True | False

data Maybe a = Just a | Nothing

data List a = Empty | Cons a (List a)
```

---

## ProductType example


quote.hs:

```haskell
data ProductType = Loan | PPA
```

> - What happens when we add a new ProductType?

---

## Haskell is Pure

. . .

Python functions have 2 return values:

 - The return value itself
 - Any side effects

```python
def visit_grocery_store():
    print('Welcome to the store')
    return ['fruit', 'veggies']
```

---

In Haskell, side effects are encoded in the type.

```haskell
getLine :: IO String
putStrLn :: String -> IO ()
```

---

## Pure (continued)

- Can't do IO outside of IO

Functions are functions in the mathematical sense;
they always return the same value for the same input

```haskell
randomIO :: IO a
```

---

## Haskell is Lazy

```haskell
positiveNumbers = [1..]

main = print(take 10 positiveNumbers)
```

---

Haskell is a compiled, functional, statically-typed,
pure, lazy, general-purpose programming language.

. . .

Python is interpreted, functional, object-oriented,
imperative, and dynamically-typed.

> - Both are functional! There is overlap

---


# Part 2. What Python can Learn from Haskell

The benefits of being

 - functional
 - statically typed
 - pure
 - lazy

---

## Benefits of functional programming

- Functions are highly reusable

> - Functions are a lightweight abstraction - general and powerful.

> - Functions can be composed; passed as parameters

---

We can write 1 function and apply it to different shapes of data.

```haskell
> isQualified loan
True

> map isQualified quotes
[True, True, False]
```

---

We can combine functions to express business logic

```haskell
> geographyRule product site = ...

> utilityRule product site = ...

> rules = [geographyRule, utilityRule]

> siteEligible site = and [
  rule product site | rule <- rules
]
```

---


## Benefits of static typing

Haskell aims for correctness.

Haskell prevents calling functions with the wrong data.

Haskell won't allow "NoneType has no attribute..."
because these types are checked at _compile time_!

example: jira_issue.hs

---

## Benefits of static typing

- Fewer runtime errors

> - Less risky to add functionality

> - Easier to refactor

---

```haskell
import Text.Read

input <- getLine
readMaybe input :: Maybe Int
```

```python
n_cores = int(input('How many cores? '))

# ^ could runtime error
```

---

In python, handle exceptions with control flow

```python
try:
    cores = int(input('How many cores? '))
except ValueError:
    print('Invalid number of cores. Please pass an integer.')
```

---

## Benefits of being pure

Purity isolate interacting with outside world

- Outside actions can always fail

> - Functions are more easily testable

> - Keeping the IO (slow stuff) separate makes it easier to optimize

---

## Benefits of being lazy

- Unnecessary computation does not happen

- IO is deferred - we can pass actions around

---

## How can Python get these benefits?

- Functional: modular, declarative

- Static types: safe, maintainable

- Pure: testable, control over effects

- Lazy: performant, control over effects

---

## Functional: modular, declarative

Python is functional! Embrace it

```python
def street_address(site):
    overridden_address = site.get('overridden_address')

    return (
        overridden_address
        if overridden_address is not None
        else site['validated_address']
    )

addresses = [
    street_address(site)
    for site in sites
]
```

---

Choose simple data types such as lists and dictionaries for better modularity.

```python
def get_with_fallback(keys, data):
    """Gets the first non-null key in a dictionary."""
    for key in keys:
        value = data.get(key)
        if value is not None:
            return value

street_address = get_fallback_key(
    ['overridden_address', 'validated_address'],
    address_data
)
```

---

Avoid functions that act on heavy objects like model instances, requests, glossaries...

```python
def get_quote_data(quote):
    ...
    return glossary
```

```haskell
get_quote_data :: SolverRegistry -> DatabaseConnection -> Cache -> Environment -> Quote ->  ...
```

Keep functions simple and compose as necessary.

---

## Static types: safe, maintainable

Python has them! PEP 484: Type Hints

```python
def _build_get_rates_payload(address: str,
                             date_active: datetime.date,
                             lse_id: Optional[int] = None) -> dict:
```

---

## Static types

Sanitize user input in the view before passing to downstream functions.

Be explicit about what types a function should accept.

```python
# Avoid this

class Product:
    def get_from_db(cls, product_id, strict=False, user=None):
        if isinstance(product_id, SolarProduct):
            return product_id
        if uuid_or_pk(product_id):
            return get_from_id(cls, product_id, user=user)
```


---

## Pure: testable, control over effects

Most business logic can be separated from IO.

Write pure functions whenever possible.

Keep IO in IO territory: views

---

```python
# Avoid
def calculate_monthly_bill_presolar(solarsite):
    usage_8760 = extrapolage_usage(solarsite)
    utility = solarsite._utility
    ...

# Prefer
def calculate_monthly_bill_presolar(utility,
                                    utility_rate_schedule,
                                    utility_territory,
                                    usage_8760):

```

---

# Laziness: performance, control over effects

- Use generators when possible

> - Django querysets: defer evaluation

---

# Part 3: Haskell and the Sofware Industry

Haskell ideas and have influenced many programming languages

- Python comprehensions, optional static types

> - Java 8 has Optional

> - Scala: immutabile, functional

---

Haskell has been around since 1990. So why don't we use it?

> - Haskell is hard to learn

> - Small (but growing) adoption

> - Haskell development is an upfront investment

---

Who does use Haskell?

- Computer science researchers

> - Financial services

> - Facebook, tech giants, and some startups

---

## Other influences

PureScript: Haskell-like language compiling to JS

> - Elm: Haskell-like language compiling to JS+HTML webapps

> - Underscore, Lodash, Ramda: functional programming for JavaScript

---

## Conclusion

Learning different programming languages exposes us to different paradigms.

> - Haskell is a particularly different way of looking at programming.

> - Haskell programs are said to be correct and maintainable.

> - We should aim for this in our code.

---

# End

```haskell
questions :: IO [String]
```

## Resources

https://www.haskell.org

http://learnyouahaskell.com

http://ramdajs.com

