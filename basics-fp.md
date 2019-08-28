# Basics of Functional Programming

This document is part of a quick introduction to basic concepts of functional programming (FP), using Scala as the language on which we apply them. 

Note that this document is not a comparison between FP languages, nor will it showcase implementations in other languages. The aim is to provide knowledge about some foundations of functional programming. These foundations have value by themselves and the implementation details (language used) are secondary to that value. By knowing these concepts, learning a new language with FP capabilities (like Rust) will be made easier.

The aim of this document is to provide basic concepts, along explanations on why are they good or used, to establish a foundation for later sessions. One of the complains when learning Functional Programming is the complexity of the topics, but quite often that complexity comes from trying to learn concepts (like Monads or Functions) without understanding why composition is good or how a Type class works. This short summary tries to fix that problem, so readers can be better prepared for the following sessions.

## Who is this aimed to

This document is aimed to people with working knowledge of Scala. The reader must be familiar with Scala's syntax and the tooling to run Scala programs. This means that the document will not go into details in some areas, like what is a `case class`, as it is assumed the reader has used these language features before.

More precisely, the reader would be familiar with the basics concepts explained at:
* https://underscore.io/books/creative-scala/
* https://books.underscore.io/essential-scala/
* https://www.scala-exercises.org/scala_tutorial/terms_and_types
* https://www.scala-exercises.org/std_lib/asserts

The concepts will be explained with a focus on 'pactical use'. This means that some concepts may not have all the theory behind them explained in this document, as it is not necessary to start using these concepts. Any gaps in knowledge may be made whole in later sessions when discussing other concepts, or by checking the abundant literature available online.

One example, as clarification: a reader can `map` over an `Option` value without knowing what a `Functor` is. Knowing about `Functor` is a good thing and makes the codebase better in the long term, so it will be explained in detail in later sessions, but it's not crucial to start using `Option` to avoid null-pointer exceptions.


## How to use the document

This document lists a series of concepts which are relevant to FP along some explanation. It has a companion project in Github:

* https://github.com/pvillega/fp-concepts/

Initially this document is support material for a short session (60-90m long) to be done with a presenter which will comment on the material, and where questions will be answered.

On their own, readers can continue reading the document and completing the exercises at their own pace. Any questions can then be discussed either in a meeting with other students and teachers, or directly in 1-to-1 sessions with some teacher.

The aim is that every reader can learn these concepts in a way that is more comfortable for them: in a group setting, or on their own time and at their own pace.


## What is Functional Programming 

Functional programming is a programming paradigm that treats computations as evaluations of functions, avoiding changing global state, side effects, and mutable data. Functions can be composed with other functions, creating new computations. This approach makes reasoning about software easier, avoiding surprises due to unexpected interactions. This document will go into detail about these concepts later on.

Functional programming is declarative, in the sense that we specify what we want to achieve, not the statements (steps) to achieve it. For example, if the reader wants to transform the elements of a `List[Int]` they would use `list.map(i => i + 1)` instead of specifying all the intermediate steps (iterate the list, modify the element, store the new value) as they would in a more imperative approach. 

This focus on the outcome means programs are usually written as a series of data transformation steps, instead of commands to execute in a given order. As a result, the code is generally more concise, and less code equals less bugs.

Scala has a statically typed system, what is called `typed functional programming`, which opens the door to  tools like `algebraic data types`. The advantage of typed systems is that they can detect errors at compile time, making  software more reliable. With type inference there is no additional typing required by the developer, as the compiler can understand the types expected.

NOTE: that said, in Scala type inference can, occasionally, misbehave. It is in the interest of the developer to help the compiler (and other developers reading the code) by providing return types on functions/methods as well as in any expression that transforms a type substantially.

At the beginning of the document it was mentioned that the document tries to focus on the practicalities of the concepts. An obvious question is: why to use functional programming instead of more stablished paradigms as OO and tooling like Java/Spring? 

The answer can be glimpsed by reading the previous section again: all the features of functional programming have a focus on reducing bugs and runtime errors, by virtue of both making it easier to write sound code and validating correctness at compile time. Functional programming is good for business. Weaker type systems like that of Java can't give the same guarantees. 


## Typed functional programming

Why do developers care about typed languages? Even with good type inference they require some extra boilerplate to make the compiler happy. Is that worth it?

Besides obvious answers like the dreaded `"x" is not a function` seen by too many Javascript developers, there is a more relevant aspect to this preference.

The [Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry–Howard_correspondence) proves that a type signature in a function is equivalent to a mathematical proof (barring side-effects in the implementation). This means that if a function has no side-effects, that function's signature is a test of correctness in itself. Not only it provides, for free, a set of tests for the code that would otherwise need to be implemented, it also restricts the possible implementations of that function.

Check this talk by Julien Truffaut about the subject: [Types vs Tests](https://skillsmatter.com/skillscasts/12648-types-vs-tests)

### Refined types

Scala has a good type system out of the box, but there are have tools that can help developers to use even more specific types.

[Refined](https://github.com/fthomas/refined) is a Scala library for refining types with type-level predicates which constrain the set of values described by the refined type. It allows a developer to define types like `PositiveInteger`, `NonEmptyString`, or `Int between 7 and 77`. All these are validated at compile time, and one can see how much more precision they can add to a given codebase.

It is out of the scope of this section to explain in detail its usage, but the reader is advised to read more about it and experiment with the library. 

## Composition

In mathematics, function composition is an associative operation that takes two functions and creates a new function. Mathematically composition is represented by `∘` as in `(g ∘ f )(x) = g(f(x))`  

In Scala code, it would be:

```
def f(a: Int): Int = ???
def g(b: Int): Int = ???

def h(c: Int) = g(f(c)) // new function by composition
```

Composition is a very basic concept but it is at the core of functional programming. What composition means is that a developer can break computations into fragments which they will `compose` later on into the desired program. 

Composition is the `magic sauce` behind which for comprehensions work, allowing a program to exit early on error cases while obtaining a result if all goes well:

```
def getValue(index: Int): Option[String] = ???
for {
	a <- getValue(0)
	b <- getValue(2)
} yield a + "_" + b
```

Composition is what allows a Scala program to be parameterised on `F[_]` (where `F` can be a `Monad`, and `Applicative`, or any of the other structures that will be seen later one) and integrate the results of all the calculations, without boilerplate necessary.

A good read about this subject is [Why FP: it's the composition](https://tech.iheart.com/why-fp-its-the-composition-f585d17b01d3) by Kailuo Wang. Beware, it goes into quite advanced concepts that are not explored in this document.


## Referential Transparency

The previous section mentioned how types can be mathematical proofs of our functions, barring side effects. This refers back to some of the properties listed when talking about functional programming: not mutating global state and not having side effects. This is commonly referred to as a function in functional programming being `referentially transparent`, or having `referential transparency`, or being `pure`.

Wikipedia says the following about referential transparency:

* The function always evaluates to the same result value given the same argument value(s). It cannot depend on any hidden state or value, and it cannot depend on any I/O.

* Evaluation of the result does not cause any semantically observable side effect or output, such as mutation of mutable objects or output to I/O devices.


The following is an example of a non-referentially transparent method, and also a common pattern in OO:

```
private var myValue = 0
def setter(newValue: Int): Unit = ???
def getter(): Int = myValue
```

Each call to `getter` may return a different value, depending if the codebase called `setter` at some point.

```
private var myValue = 0
val a = getter()
setter(2)
val b = getter()
assert(a == b) // error
```

A referentially transparent function doesn't depend on global state nor has side effects, like the following function:

```
def addOne(a: Int): Int = a + 1

val x = addOne(1)
val y = addOne(1)
assert(x == y) // true
```


Why do developers care about referential transparency? Referential transparency helps with reasoning of a codebase. By being referentially transparent, the developer can replace a call to a function by its implementation while not affecting the computation. For example:

```
def addOne(a: Int): Int = a + 1

val x = addOne(1)
val y = addOne(1)
assert(x == y) // true
```

becomes

```
def addOne(a: Int): Int = a + 1

val x = 1 + 1
val y = 1 + 1
assert(x == y) // true
```

This means that if a function (like `addOne`) is referentially transparent a developer doesn't need to understand the implementation details of that function, only the input and output types. This makes reasoning about a fragment of a codebase local, not having to take into consideration any external state, which is much simpler and less prone to error. 


As a comparison, given the following non-referentially transparent code:

```
var x = 0
def addOne() = { x = x + 1; x }

val a = addOne()
val b = addOne()
assert(a == b) // error
```

replacing the methods by the implementation:

```
var x = 0

val a = { x = x + 1; x}
val b = { x = x + 1; x}
```

but `a` and `b` are not the same due to the mutation of `x` that they cause, but which could also be cause by other areas of the codebase. Suddenly a developer working with this fragment of code needs to understand more of the codebase, to make sure no other method is modifying `x` in unexpected ways. Local reasoning isn't enough.


Besides facilitating reasoning, referential transparency provides other benefits:

- if two functions are referentially transparent and don't have any data dependency between them, they could be run in parallel without any risk
- if the output of a function is not used, the function can be removed safely
- if the program is referentially transparent, expressions can be reordered (based on data dependencies), pruned, or evaluated in a lazy way. This is why Haskell is lazy, for example.


Referential transparency can be achieved with less effort if the developer uses some techniques described in the next sections

### Immutability

Immutability means not changing the value of a variable once assigned. Scala helps by providing the keyword `val` to define immutable values.

Immutability helps with referential transparency as it disallows global state. For example:

```
val x = 0
def increment() = x + 1 
```

In the above example `increment` will always return `1` as `x` can't be modified. This helps the developer to reason better about any code using `increment`, and to simplify the implementation by removing unnecessary state in the codebase.

Immutability also refers to immutability in `collections`, like `List`. By making collections immutable a developer can be sure that they will not be modified by other methods, avoiding concurrency issues caused by the list changing while it is being used in our functions. Immutable collections also provide referential transparency, as can be seen in:

```
val l = List(1,2,3)
//...
val before = l
//...
val k = l.drop(1)  // l has not been modified here, new List created instead
//...
val after = l
assert(before == after) // true
```

which helps the developer with local reasoning of their code.


### Iteration in referential transparent code

A common use of mutable variables is iteration over collections. In imperative code, the standard pattern to iterate over an structure is similar to:

```
var i = 0
while(i < 10){
   // ...
   i = i + 1
}
```

Removal of mutability makes this impossible to implement. Given this is a commonly used pattern, what are the alternatives for a developer using immutable code?

The answer is Recursion. A recursive function is a function that:

* calls itself
* has a base case as an exit point

This is commonly used in functional programming and it is a pattern that a developer should become familiar with.

For example, we can use recursion to iterate over a list and add all its elements:

```
def sumAll(l: List[Int]): Int = {
 l match {
   case Nil => 0
   case x :: tail => x + summAll(tail)
 }
}
```

Thus using recursion we can avoid mutability in our code.

Be aware that the above could be implemented using methods provided by the standard library, like `foldLeft`. But the concept of recursion is very important to functional programming,
as a way to sidestep temporary mutable accumulators for your calculations.


### Error handling

The previous sections mentioned referential transparency as a way to facilitate reasoning about programs. Something that makes that reasoning much harder, but has become commonplace, are `Exceptions` as tool for error management. The following code:

```
def willFail(): Int = {
	callMethodThatMayThrow() + 1
}
```

is technically a lie. The fact that internally it calls a method a can throw an exception means these two calls may not be the same:

```
val a = willFail()
val b = willFail()
```

as one of them may raise an exception while the other won't. This adds a burden to the developer: at any point the developer must consider if an exception is possible and write defensive code against it, ending up with convoluted try-catch blocks that may be catching errors that will never happen. Exceptions are `implicit` in the context, and they don't compose either. That complicates things.
 
The basic solution to this problem is to turn the exception into two different data structures:

* Option - for cases where a result may not exist. Avoids mainly `NullPointerException`, given the convention where `null` is used to signal `absence`
* Either[Error, Ok] - to indicate if an operation has succeeded or failed

Note that the text said `basic solution`. There are more advanced patterns (spoiler: `MonadError`) which will be shown in later sessions.

The reader should be familiar with the use of both `Option` and `Either`. A simple example reworking the previously shown function:

```
def willFail(): Either[Exception, Int] = {
	callMethodThatMayFail().map(_ + 1)
}
```

shows that by making the error case explicit, the developer can reason about it. Furthermore, the codebase enforces handling of the error path (barring use of methods like `.get`) as otherwise the developer can't access the `int` value. And both `Option` and `Either` compose, allow the developer to build independent steps that can be joined into the final program.


A good refresher on why this is good practice can be found in the [Railway oriented programming](https://fsharpforfunandprofit.com/posts/recipe-part2/) blog post.

Please note that Scala works in the JVM and it is hold to particularities of the system. To facilitate interop between your errors and the JVM, it is recommended that you declare your error types as case classes
that inherit from `Exception` along the trait `NoStackTrace`.

Example:

```
final case class MyBadError(msg: String) extends Exception with NoStackTrace
``` 

### Effects and Side effects

The definition of referential transparency mentioned that `Evaluation of the result does not cause any semantically observable side effect or output`. In the literature there is a lot of talk about Effects and Side Effects, which can become confusing for a new developer. This section intends to explain them.

Effects can be defined as intended changes to a computation, something that the developer models and expect as a result of executing a function. Usually linked to `Monads`

Side-effects are unintended outcomes, not explicitly declared in a computation.

The difference can be understood better using the previous section as an example:

- an `Exception` is a side effect, as it is an unintended (and surprising) outcome in a computation. 
- `Either` represents an effect, in this case the `effect of managing errors in a computation`. It is also a `Monad`. 
- `Option` is another effect, the `effect of optionality of a value`

As the reader can see, an effect is intentional and explicit. A developer knows that a function handles an Effect as it is part of the signature:

```
def doRequest(): Either[Exception, String]
def findValue(): Option[Int]
```

while side-effects are not explicit. 

Please note an effectful function always declares the effect. Generalising, an effectful function will return `F[A]`, not `A`, as in `Option[A]`, `IO[A]`, etc. 

Effects are also referentially transparent, in opposition to side-effects. This point may seem counterintuitive, but it is very important.

Usually developers consider, given that the definition of referential transparency mentions `output to I/O devices` that a function that calls a web service will not be referentially transparent.
The confusion comes from the fact hat non-referential transparency is based on explicitness (note: this is technically not right but it will suffice for this explanation)

A developer can define a function:

```
def findValue(): Option[Int]
```

and this function could call a database (I/O) in the implementation. As a developer, we can still consider that implementation referentially transparent if:

- any error will return `None`
- if the value is missing, the call will return `None`
- if the value is present, the call will return `Some(value)`

which, if read again, can be seen to be the `rules` usually associated with the `Option` effect. These rules guarantee there are only 2 outcomes for that function, `Some` in case of success, `None` in case of failure or error. This helps the developer to reason about the function, without having to consider any side-effects: exceptions, time-outs, and other issues. 

When codebases are wrapped around `Either`, `IO`, or more generally `F` the aim is to enforce this guarantee: a developer only cares about `F` and its contract (how it declares error cases or success cases). The developer doesn't need to consider anything else. Which makes coding much easier.


This [talk by Rob Norris](https://www.youtube.com/watch?v=po3wmq4S15A) enters into more detail on the differences and shows additional examples.


#### Suspending side-effects

One characteristic of effects is that the developer is in control about their processing. An `Either` doesn't expose the error until tested for error conditions, for example using pattern matching. Before that it is an effect, `Either`. Some effects, like `IO`, go further and don't execute the computation until the command `run` (`unsafeSyncRun` or equivalent) is called by the program. Effects `suspend` the effect inside the `F`.

In contrast, side-effects are not controllable. A logger will write immediately. A `Future` starts the computation the moment it is created. The developer has no control, or not complete control, over them.


Side-effects may not be avoidable: a codebase needs logging, querying to a database, or sending messages to Kafka. All these are interactions which can be considered side-effectful, given that they break referential transparency. How can the developer acquire control?

The solution is to wrap the side-effect inside an effect. As defined previously, each effect has some intention associated with it (manage errors, optionality, estate). There are specific effects for things like dependency injection (`Reader`) or logging (`Writer`) that the reader can check if interested. The reality is that in our codebase usually the effect we use to wrap side-effects is `IO` (or `F` if parameterised)

A developer can turn a logging command into an effect as follows:

```
IO.delay(logger.info("my message"))
IO(logger.info("my message"))
``` 

Both `IO.delay` and `IO.apply` are equivalent, but when using abstracted code (which uses `F`) the developer will need to use `F.delay` as there is no `apply` available for the type. Given that it may be a good practice to always use `delay`.

After lifting, the developer can treat the side effect as an effect and work as usual with it, safely assuming it will behave as expected:

```
for {
  _ <- IO.delay(logger.info("my message"))
  result <- otherIOThing()
} yield result
```


### Everything is an expression

One of the consequences of referential transparency is that functions return either a value or an effect. That is the only way a function can be useful. As a logical consequence, all code that is written in the codebase must return either a value or an effect, otherwise it is not referentially transparent code.

This is the reason why in Scala structures like `if else` or `pattern matching` return a value.

Another consequence is that a codebase shouldn't have functions that return `Unit`, as that would indicate a non-referentially transparent function. What if the developer doesn't care about the result? Then that indicates that the function is doing some effect, in which case it can return `F[Unit]`, making the function referentially transparent and providing a effect that can be composed with the other parts of the codebase.


## Algebraic Data Types

Algebraic Data Types (ADT) are composite types, types that contain other types and form a data structure relevant to the program. In Scala they are implemented using `trait` and `case class`.

There are two common classes of ADT:

* Product types, also known as  Records, or Tuples
* Sum types, also known as Coproducts, or Disjoint unions


A `Product` is a wrapper of values, usually implemented as a `final case class` as follows:

```
final case class MyProduct(a: Int, b: String)
```

In the example above `MyProduct` is a `Product type`. The set of all the possible instances of `MyProduct` contains all possible instances of `Int` multiplied by all possible instances of `String`, 
which is the `Cartesian Product` of the sets of the values of the type, thus the name `Product`. The other name, `Tuple`, comes from the equivalence (if we focus only on the values) between `MyProduct` and `(Int, String)`. 

A `Sum` type is a set of mutually exclusive instances for a value, usually implemented as a `sealed trait` as follows:

```
sealed trait MySumType
final case class OneOption() extends MySumType
final case object SecondOption() extends MySumType
```

The set of possible values of the type is the sum of the possible values of each instance, thus the name `Sum Type`. An `Enumeration` is just a special `Sum Type` with a particular set of elements.

A `Sum Type` can have other `Products` or `Sum Type`, and a `Product` can take `Sum Types` as values.

ADT are powerful, for example a `List` in Scala can be implemented as a `Sum Type`:

```
sealed trait List[A]
case object Nil extends List[A]
final case class Cons(head: A, tail: List[A]) extends List[A]
```

### Benefits of ADTs

ADT are the main representation of data in functional programming. While in Object-Oriented programming the developer wraps data and methods that modify the data, in functional programming this can't be done as it would break referential transparency. The pattern is functional programming is to store the data as an ADT and pass that as input to pure functions that may use it.

Not only that, ADT have implicit guarantees that help creating safer code:

* Sum types can be pattern matched, and the compiled can verify exhaustivity. This means the developer can't forget instances, as the application won't compile
* Using ADT decouples data from the operations. The same set of ADT can be shared across operations while keeping the operations independent, in contrast to OO programming. 
 
Another benefit of ADT is that by having a sealed family of case classes (a single Sum type or Product type at the root) the developer has a known hierarchy of types. This knowledge is available at compile time,
and it allows a developer to manipulate that structure, as the boundaries are known. This is one of the core concepts of [Shapeless](https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#generic-representation-of-sealed-families-of-case-classes). In fact by understanding ADT and the concepts mentioned before, the reader can now understand how most of the Shapeless mechanism work, including `HList` and `Generic types`.

The practical applications of this are, for example, automatic creation of JSON encoders and decoders for an ADT (using circe and shapeless).  

### Reification and Church Encoding

Reification is a process by which a developer converts a method into a data structure plus some helper functions, turning something `abstract` into something `concrete`. Usually it converts every method 
in the OO representation into a case class in a sealed trait (an ADT), and then uses a pattern match to chose the action to take (a structural recursion).

For example:

```
def literal(v: Double): Double

final case class Literal(v: Double)
def eval(lit: Literal): Double
```

There is a good [post by Noel Welsh](https://underscore.io/blog/posts/2017/06/02/uniting-church-and-state.html) that explains this process in more detail. 

Reification is very useful as it converts actions into data (ADT). Storing actions as data allows the developer to build a tree of actions (using the ADT structure) that defines a series of steps to take, but are not executed until the developer decides so. The execution usually happens using an interpreter that evaluates the data structure. The fact the actions are known before hand allows the interpreter to optimise them, for example erasing duplicates or reordering computations.

This concept (actions as data) is (along other theory) one of the enablers of the [Free Monad](https://perevillega.com/understanding-free-monads). The Free Monad is used to implement libraries like `Doobie`, `Cats effect`, or `FS2 streams`. If a developer navigates the code of `IO` they will see the following:

```
def apply[A](body: => A): IO[A] =
   delay(body)

def delay[A](body: => A): IO[A] =
   Delay(body _)

```

When the previous sections mentioned delaying side-effects in a codebase by using `IO`, behind the scenes this is happening by storing the side-effect inside an ADT (`Delay`). This way the effect is `suspended` until the moment that instance of the ADT is interpreted, during program execution.

Having actions as data provides both control and referential transparency (the action won't happen until the developer decides so), and that is the reason this is a common pattern in functional programming.


## Higher order functions

A Higher Order Function is a function that at least does one of:

* takes one or more functions as an argument
* returns a function as its result

Higher order functions are key to functional programming, as they enable composition of functions, not just of the results of the functions themselves.

The reader is already familiar with at least one higher order function:

```
def map[A, B](l: List[A])(f: A => B): List[B]
```

Higher order functions allow the developer to extract common functionality and simplify the codebase. They are, for example, a full replacement of the [Strategy pattern](https://en.wikipedia.org/wiki/Strategy_pattern) used in Object Oriented programming.


Functions that return other functions, like:

```
def doTwice(f: Int => Int): Int => Int = f compose f
```

are also Higher Order Functions.

## Kind Types

Kind types (or type constructors) are types that are used to build other types. In the same way that a function can be given an input to generate a value, a 
type constructor can be given a type to generate another type.

If `Int` is a type, `List` is a type constructor as a developer can:

```
List + Int => List[Int]
List + String => List[String]
List + A => List[A]
```

Note that the example uses `List`, not `List[_]`. 

Type constructors are usually referenced using `*` to annotate their arity:


```
*            - a simple type that accepts no argument, like Int
* -> *       - a type that accepts one argument, like List or F to become List[Int], F[A], etc
(*,*) -> *   - a type that accepts two arguments, like Either
```

As with functions, there exist Higher Kinded types which received a type constructor as a parameter. They are of type `(* -> *) -> *` where the parentesis represent the type constructor provided.

An example would be the typeclass `Functor` which is defined as `trait Functor[F[_]]` and needs an `F` (a type constructor, as seen before) to construct an instance of the type.


# Typeclasses

Typeclasses are a long subject. This section will summarise core concepts but it is recommended for the reader to check [the relevant chapter](https://books.underscore.io/scala-with-cats/scala-with-cats.html#sec:type-classes) in `Scala with Cats` for additional examples. 

Typeclasses are a solution to the [expression problem](https://en.wikipedia.org/wiki/Expression_problem). The expression problem states that:

`The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety (e.g., no casts).`

Typeclasses allow us to extend existing libraries and types with new functionality, without using traditional inheritance, and without altering the original source code. They enable ad-hoc polymorphism, also known as overloading. 

## Creating a type class

Creating and using a typeclass is simple, as the developer just needs to follow an stablished pattern. The typical example on defining a type class is to define a Json serialiser that given the following ADT:

```
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json
```

can convert any value `A` into a `Json` value.

A typeclass is defined by creating a trait such as:

```
trait JsonWriter[A] {
  def write(value: A): Json
}
```

Note that the trait is parameterised.

Then instances for different types can be created, as implicit functions or values:

```
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

}
```

To use a typeclass, a method that expects an implicit of the given typeclass for a given type must be created:

```
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}
```

And last, the method can be used to make the conversion:

```
Json.toJson("A string") // Converting to JsString("A String")
```

This is a simple and repetitive pattern that can be applied to any type class. For example, a developer could create a type class `Show` to convert any element `A` for which there is a valid instance of the 
typeclass in context into a `String`. The reader should try now to implement `Show`.

### Laws

Some typeclasses come with laws associated. A law is a condition that is always true for any instance of the type class. This is most common on effects represented by typeclasses (`Functor`, `Monad`) but can be true of any typeclass. Not all typeclasses have laws, though.

If a typeclass has laws, a developer can then test its own implementations against the laws of the typeclass. If the tests pass, the developer is guaranteed the implementation will honor the contract of the typeclass. Furthermore, any code that can be lifted into operations of that typeclass is tested 'for free'.


### Typeclasses in the wild

Typeclasses are a foundation stone for libraries like Cats, where they are used to implement instances for concepts like `Functor`, or `Monad`, or to extend existing types like `Either` with utilities like `catchNonFatal` for error management. Typeclasses are the reason implicits are so popular for those libraries, as they are needed to bring the right typeclasses into scope. That is why when there is an import for implicits missing the code doesn't compile, as the instance is missing and the compiler can't add the relevant implementation for the typeclass.

A complete list of all typeclass instances in Cats is available [here](https://typelevel.org/cats/typeclasses.html) at the bottom of the page. Or the reader can check [this infographic](https://camo.githubusercontent.com/b4caa27f5df29a8b2c29b1a543ccc4599eb2dae4/68747470733a2f2f63646e2e7261776769742e636f6d2f74706f6c656361742f636174732d696e666f677261706869632f6d61737465722f636174732e7376673f63616368654275737465723d33).

## Polymorphism 

A function that has fully defined types is considered monomorphic. For example: `def sum: a: Int, b: Int): Int`

In contrast, a function for which a parameter is not fully defined, but instead written in a way that can handle different types, is considered `polymorphic`, `parametric polymorphic`, or `generic`.
Example: `def sum[A](a: A, b: A): A`

Polymorphism matters because it constraints the space of possible implementations of a function, forcing our code to be more correct. This is why a developer should try to create polymorphic Functions
whenever they can.

For example, write possible implementations for:

```
def foldString(s: String, f: String => Int): Int

def foldString[A](s: String, f: String => A): A
```

While the monomorphic function can have infinite implementations (one could return just `0`, ignoring both `s` and `f`) on the polymorphic function there is no way to generate an `A` unless we use both `s` and `f`.

Polymorphism can also be applied to effects:

```
def asList(a: Int): List[Int]

def asF[F[_], A](a: A)(implicit ev: Apply[F]): F[A]

def asF2[F[_]](a: Int): F[Int]
```

While `asList` could return many different `List`, like `Nil`, `asF` can only have a single implementation, using the fact `F` has an instance of `Apply`, as seen by the implicit. Polymorphism is so restrictive that, as is stands, a developer can't implement `asF2` as there is no constraint over `F` that hints on how to create an instance of the effect.


## What next?

After this wall of theory, next sessions will be focused on Cats library and the answer to the question:

`Why is a monad just a monoid in the category of endofunctors`

which will help the reader understand why `for comprehensions` work as they do, and build knowledge towards understanding of `IO` and other effects.



