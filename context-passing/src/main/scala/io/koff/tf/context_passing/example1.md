First of all, let's define our types that we are going to use in this example.
* Context - is a representation of the context we are going to pass across our calls' stack
```scala
final case class User(name: String)
type Context = User
```
* Input - is a type for input params. For simplicity let's use this one type for all inputs.
```scala
type Input = String
```
* Output - is the same but for outputs of our functions.
```scala
type Output  = Int
```

The next step is to define our services. For demonstration purposes I am going to use these simple services' interfaces. Each one of them contains a single operation that is used by overlying services - `Layer1` uses `Layer2`, `Layer2` uses `Layer3` and so on.
```scala
trait Layer1[F[_]]:
  def operation1(in: Input): F[Output]

trait Layer2[F[_]]:
  def operation2(in: Input): F[Output]

trait Layer3[F[_]]:
  def operation3(in: Input): F[Output]

trait Layer4[F[_]]:
  def operation4(in: Input): F[Output]
```

```scala
import cats.mtl.Ask

```