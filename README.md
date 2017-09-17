# bluebin

A workalike for `defrecord` but which produces a *recyclable* object,
using Netty's [`Recycler`](https://netty.io/4.1/api/io/netty/util/Recycler.html)
class.

## Rationale

The idea is to reduce GC churn, where creating many record instances,
and having them be 

This is definitely experimental, and certainly is alpha status! The API
may break or be incomplete, this library may 

## Usage

```clojure
(require 'bluebin.core)

; Define a type like a record
(bluebin.core/defrecyclable Foo [bar baz])

; The arrow constructor takes all arguments positionally
(def foo (->Foo :barval :bazval))

; Instances behave like a map.
(:bar foo) ; => :barval
(:baz foo) ; => :bazval

; Recycling an object places it back in the thread-local pool,
; ready for reuse
(bluebin.core/recycle! foo)

; Trying to use a recycled object will fail
(:bar foo) ; => throws IllegalStateException

; The map-> constructor takes a map of arguments, missing
; values are set to nil
(def foo2 (map->Foo {:bar :quux}))

; Note you may get the same reference back. Handling this
; is up to you, like any manual memory management.
(identical? foo foo2) ; *might* return true

; You can also namespace keys
(bluebin.core/defrecyclable FooNamed [bar baz] :key-ns "foo.bar")
(def foo (map->FooNamed {:foo.bar/bar :quux}))
(:foo.bar/bar foo) ; => :quux
```

## License

Copyright © 2017 Casey Marshall

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
