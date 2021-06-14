#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(require (only-in scribble/core delayed-block))

@(define (set-method name #:alt-docstrings (docs "") #:contract (contract #f) #:return (return #f))
  (method-doc "Set" #f name #:alt-docstrings docs #:contract contract #:return return))

@(define s-of-a '(a-app (a-id "Set" (xref "sets" "Set")) "a"))
@(define s-of-b '(a-app (a-id "Set" (xref "sets" "Set")) "b"))
@(define l-of-a '(a-app (a-id "List" (xref "lists" "List")) "a"))
@(define boolean '(a-id "Boolean" (xref "<global>" "Boolean")))

@(append-gen-docs
  `(module "sets"
    (path "src/js/base/runtime-anf.js")
    (fun-spec
      (name "set")
      (arity 1))
    (fun-spec
      (name "list-to-set")
      (arity 1))
    (fun-spec
      (name "list-to-list-set")
      (arity 1))
    (fun-spec
      (name "list-to-tree-set")
      (arity 1))
    (fun-spec (name "list-set"))
    (fun-spec (name "tree-set"))
    (data-spec
      (name "Set")
      (type-vars (a-id "a"))
      (variants ("set"))
      (shared
        ((method-spec
          (name "fold")
          (arity 3)
          (params [list: leaf("b")])
          (args ("self" "f" "base"))
          (return (a-id "b"))
          (contract
            (a-arrow
              ,s-of-a
              (a-arrow "b" "a" "b")
              "b"))
        )
        (method-spec
          (name "size")
          (arity 1)
          (params)
          (args ("self"))
        )
        (method-spec
          (name "pick")
          (arity 1)
          (params)
          (args ("self"))
        )
        (method-spec
          (name "member")
          (arity 2)
          (params)
          (args ("self" "elt"))
          (return ,boolean)
          (contract
            (a-arrow
              ,s-of-a
              "a"
              ,boolean))
        )
        (method-spec
          (name "add")
          (arity 2)
          (params)
          (args ("self" "elt"))
          (return ,s-of-a)
          (contract
            (a-arrow
              ,s-of-a
              "a"
              ,s-of-a))
        )
        (method-spec
          (name "remove")
          (arity 2)
          (params)
          (args ("self" "elt"))
          (return ,s-of-a)
          (contract
            (a-arrow
              ,s-of-a
              "a"
              ,s-of-a)))
        (method-spec
          (name "to-list")
          (arity 1)
          (params)
          (args ("self"))
          (return ,l-of-a)
          (contract
            (a-arrow
              ,s-of-a
              ,l-of-a)))
        (method-spec
          (name "union")
          (arity 2)
          (params)
          (args ("self" "other"))
          (return ,s-of-a)
          (contract
            (a-arrow
              ,s-of-a
              ,s-of-a
              ,s-of-a)))
        (method-spec
          (name "intersect")
          (arity 2)
          (params)
          (args ("self" "other"))
          (return ,s-of-a)
          (contract
            (a-arrow
              ,s-of-a
              ,s-of-a
              ,s-of-a)))
        (method-spec
          (name "difference")
          (arity 2)
          (params)
          (args ("self" "other"))
          (return ,s-of-a)
          (contract
            (a-arrow
              ,s-of-a
              ,s-of-a
              ,s-of-a)))
        (method-spec
          (name "symmetric-difference")
          (arity 2)
          (params)
          (args ("self" "other"))
          (return ,s-of-a)
          (contract
            (a-arrow
              ,s-of-a
              ,s-of-a
              ,s-of-a)))
        ))
      )
  (fun-spec
    (name "size")
    (arity 1))
  (fun-spec
    (name "set-map")
    (arity 2)
    (params ())
    (args ("f" "st"))
    (return ,s-of-b)
    (contract
      (a-arrow
        (a-arrow "a" "b")
        ,s-of-a
        (a-app ,s-of-b)))
    (doc
      "Takes a function and set, and returns a set of the result of applying the function to every element in the set."))
  (fun-spec
    (name "set-filter")
    (arity 2)
    (params ())
    (args ("f" "st"))
    (return ,s-of-a)
    (contract
      (a-arrow
        (a-arrow "a" ,boolean)
        ,s-of-a
        (a-app ,s-of-a)))
    (doc
      "Returns the subset of st for which f(elem) is true."))


  ))

@docmodule["sets"]{

The interface to sets is in flux, and its design may change significantly in
the future.

@section{The Set Type}

@type-spec["Set" (list "a")]{

There are two underlying representations that sets may have.  List-based sets
work on all values that can be compared with the @pyret-id["equal-always"
"equality"] built-in function (this means that, for example, a set of functions
won't work).  List-based sets perform up to n comparisons on removal, addition,
and membership testing, where n is the number of elements in the set (in order
to give this guarantee, list-based sets don't store duplicate elements by
scanning the whole list on insertion).  Tree-based sets require that all
elements implement the @pyret{_lessthan} method in order to perform
comparisons, and guarantee that only up to log(n) less-than comparisons will be
performed for a set with n elements on removal, addition, and membership
testing.

There are no variants for @pyret-id{Set}s, and programs cannot use
@pyret{cases} statements with @pyret-id{Set}s.  Instead, they can be created
with the constructors below, and manipulated with the methods and functions
below.


Some methods, like @pyret-method["Set" "union"], combine multiple sets.  The
set on the left-hand side is the representation of the result.  For example, in

@pyret-block{
  [list-set: 1, 2].union([tree-set: 3, 4])
}

the result will be a @pyret{list-set}.
}
@section{Set Constructors}

@collection-doc["list-set" #:contract `(a-arrow ("elt" "a") ,(S-of "a"))]

Constructs a set out of the @pyret{elt}s.

@examples{
check:
  [list-set: 1, 2, 3] is [list-set: 1, 2, 3]
  [list-set: 1, 2, 2] is [list-set: 1, 2]
  [list-set: [list: 1], [list: 1], [list: 2]] is
    [list-set: [list: 2], [list: 1]]
end
}

@singleton-doc["Set" "empty-list-set" (S-of "a")]

An empty set.

@collection-doc["tree-set" #:contract `(a-arrow ("elt" "a") ,(S-of "a"))]

Constructs a set out of the @pyret{elt}s backed by a tree.  Raises an exception
if the elements don't support the @pyret{<} operator via @pyret{_lessthan}.

@examples{
check:
  [tree-set: 1, 2, 3] is [tree-set: 1, 2, 3]
  [tree-set: 1, 2, 2] is [tree-set: 1, 2]
  [tree-set: [list: 1], [list: 1], [list: 2]] raises "binop-error"
end
}

@singleton-doc["Set" "empty-tree-set" (S-of "a")]

An empty set backed by a tree.

@collection-doc["set" #:contract `(a-arrow ("elt" "a") ,(S-of "a"))]

Another name for @pyret-id{list-set}.

@function["list-to-list-set"
  #:contract (a-arrow (L-of "a") (S-of "a"))
  #:args (list (list "lst" #f))
  #:return (S-of "a")
]

Turn a list into a list-set.

@examples{
check:
  s1 = sets.list-to-list-set([list: 1, 2, 3, 3, 3])
  s1 is [list-set: 1, 2, 3]
end
}


@function["list-to-tree-set"
  #:contract (a-arrow (L-of "a") (S-of "a"))
  #:args (list (list "lst" #f))
  #:return (S-of "a")
]

Turn a list into a tree-set.

@examples{
check:
  s1 = sets.list-to-tree-set([list: 1, 2, 3, 3, 3])
  s1 is [tree-set: 1, 2, 3]
end
}


@function["list-to-set"
  #:contract (a-arrow (L-of "a") (S-of "a"))
  #:args (list (list "lst" #f))
  #:return (S-of "a")
]

Another name for @pyret-id["list-to-list-set"].

@section{Set Methods}

@set-method["add"]
@set-method["remove"]

@set-method["size" #:alt-docstrings "" #:contract (a-arrow (S-of "a") N) #:return N]

Get the number of elements in the set.

@examples{
check:
  [set: 1, 2, 3].size() is 3
  [tree-set: 1, 2, 3].size() is 3
  [list-set: 1, 2, 3].size() is 3
end
}

@set-method["member"]

Checks if @pyret{elt} is contained within this set (checking membership with
@pyret-id["equal-always" "equality"]).

@set-method["pick" #:alt-docstrings "" #:contract (a-arrow (S-of "a") (P-of "a" (S-of "a"))) #:return (P-of "a" (S-of "a"))]

@emph{Picks} an arbitrary element out of the set, and returns a
@pyret-id["Pick" "pick"] data structure.  If the set is empty, a
@pyret-id["pick-none" "pick"] is returned, otherwise a @pyret-id["pick-some"
"pick"] is returned, and the rest of the set (without the picked value) is
stored in the @pyret{rest} field of the @pyret-id["pick-some" "pick"].

@examples{
import pick as P
check:
  fun pick-sum(s):
    cases(P.Pick) s.pick():
      | pick-none => 0
      | pick-some(elt, rest) => elt + pick-sum(rest)
    end
  end

  pick-sum([set: 1, 2, 3, 4]) is 10

  [set:].pick() is P.pick-none
end
}

Note that the order of elements returned from @pyret-method["Set" "pick"] is
non-deterministic, so multiple calls to @pyret-method["Set" "pick"] may not
produce the same result for the same set.

@set-method["union"]
@set-method["intersect"]
@set-method["difference"]
@set-method["symmetric-difference"]

@set-method["to-list"]

@set-method["fold"]

Applies @pyret{f} to each element of the set along with the accumulator
(starting with @pyret{base}) to produce a new value.  Traverses elements in an
unspecified order.

@examples{
check:
  fun one-of(ans, elts):
    lists.member(elts, ans)
  end
  t1 = [tree-set: "1", "2", "3"]
  result = t1.fold(string-append, "")

  result is%(one-of) [list: "123", "132", "213", "231", "312", "321"]
end
}

@section{Set Functions}

These functions are available on the @pyret{sets} module object. Some of the
functions require the @pyret{sets} module to be @pyret{import}ed, as indicated
in the examples [TODO: which ones require this? Currently, we name each function
@pyret{set-<...>}; if we require the module to be imported, we can potentially
get rid of the prefix].

  @function["size"
    #:contract (a-arrow (S-of "a") N)
    #:args '(("st" #f))
    #:return N
    ]{

      Returns the number of elements in the @pyret{Set}.

      @examples{
        import sets as S
        check:
          S.size(empty-set) is 0
          S.size([set: 1, 0, 5, 3]) is 4
          S.size([tree-set: "Pyret", "rocks!"]) is 2
        end
      }
    }

  @function["set-map"]

Note that set-map is @bold{NOT} shape-preserving; that is, the result set may be smaller than the original set.
@examples{
  check:
    set-map(is-string, empty-set) is empty-set
    set-map(lam(n): n * 2 end, [set: 1, 0, 3, 5]) is [set: 2, 0, 6, 10]
    set-map(lam(n): n > 10 end, [tree-set: 3, 0, 10, 111]) is [set: true, false]
    set-map(is-number, [set: "P", "y", "r", "e", "t"]) is [set: false]
    set-map(lam(n): num-abs(n) end, [set: 6, -9, 4, -4]) is [set: 6, 9, 4]
  end
}

  @function[
    "set-filter"
    #:examples
    `@{
      check:
        set-filter(is-string, empty-set) is empty-set
        set-filter(is-string, [set: 1]) is empty-set
        set-filter(is-string, [set: "A", "B", "C"]) is [set: "A", "B", "C"]
        set-filter(lam(n): n > 10 end, [set: 1, 2, 10, 100, -11]) is [set: 100]
      end
    }
  ]

}
