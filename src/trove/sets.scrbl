#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(require (only-in scribble/core delayed-block))

@(define (set-method name #:alt-docstrings (docs "") #:contract (contract #f) #:return (return #f))
  (method-doc "Set" #f name #:alt-docstrings docs #:contract contract #:return return))

@(define s-of-a '(a-app (a-id "Set" (xref "sets" "Set")) "a"))
@(define s-of-b '(a-app (a-id "Set" (xref "sets" "Set")) "b"))
@(define l-of-a '(a-app (a-id "List" (xref "lists" "List")) "a"))
@(define opt-of-a '(a-app (a-id "Option" (xref "option" "Option")) "a")))
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
    (params)
    (args ("f" "st"))
    (return ,s-of-b)
    (contract
      (a-arrow
        (a-arrow "a" "b")
        ,s-of-a
        (a-app ,s-of-b))))
  (fun-spec
    (name "set-filter")
    (arity 2)
    (params)
    (args ("f" "st"))
    (return ,s-of-a)
    (contract
      (a-arrow
        (a-arrow "a" ,boolean)
        ,s-of-a
        (a-app ,s-of-a))))
  (fun-spec
    (name "set-all")
    (arity 2)
    (params)
    (args ("f" "st"))
    (return ,boolean)
    (contract
      (a-arrow
        (a-arrow "a" ,boolean)
        ,s-of-a)
        ,boolean))
  (fun-spec
    (name "set-any")
    (arity 2)
    (params)
    (args ("f" "st"))
    (return ,boolean)
    (contract
      (a-arrow
        (a-arrow "a" ,boolean)
        ,s-of-a)
        ,boolean))
  (fun-spec
    (name "set-find")
    (arity 2)
    (params)
    (args ("f" "st"))
    (return ,opt-of-a)
    (contract
      (a-arrow
        (a-arrow "a" ,boolean)
        ,s-of-a)
        ,opt-of-a))
  (fun-spec
    (name "set-partition")
    (arity 2)
    (params)
    (args ("f" "st"))
    (return
      (a-record
        (a-field "is-true" ,s-of-a)
        (a-field "is-false" ,s-of-a)))
    (contract
      (a-arrow
        (a-arrow "a" ,boolean)
        ,s-of-a
        (a-record
          (a-field "is-true" ,s-of-a)
          (a-field "is-false" ,s-of-a)))))
  (fun-spec
    (name "set-disjoint")
    (arity 2)
    (params)
    (args ("st1" "st2"))
    (return ,boolean)
    (contract
      (a-arrow
        ,s-of-a
        ,s-of-a)
        ,boolean))
  (fun-spec
    (name "is-subset")
    (arity 2)
    (params)
    (args ("st1" "st2"))
    (return ,boolean)
    (contract
      (a-arrow
        ,s-of-a
        ,s-of-a)
        ,boolean))
  (fun-spec
    (name "set-equal")
    (arity 2)
    (params)
    (args ("st1" "st2"))
    (return ,boolean)
    (contract
      (a-arrow
        ,s-of-a
        ,s-of-a)
        ,boolean))
  (fun-spec
    (name "power-set")
    (arity 1)
    (params)
    (args ("st"))
    (return (a-app (a-id "Set" (xref "sets" "Set")) ,s-of-a))
    (contract
      (a-arrow
        ,s-of-a)
        (a-app (a-id "Set" (xref "sets" "Set")) ,s-of-a)))
  (fun-spec
    (name "set-union")
    (arity 2)
    (params)
    (args ("st1" "st2"))
    (return ,s-of-a)
    (contract
      (a-arrow
        ,s-of-a
        ,s-of-a)
        ,s-of-a))
  (fun-spec
    (name "set-difference")
    (arity 2)
    (params)
    (args ("st1" "st2"))
    (return ,s-of-a)
    (contract
      (a-arrow
        ,s-of-a
        ,s-of-a)
        ,s-of-a))
  (fun-spec
    (name "set-symmetric-difference")
    (arity 2)
    (params)
    (args ("st1" "st2"))
    (return ,s-of-a)
    (contract
      (a-arrow
        ,s-of-a
        ,s-of-a)
        ,s-of-a))
  (fun-spec
    (name "set-to-list")
    (arity 1)
    (params)
    (args ("st"))
    (return ,l-of-a)
    (contract
      (a-arrow
        ,s-of-a)
        ,l-of-a))
  (fun-spec
    (name "set-add")
    (arity 2)
    (params)
    (args ("elt" "st"))
    (return ,s-of-a)
    (contract
      (a-arrow
        "a"
        ,s-of-a)
        ,s-of-a))
  (fun-spec
    (name "set-remove")
    (arity 2)
    (params)
    (args ("elt" "st"))
    (return ,s-of-a)
    (contract
      (a-arrow
        "a"
        ,s-of-a)
        ,s-of-a))
  (fun-spec
    (name "set-member")
    (arity 2)
    (params)
    (args ("st" "elt"))
    (return ,boolean)
    (contract
      (a-arrow
        ,s-of-a
        "a")
        ,boolean))
  (fun-spec
    (name "set-fold")
    (arity 3)
    (params)
    (args ("f" "base" "st"))
    (return "Base")
    (contract
      (a-arrow
        (a-arrow "Base" "Elt" "Base")
        "Base"
        (a-app (a-id "Set" (xref "sets" "Set")) "Elt"))
        "Base"))
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
Takes a function and set, and returns a set of the result of applying
the function to every element in the set.
Note that @pyret{set-map} is @bold{NOT} shape-preserving; that is, 
the result set may be smaller than the original set.
@examples{
  check:
    set-map(is-string, empty-set) is empty-set
    set-map(lam(n): n * 2 end, [set: 1, 0, 3, 5]) is [set: 2, 0, 6, 10]
    set-map(lam(n): n > 10 end, [tree-set: 3, 0, 10, 111]) is
      [set: true, false]
    set-map(is-number, [set: "P", "y", "r", "e", "t"]) is [set: false]
    set-map(lam(n): num-abs(n) end, [set: 6, -9, 4, -4]) is [set: 6, 9, 4]
  end
}

  @function["set-filter"]

Returns the subset of @pyret{st} for which @pyret{f(elem)} is true.

@examples{
  check:
    set-filter(is-string, empty-set) is empty-set
    set-filter(is-string, [set: 1]) is empty-set
    set-filter(is-string, [set: "A", "B", "C"]) is [set: "A", "B", "C"]
    set-filter(lam(n): n > 10 end, [set: 1, 2, 10, 100, -11]) is [set: 100]
  end
}



  @function["set-all"]

Returns true if the given predicate @pyret{f} is true for all elements in a set.

@examples{
  check:
    set-all(is-number, [set: ]) is true
    set-all(is-number, [set: 1]) is true
    set-all(is-number, [set: "A", "B"]) is false
    set-all(lam(n): n > 10 end, [set: 4, 10, 3, 11]) is false
    set-all(lam(n): n > 10 end, [set: 12, 200, 11]) is true
    set-all(lam(s): string-length(s) > 3 end, [set: "A", "C", "EEEE"]) is
      false
    set-all(lam(s): string-length(s) > 3 end, [set: "Five", "four", "floor"]) is
      true
  end
}


  @function["set-any"]

Returns true if the given predicate @pyret{f} is true for any element in the set.

  @examples{
    check:
      set-any(is-number, [set: ]) is false
      set-any(is-number, [set: 2]) is true
      set-any(lam(n): n < 5 end, [set: 0, 10, 22]) is true
      set-any(lam(n): n < 5 end, [set: 5, 7, 6]) is false
      set-any(lam(s): string-length(s) > 3 end, [set: "A", "C", "EEEE"]) is
        true
    end
  }


  @function["set-find"]

Returns @pyret{some(elt)} for an element in @pyret{st} that satisfies @pyret{f(elt)},
or @pyret{none} otherwise.

Note that @pyret{set-find} may be non-deterministic; repeated calls may not produce the
same result for the same set. [TODO: currently, @pyret{set-find} always returns the same
value; should some performance be sacrificed for non-determinism?]

  @examples{
    check:
      set-find(is-number, [set: ]) is none
      set-find(is-number, [set: "A", "B"]) is none
      [set: 2, 3, 10].member(set-find(is-number, [set: 2, 3, 10]).value) is
        true
      set-find(lam(n): n < 10 end, [set: 10, -2, 20]) is some(-2)
    end
  }

  
  @function["set-partition"]

Partitions a set into two sets, one with elements for which @pyret{f(elt)} is true,
and one with elements for which @pyret{f(elt)} is false.

  @examples{
    check:
      set-partition(is-number, [set: ]) is
        {is-true: [set: ], is-false: [set: ]}
      set-partition(is-number, [set: 1]) is
        {is-true: [set: 1], is-false: [set: ]}
      set-partition(is-number, [set: "A", "B"]) is
        {is-true: [set: ], is-false: [set: "A", "B"]}
      set-partition(lam(n): n > 10 end, [set: 5, 6, 10, 20]) is
        {is-true: [set: 20], is-false: [set: 5, 6, 10]}
    end
  }


  @function["set-disjoint"]

 Returns true if two sets are disjoint.

  @examples{
    check:
      set-disjoint([set: ], [set: ]) is true
      set-disjoint([set: ], [set: 2]) is true
      set-disjoint([set: "A"], [set: ]) is true
      set-disjoint([set: "A"], [set: "A"]) is false
      set-disjoint([set: 1, 2, 3], [set: 2, 5, 6]) is false
      set-disjoint([set: "A", "B", "C"], [set: "X", "Y", "Z"]) is true
    end
  }


  @function["is-subset"]

Determines if a set @pyret{st1} is a subset of another set @pyret{st2}.

  @examples{
    check:
      is-subset(empty-set, empty-set) is true
      is-subset(empty-set, [set: 1, 5, 23]) is true
      is-subset([set: 4, 3, 2, 1], empty-set) is false
      is-subset([set: 1, 2], [set: 1, 2]) is true
      is-subset([set: 1, 3], [set: 2, 3]) is false
      is-subset([set: "hi", "he"], [set: "hi", "hello"]) is false
      is-subset([set: "abyss", "oryx", "lab"],
        [set: "abyss", "oryx", "lab", "castle"]) is true
    end
  }


  @function["set-equal"]

Determines whether two given sets are equal.

  @examples{
    check:
      set-equal(empty-set, empty-set) is true
      set-equal(empty-set, [set: 1, 2]) is false
      set-equal([set: "library", "gem", "crown"],
        [set: "library", "gem", "crown"]) is true
      set-equal([set: 1, 3, 2, 0, 5], [set: 1, 3, 0, 5]) is false
    end
  }

  
  @function["power-set"]

Generates the power set of a set.

  @examples{
    check:
      power-set(empty-set) is [set: empty-set]
      power-set([set: 1]) is [set: empty-set, [set: 1]]
      power-set([set: 1, 2]) is
        [set: empty-set, [set: 1], [set: 2], [set: 1, 2]]
      power-set([set: "A", "B", "C"]) is
        [set: empty-set, [set: "A"], [set: "B"], [set: "C"], [set: "A", "B"],
        [set: "A", "C"], [set: "B", "C"], [set: "A", "B", "C"]]
    end
  }


  @function["set-union"]
  @function["set-difference"]
  @function["set-symmetric-difference"]
  @function["set-to-list"]
  @function["set-add"]
  @function["set-remove"]
  @function["set-member"]
  @function["set-fold"]

}
