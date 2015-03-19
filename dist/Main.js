// Generated by psc version 0.6.8
var PS = PS || {};
PS.Prelude = (function () {
    "use strict";
    
    function cons(e) {
      return function(l) {
        return [e].concat(l);
      };
    }
    ;
    
    function refEq(r1) {
      return function(r2) {
        return r1 === r2;
      };
    }
    ;
    
    function refIneq(r1) {
      return function(r2) {
        return r1 !== r2;
      };
    }
    ;
    
    function unsafeCompareImpl(lt) {
      return function(eq) {
        return function(gt) {
          return function(x) {
            return function(y) {
              return x < y ? lt : x > y ? gt : eq;
            };
          };
        };
      };
    }
    ;
    
    function boolAnd(b1) {
      return function(b2) {
        return b1 && b2;
      };
    }
    ;
    
    function boolOr(b1) {
      return function(b2) {
        return b1 || b2;
      };
    }
    ;
    
    function boolNot(b) {
      return !b;
    }
    ;
    var Unit = function (x) {
        return x;
    };
    var LT = (function () {
        function LT() {

        };
        LT.value = new LT();
        return LT;
    })();
    var GT = (function () {
        function GT() {

        };
        GT.value = new GT();
        return GT;
    })();
    var EQ = (function () {
        function EQ() {

        };
        EQ.value = new EQ();
        return EQ;
    })();
    var Functor = function ($less$dollar$greater) {
        this["<$>"] = $less$dollar$greater;
    };
    var Apply = function ($less$times$greater, __superclass_Prelude$dotFunctor_0) {
        this["<*>"] = $less$times$greater;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    };
    var Applicative = function (__superclass_Prelude$dotApply_0, pure) {
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
        this.pure = pure;
    };
    var Bind = function ($greater$greater$eq, __superclass_Prelude$dotApply_0) {
        this[">>="] = $greater$greater$eq;
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    };
    var Monad = function (__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
        this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
        this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
    };
    var Eq = function ($div$eq, $eq$eq) {
        this["/="] = $div$eq;
        this["=="] = $eq$eq;
    };
    var Ord = function (__superclass_Prelude$dotEq_0, compare) {
        this["__superclass_Prelude.Eq_0"] = __superclass_Prelude$dotEq_0;
        this.compare = compare;
    };
    var BoolLike = function ($amp$amp, not, $bar$bar) {
        this["&&"] = $amp$amp;
        this.not = not;
        this["||"] = $bar$bar;
    };
    var $greater$greater$eq = function (dict) {
        return dict[">>="];
    };
    var $eq$eq = function (dict) {
        return dict["=="];
    };
    var $less$times$greater = function (dict) {
        return dict["<*>"];
    };
    var $colon = cons;
    var $amp$amp = function (dict) {
        return dict["&&"];
    };
    var $dollar = function (f) {
        return function (x) {
            return f(x);
        };
    };
    var unsafeCompare = unsafeCompareImpl(LT.value)(EQ.value)(GT.value);
    var unit = {};
    var pure = function (dict) {
        return dict.pure;
    };
    var $$return = function (__dict_Monad_5) {
        return pure(__dict_Monad_5["__superclass_Prelude.Applicative_0"]());
    };
    var liftA1 = function (__dict_Applicative_8) {
        return function (f) {
            return function (a) {
                return $less$times$greater(__dict_Applicative_8["__superclass_Prelude.Apply_0"]())(pure(__dict_Applicative_8)(f))(a);
            };
        };
    };
    var eqString = new Eq(refIneq, refEq);
    var ordString = new Ord(function () {
        return eqString;
    }, unsafeCompare);
    
    /**
     *  | Returns its first argument and ignores its second.
     */
    var $$const = function (_72) {
        return function (_73) {
            return _72;
        };
    };
    var compare = function (dict) {
        return dict.compare;
    };
    var $less = function (__dict_Ord_12) {
        return function (a1) {
            return function (a2) {
                var _748 = compare(__dict_Ord_12)(a1)(a2);
                if (_748 instanceof LT) {
                    return true;
                };
                return false;
            };
        };
    };
    var $less$eq = function (__dict_Ord_13) {
        return function (a1) {
            return function (a2) {
                var _749 = compare(__dict_Ord_13)(a1)(a2);
                if (_749 instanceof GT) {
                    return false;
                };
                return true;
            };
        };
    };
    var boolLikeBoolean = new BoolLike(boolAnd, boolNot, boolOr);
    var ap = function (__dict_Monad_16) {
        return function (f) {
            return function (a) {
                return $greater$greater$eq(__dict_Monad_16["__superclass_Prelude.Bind_1"]())(f)(function (_2) {
                    return $greater$greater$eq(__dict_Monad_16["__superclass_Prelude.Bind_1"]())(a)(function (_1) {
                        return $$return(__dict_Monad_16)(_2(_1));
                    });
                });
            };
        };
    };
    return {
        Unit: Unit, 
        LT: LT, 
        GT: GT, 
        EQ: EQ, 
        BoolLike: BoolLike, 
        Ord: Ord, 
        Eq: Eq, 
        Monad: Monad, 
        Bind: Bind, 
        Applicative: Applicative, 
        Apply: Apply, 
        Functor: Functor, 
        unit: unit, 
        "&&": $amp$amp, 
        "<=": $less$eq, 
        "<": $less, 
        compare: compare, 
        refIneq: refIneq, 
        refEq: refEq, 
        "==": $eq$eq, 
        ap: ap, 
        "return": $$return, 
        ">>=": $greater$greater$eq, 
        liftA1: liftA1, 
        pure: pure, 
        "<*>": $less$times$greater, 
        cons: cons, 
        ":": $colon, 
        "$": $dollar, 
        "const": $$const, 
        eqString: eqString, 
        ordString: ordString, 
        boolLikeBoolean: boolLikeBoolean
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    
    function returnE(a) {
      return function() {
        return a;
      };
    }
    ;
    
    function bindE(a) {
      return function(f) {
        return function() {
          return f(a())();
        };
      };
    }
    ;
    var monadEff = new Prelude.Monad(function () {
        return applicativeEff;
    }, function () {
        return bindEff;
    });
    var bindEff = new Prelude.Bind(bindE, function () {
        return applyEff;
    });
    var applyEff = new Prelude.Apply(Prelude.ap(monadEff), function () {
        return functorEff;
    });
    var applicativeEff = new Prelude.Applicative(function () {
        return applyEff;
    }, returnE);
    var functorEff = new Prelude.Functor(Prelude.liftA1(applicativeEff));
    return {
        bindE: bindE, 
        returnE: returnE, 
        functorEff: functorEff, 
        applyEff: applyEff, 
        applicativeEff: applicativeEff, 
        bindEff: bindEff, 
        monadEff: monadEff
    };
})();
var PS = PS || {};
PS.Debug_Trace = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    
    function trace(s) {
      return function() {
        console.log(s);
        return {};
      };
    }
    ;
    return {
        trace: trace
    };
})();
var PS = PS || {};
PS.Data_Maybe = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Alt = PS.Control_Alt;
    var Control_Alternative = PS.Control_Alternative;
    var Control_Extend = PS.Control_Extend;
    var Control_MonadPlus = PS.Control_MonadPlus;
    var Control_Plus = PS.Control_Plus;
    var Nothing = (function () {
        function Nothing() {

        };
        Nothing.value = new Nothing();
        return Nothing;
    })();
    var Just = (function () {
        function Just(value0) {
            this.value0 = value0;
        };
        Just.create = function (value0) {
            return new Just(value0);
        };
        return Just;
    })();
    var maybe = function (_142) {
        return function (_143) {
            return function (_144) {
                if (_144 instanceof Nothing) {
                    return _142;
                };
                if (_144 instanceof Just) {
                    return _143(_144.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var isJust = maybe(false)(Prelude["const"](true));
    return {
        Nothing: Nothing, 
        Just: Just, 
        isJust: isJust, 
        maybe: maybe
    };
})();
var PS = PS || {};
PS.Data_Tuple = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    var Control_Lazy = PS.Control_Lazy;
    var Data_Array = PS.Data_Array;
    var Control_Comonad = PS.Control_Comonad;
    var Control_Extend = PS.Control_Extend;
    var Tuple = (function () {
        function Tuple(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        };
        Tuple.create = function (value0) {
            return function (value1) {
                return new Tuple(value0, value1);
            };
        };
        return Tuple;
    })();
    return {
        Tuple: Tuple
    };
})();
var PS = PS || {};
PS.Data_Foldable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    var Control_Apply = PS.Control_Apply;
    var Data_Monoid_First = PS.Data_Monoid_First;
    var Data_Either = PS.Data_Either;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Monoid_Additive = PS.Data_Monoid_Additive;
    var Data_Monoid_Dual = PS.Data_Monoid_Dual;
    var Data_Monoid_Last = PS.Data_Monoid_Last;
    var Data_Monoid_Multiplicative = PS.Data_Monoid_Multiplicative;
    var Data_Tuple = PS.Data_Tuple;
    
  function foldlArray(f) {
    return function(z) {
      return function(xs) {
        var acc = z;
        for (var i = 0, len = xs.length; i < len; ++i) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  }
  ;
    return {
        foldlArray: foldlArray
    };
})();
var PS = PS || {};
PS.Data_Map = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Foldable = PS.Data_Foldable;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Traversable = PS.Data_Traversable;
    var Data_Array = PS.Data_Array;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Monoid = PS.Data_Monoid;
    var Leaf = (function () {
        function Leaf() {

        };
        Leaf.value = new Leaf();
        return Leaf;
    })();
    var Two = (function () {
        function Two(value0, value1, value2, value3) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
        };
        Two.create = function (value0) {
            return function (value1) {
                return function (value2) {
                    return function (value3) {
                        return new Two(value0, value1, value2, value3);
                    };
                };
            };
        };
        return Two;
    })();
    var Three = (function () {
        function Three(value0, value1, value2, value3, value4, value5, value6) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value2;
            this.value3 = value3;
            this.value4 = value4;
            this.value5 = value5;
            this.value6 = value6;
        };
        Three.create = function (value0) {
            return function (value1) {
                return function (value2) {
                    return function (value3) {
                        return function (value4) {
                            return function (value5) {
                                return function (value6) {
                                    return new Three(value0, value1, value2, value3, value4, value5, value6);
                                };
                            };
                        };
                    };
                };
            };
        };
        return Three;
    })();
    var lookup = function (__copy___dict_Ord_428) {
        return function (__copy__529) {
            return function (__copy__530) {
                var __dict_Ord_428 = __copy___dict_Ord_428;
                var _529 = __copy__529;
                var _530 = __copy__530;
                tco: while (true) {
                    if (_530 instanceof Leaf) {
                        return Data_Maybe.Nothing.value;
                    };
                    if (_530 instanceof Two && Prelude["=="](__dict_Ord_428["__superclass_Prelude.Eq_0"]())(_529)(_530.value1)) {
                        return new Data_Maybe.Just(_530.value2);
                    };
                    if (_530 instanceof Two && Prelude["<"](__dict_Ord_428)(_529)(_530.value1)) {
                        var __tco___dict_Ord_428 = __dict_Ord_428;
                        var __tco__529 = _529;
                        var __tco__530 = _530.value0;
                        __dict_Ord_428 = __tco___dict_Ord_428;
                        _529 = __tco__529;
                        _530 = __tco__530;
                        continue tco;
                    };
                    if (_530 instanceof Two) {
                        var __tco___dict_Ord_428 = __dict_Ord_428;
                        var __tco__529 = _529;
                        var __tco__530 = _530.value3;
                        __dict_Ord_428 = __tco___dict_Ord_428;
                        _529 = __tco__529;
                        _530 = __tco__530;
                        continue tco;
                    };
                    if (_530 instanceof Three && Prelude["=="](__dict_Ord_428["__superclass_Prelude.Eq_0"]())(_529)(_530.value1)) {
                        return new Data_Maybe.Just(_530.value2);
                    };
                    if (_530 instanceof Three && Prelude["=="](__dict_Ord_428["__superclass_Prelude.Eq_0"]())(_529)(_530.value4)) {
                        return new Data_Maybe.Just(_530.value5);
                    };
                    if (_530 instanceof Three && Prelude["<"](__dict_Ord_428)(_529)(_530.value1)) {
                        var __tco___dict_Ord_428 = __dict_Ord_428;
                        var __tco__529 = _529;
                        var __tco__530 = _530.value0;
                        __dict_Ord_428 = __tco___dict_Ord_428;
                        _529 = __tco__529;
                        _530 = __tco__530;
                        continue tco;
                    };
                    if (_530 instanceof Three && (Prelude["<"](__dict_Ord_428)(_530.value1)(_529) && Prelude["<="](__dict_Ord_428)(_529)(_530.value4))) {
                        var __tco___dict_Ord_428 = __dict_Ord_428;
                        var __tco__529 = _529;
                        var __tco__530 = _530.value3;
                        __dict_Ord_428 = __tco___dict_Ord_428;
                        _529 = __tco__529;
                        _530 = __tco__530;
                        continue tco;
                    };
                    if (_530 instanceof Three) {
                        var __tco___dict_Ord_428 = __dict_Ord_428;
                        var __tco__529 = _529;
                        var __tco__530 = _530.value6;
                        __dict_Ord_428 = __tco___dict_Ord_428;
                        _529 = __tco__529;
                        _530 = __tco__530;
                        continue tco;
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
    };
    var member = function (__dict_Ord_429) {
        return function (k) {
            return function (m) {
                return Data_Maybe.isJust(lookup(__dict_Ord_429)(k)(m));
            };
        };
    };
    return {
        member: member, 
        lookup: lookup
    };
})();
var PS = PS || {};
PS.Data_Graph = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_ST = PS.Control_Monad_ST;
    var Data_Map = PS.Data_Map;
    var Control_Monad = PS.Control_Monad;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Traversable = PS.Data_Traversable;
    var Data_Foldable = PS.Data_Foldable;
    var Data_Array = PS.Data_Array;
    var $$Math = PS.$$Math;
    var Data_Set = PS.Data_Set;
    
    /**
     *  | An directed edge between vertices labelled with keys of type `k`.
     */
    var Edge = (function () {
        function Edge(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        };
        Edge.create = function (value0) {
            return function (value1) {
                return new Edge(value0, value1);
            };
        };
        return Edge;
    })();
    
    /**
     *  | A graph with vertices of type `v`.
     *  |
     *  | Edges refer to vertices using keys of type `k`.
     */
    var Graph = (function () {
        function Graph(value0, value1) {
            this.value0 = value0;
            this.value1 = value1;
        };
        Graph.create = function (value0) {
            return function (value1) {
                return new Graph(value0, value1);
            };
        };
        return Graph;
    })();
    return {
        Graph: Graph, 
        Edge: Edge
    };
})();
var PS = PS || {};
PS.Signal_Norn = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Map = PS.Data_Map;
    var Data_Foldable = PS.Data_Foldable;
    var Debug_Trace = PS.Debug_Trace;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_Eff_Ref = PS.Control_Monad_Eff_Ref;
    var Control_Monad_ST = PS.Control_Monad_ST;
    var Data_Either = PS.Data_Either;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Function = PS.Data_Function;
    var Data_Graph = PS.Data_Graph;
    
    function emptyNornState(){
        return {};    
    }
;
    var Emitter = (function () {
        function Emitter(value0) {
            this.value0 = value0;
        };
        Emitter.create = function (value0) {
            return new Emitter(value0);
        };
        return Emitter;
    })();
    var Event = (function () {
        function Event(value0, value1, value2) {
            this.value0 = value0;
            this.value1 = value1;
            this.value2 = value2;
        };
        Event.create = function (value0) {
            return function (value1) {
                return function (value2) {
                    return new Event(value0, value1, value2);
                };
            };
        };
        return Event;
    })();
    var Cfg = (function () {
        function Cfg(value0) {
            this.value0 = value0;
        };
        Cfg.create = function (value0) {
            return new Cfg(value0);
        };
        return Cfg;
    })();
    var onlyAfter = function (_714) {
        return function (_715) {
            return new Event(_715.value0, _715.value1, function (_712) {
                var _808 = Data_Map.member(Prelude.ordString)(_714)(_712.value1);
                if (_808) {
                    return _715.value2(new Data_Tuple.Tuple(_712.value0, _712.value1));
                };
                if (!_808) {
                    return Prelude["return"](Control_Monad_Eff.monadEff)(Prelude.unit);
                };
                throw new Error("Failed pattern match");
            });
        };
    };
    var noopEvent = function (name) {
        return function __do() {
            var _52 = emptyNornState();
            return new Event(name, function (_710) {
                return _52;
            }, function (_711) {
                return Prelude["return"](Control_Monad_Eff.monadEff)(Prelude.unit);
            });
        };
    };
    var main = Debug_Trace.trace("Foob");
    var eventName = function (_713) {
        return _713.value0;
    };
    var attachEvent = function (_716) {
        return function (_717) {
            return function (_718) {
                return function (_719) {
                    var vert_name = eventName(_718);
                    var new_verts = Prelude[":"](new Cfg({
                        emitter: _716, 
                        deps: _717, 
                        event: _718
                    }))(_719.value0);
                    var new_edges = Data_Foldable.foldlArray(function (m) {
                        return function (i) {
                            return Prelude[":"](new Data_Graph.Edge(i, vert_name))(m);
                        };
                    })(_719.value1)(_717);
                    return new Data_Graph.Graph(new_verts, new_edges);
                };
            };
        };
    };
    return {
        Cfg: Cfg, 
        Emitter: Emitter, 
        Event: Event, 
        main: main, 
        attachEvent: attachEvent, 
        onlyAfter: onlyAfter, 
        noopEvent: noopEvent, 
        eventName: eventName, 
        emptyNornState: emptyNornState
    };
})();
