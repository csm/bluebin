(ns bluebin.core
  (import [io.netty.util Recycler]
          [clojure.lang ILookup Associative MapEntry Seqable Counted IPersistentCollection Sequential Indexed Reversible IPersistentStack IPersistentVector]
          [java.io Writer]))

(defprotocol Recyclable
  (recycle! [this] "Recycles this object. The current reference will be made invalid,
                    and it will be returned to the pool to be reused."))

(defprotocol RecyclableInit
  (init! [this values] "Initializes this object. This is an internal implementation
                        detail; you should not call this on your own."))

(defmacro defrecyclable
  "Define a recyclable record.

  Instances of the new type act like persistent maps; you can get
  all fields by their keyword, and all operations normally used
  on maps.

  Instances can be recycled; note that this can be a security hole:
  if you create an instance of a recyclable object, you don't know
  if any other thread is still holding a reference to that object,
  and thus can read your data out of it. Nevertheless, recyclable
  objects are intended for performance-critical code that experiences
  a lot of GC thrashing.

  type-name is a symbol for the new type to define.
  fields is a vector of symbols to add as fields to the new type.

  Optionally takes keyword arguments:

  :max-capacity the maximum number of recycled objects to keep per thread
                (default 1024)
  :key-ns a namespace to place on keywords in the type; if omitted, values
          can be accessed with un-namespaced keys."
  [type-name fields & {:keys [max-capacity key-ns] :or {max-capacity 1024}}]
  (let [mk-sym (symbol (str "mk-" type-name))
        map-sym (symbol (str "map->" type-name))
        recycler-sym (gensym (str type-name "-recycler-"))
        ->keyword (fn [f] (if (some? key-ns)
                            (keyword key-ns (name f))
                            (keyword (name f))))
        ksym (gensym "k__auto__")
        vsym (gensym "v__auto__")]
    `(do
       (declare ^:private ~mk-sym)
       (declare ~(symbol (str '-> type-name)))
       (declare ~map-sym)

       (def ^:private ~recycler-sym
         (proxy [Recycler] [~max-capacity]
           (newObject [h#]
             (~mk-sym h#))))

       (deftype ~type-name [~@fields handle# active#]
         ILookup
         (valAt [_# ~ksym nf#]
           (dosync
             (if (deref active#)
               (cond
                 ~@(mapcat
                     (fn [f] [`(= ~ksym ~(->keyword f)) `(deref ~f)])
                     fields)
                 :else nf#)
               (throw (IllegalStateException. "recycled object")))))

         (valAt [t# k#]
           (.valAt t# k# nil))

         Associative
         (containsKey [_# ~ksym]
           (dosync
             (if (deref active#)
               (or
                 ~@(map
                     (fn [f] `(= ~ksym ~(->keyword f)))
                     fields))
               (throw (IllegalStateException. "recycled object")))))

         (entryAt [t# k#]
           (dosync
             (if (deref active#)
               (when-let [v# (get t# k#)]
                 (MapEntry. k# v#))
               (throw (IllegalStateException. "recycled object")))))

         (assoc [t# ~ksym ~vsym]
           (dosync
             (if (deref active#)
               (if (.containsKey t# ~ksym)
                 (~(symbol (str '-> type-name))
                   ~@(map
                       (fn [f]
                         `(if (= ~ksym ~(->keyword f)) ~vsym (deref ~f)))
                       fields))
                 (assoc (hash-map ~@(mapcat (fn [f] `[~(->keyword f) (deref ~f)]) fields)) ~ksym ~vsym))
               (throw (IllegalStateException. "recycled object")))))

         Seqable
         (seq [t#]
           (dosync
             (if (deref active#)
               (->> nil
                    ~@(map (fn [f] `(cons (MapEntry. ~(->keyword f) (deref ~f)))) fields))
               (throw (IllegalStateException. "recycled object")))))

         Counted
         (count [_#]
           (if (deref active#)
             ~(count fields)
             (throw (IllegalStateException. "recycled object"))))

         IPersistentCollection
         (cons [t# v#]
           (dosync
             (if (deref active#)
               (let [[k# v#] v#]
                 (assoc t# k# v#))
               (throw (IllegalStateException. "recycled object")))))

         (equiv [t# o#]
           (dosync
             (if (deref active#)
               (or (identical? t# o#)
                   (= (seq t#) (seq o#))))))

         RecyclableInit
         (init! [_# values#]
           (let [[~@(map (fn [f] (symbol (str "in_" f))) fields)] values#]
             (dosync
               (if (false? (deref active#))
                 (do
                   (ref-set active# true)
                   ~@(map (fn [f] `(ref-set ~f ~(symbol (str "in_" f)))) fields))
                 (throw (IllegalStateException. "object is in use"))))))

         Recyclable
         (recycle! [t#]
           (let [recycle# (ref false)
                 ~@(mapcat
                     (fn [f] [(symbol (str "__saved_" f)) `(volatile! nil)])
                     fields)]
             (dosync
               (when (true? (deref active#))
                 (ref-set active# false)
                 (ref-set recycle# true)
                 ~@(mapcat (fn [f] [`(when (satisfies? Recyclable (deref ~f))
                                       (vreset! ~(symbol (str "__saved_" f)) (deref ~f)))
                                    `(ref-set ~f nil)])
                           fields)))
             (when (deref recycle#)
               ~@(map
                   (fn [f]
                     `(when-let [v# (deref ~(symbol (str "__saved_" f)))]
                        (recycle! v#)))
                   fields)
               (.recycle ~recycler-sym t# handle#)))))

       (defmethod print-method ~type-name
         [~'this ^Writer ~'writer]
         (.write ~'writer ~(str \# *ns* \. type-name \{))
         ~@(mapcat
             (fn [f]
               [`(.write ~'writer ~(str (->keyword f)))
                `(.write ~'writer " ")
                `(.write ~'writer (pr-str (get ~'this ~(->keyword f))))
                `(.write ~'writer " ")])
             fields)
         (.write ~'writer "}"))

       (defn- ~mk-sym
         [handle#]
         (~(symbol (str type-name \.)) ~@(repeat (count fields) `(ref nil)) handle# (ref false)))

       (defn ~(symbol (str '-> type-name))
         [~@fields]
         (let [v# (.get ~recycler-sym)]
           (init! v# [~@fields])
           v#))

       (defn ~map-sym
         [m#] ()
         (let [kws# [~@(map ->keyword fields)]]
           (apply ~(symbol (str '-> type-name)) (map (fn [k#] (get m# k#)) kws#)))))))

(declare ^:private mk-RecyclableTinyVector)
(def ^:private RecyclableTinyVector-recycler
  (proxy [Recycler] [8192]
    (newObject [handle]
      (mk-RecyclableTinyVector handle))))

(deftype RecyclableTinyVector [num-items elements handle in-use]
  "A recyclable, \"tiny\" vector.

  These vectors have up to eight items, but can be recycled to
  re-use the containing instance. "
  IPersistentVector
  (length [_]
    (dosync
      (if (deref in-use)
        (deref num-items)
        (throw (IllegalStateException. "recycled object")))))

  (assocN [_ i v]
    (dosync
      (if (deref in-use)
        (let [ret (vec (map deref (take (deref num-items) elements)))]
          (assoc ret i v))
        (throw (IllegalStateException. "recycled object")))))

  (cons [_ o]
    (dosync
      (if (deref in-use)
        (cons (vec (map deref (take (deref num-items) elements))) o)
        (throw (IllegalStateException. "recycled object")))))

  Associative
  (containsKey [_ k]
    (dosync
      (if (deref in-use)
        (and (integer? k) (<= 0 k (deref num-items))))
      (throw (IllegalStateException. "recycled object"))))

  (assoc [this k v]
    (.assocN this k v))

  IPersistentCollection
  (empty [this]
    (when (zero? (.length this))
      this))

  Seqable
  (seq [_]
    (dosync
      (if (deref in-use)
        (seq (map deref (take (deref num-items) elements)))
        (throw (IllegalStateException. "recycled object")))))

  ILookup
  (valAt [_ k not-found]
    (dosync
      (if (deref in-use)
        (if (and (integer? k) (<= 0 k (deref num-items)))
          (deref (nth elements k))
          not-found)
        (throw (IllegalStateException. "recycled object")))))

  (valAt [this k] (.valAt this k nil))

  IPersistentStack
  (peek [this] (get this 0))

  (pop [this]
    (drop 1 (seq this)))

  Reversible
  (rseq [this] (reverse (seq this)))

  Indexed
  (nth [this i not-found] (.valAt this i not-found))

  (nth [this i] (.valAt this i nil))

  Counted
  (count [this] (.length this))

  Sequential

  Recyclable
  (recycle! [this]
    (let [recycle? (ref false)
          recycle0 (ref nil)
          recycle1 (ref nil)
          recycle2 (ref nil)
          recycle3 (ref nil)
          recycle4 (ref nil)
          recycle5 (ref nil)
          recycle6 (ref nil)
          recycle7 (ref nil)]
      (dosync
        (if (deref in-use)
          (do
            (when (and (> (deref num-items) 0)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle0 (deref (aget elements 0))))
            (when (and (> (deref num-items) 1)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle1 (deref (aget elements 1))))
            (when (and (> (deref num-items) 2)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle2 (deref (aget elements 2))))
            (when (and (> (deref num-items) 3)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle3 (deref (aget elements 3))))
            (when (and (> (deref num-items) 4)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle4 (deref (aget elements 4))))
            (when (and (> (deref num-items) 5)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle5 (deref (aget elements 5))))
            (when (and (> (deref num-items) 6)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle6 (deref (aget elements 6))))
            (when (and (> (deref num-items) 7)
                       (satisfies? Recyclable (deref (aget elements 0))))
              (ref-set recycle7 (deref (aget elements 7))))
            (ref-set (aget elements 0) nil)
            (ref-set (aget elements 1) nil)
            (ref-set (aget elements 2) nil)
            (ref-set (aget elements 3) nil)
            (ref-set (aget elements 4) nil)
            (ref-set (aget elements 5) nil)
            (ref-set (aget elements 6) nil)
            (ref-set (aget elements 7) nil)
            (ref-set num-items 0)
            (ref-set in-use false)
            (ref-set recycle? true))))
      (when recycle?
        (when (some? (deref recycle0))
          (recycle! (deref recycle0))))
      (when recycle?
        (when (some? (deref recycle1))
          (recycle! (deref recycle1))))
      (when recycle?
        (when (some? (deref recycle2))
          (recycle! (deref recycle2))))
      (when recycle?
        (when (some? (deref recycle3))
          (recycle! (deref recycle3))))
      (when recycle?
        (when (some? (deref recycle4))
          (recycle! (deref recycle4))))
      (when recycle?
        (when (some? (deref recycle5))
          (recycle! (deref recycle5))))
      (when recycle?
        (when (some? (deref recycle6))
          (recycle! (deref recycle6))))
      (when recycle?
        (when (some? (deref recycle7))
          (recycle! (deref recycle7)))))))

(defmacro gen->rec-vec
  []
  (let [syms ['a 'b 'c 'd 'e 'f 'g 'h]
        v (gensym "v")]
    `(defn ->rec-vec
       ~@(map (fn [i]
                `([~@(take i syms)]
                   (let [~v (.get RecyclableTinyVector-recycler)]
                     (dosync
                       (if (false? (deref (.in_use ~v)))
                         (do
                           (ref-set (.num_items ~v) ~i)
                           ~@(map-indexed (fn [j s] `(ref-set (nth (.elements ~v) ~j) ~s))
                                          (take i syms))
                           (ref-set (.in_use ~v) true)
                           ~v)
                         (throw (IllegalStateException. "object is in use")))))))
              (range 9))
       ([~@syms & more#]
         (vec (concat [~@syms] more#))))))

(gen->rec-vec)

(defn- mk-RecyclableTinyVector
  [handle]
  (RecyclableTinyVector. (ref 0) (repeatedly 8 #(ref nil)) handle (ref false)))