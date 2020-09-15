(ns eutros.clojurelib.lib.type-hints
  (:import clojure.lang.Compiler$HostExpr
           (java.lang.reflect Array)))

(defn- prim-class
  [sym]
  (case (name sym)
    "int" Integer/TYPE
    "long" Long/TYPE
    "float" Float/TYPE
    "double" Double/TYPE
    "char" Character/TYPE
    "short" Short/TYPE
    "byte" Byte/TYPE
    "boolean" Boolean/TYPE
    "void" Void/TYPE
    nil))

(defn- tag-to-class
  [tag]
  (or (when (and (symbol? tag)
                 (nil? (namespace tag)))
        (Compiler$HostExpr/maybeSpecialTag tag))
      (Compiler$HostExpr/maybeClass tag true)
      (throw (IllegalStateException. (str "Unable to resolve classname: " tag)))))

(defn tag-class
  [tag]
  (if (nil? tag)
    Void/TYPE
    (or (if (symbol? tag)
          (prim-class tag))
        (tag-to-class tag))))

(defn ^Class hint-from
  ([metadata] (hint-from metadata Object))
  ([metadata fallback]
   (tag-class (get metadata :tag fallback))))

(defn get-type-hint
  ([imeta] (get-type-hint imeta Object))
  ([imeta fallback]
   (hint-from (meta imeta) fallback)))

(defn fn-hint-safe
  [sym]
  (let [hint ^Class (get-type-hint sym)]
    (if (and (.isPrimitive hint)
             (not= hint Long/TYPE)
             (not= hint Double/TYPE))
      (with-meta sym
                 (assoc (meta sym)
                   :tag nil))
      sym)))
