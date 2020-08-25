(ns eutros.clojurelib.lib.type-hints
  (:import clojure.lang.Compiler$HostExpr))

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
    Object
    (or (if (symbol? tag)
          (prim-class tag))
        (tag-to-class tag))))

(defn get-type-hint
  [imeta]
  (tag-class
    (get (meta imeta) :tag)))

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
