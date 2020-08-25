(ns eutros.clojurelib.lib.class-gen
  (:import clojure.lang.DynamicClassLoader
           (java.io FileOutputStream
                    DataOutputStream
                    File)
           (org.objectweb.asm ClassWriter Opcodes Type AnnotationVisitor)
           (org.objectweb.asm.tree ClassNode FieldNode MethodNode VarInsnNode FieldInsnNode MethodInsnNode InsnNode)
           (clojure.lang IFn Symbol)
           (org.objectweb.asm.commons GeneratorAdapter)
           (java.lang.annotation Annotation Retention RetentionPolicy))
  (:use eutros.clojurelib.lib.core
        eutros.clojurelib.lib.type-hints))

(defn load-bytes
  [^String name ^bytes bytes super interfaces]
  (when *class-dump-location*
    (with-open [fos (FileOutputStream. (File. (File. ^String *class-dump-location*)
                                              name))
                dos (DataOutputStream. fos)]
      (.write dos bytes)))
  (when *compile-files*
    (Compiler/writeClassFile (.replace name \. \/) bytes))
  (.defineClass ^DynamicClassLoader (deref Compiler/LOADER)
                name bytes [super interfaces]))

(defn get-tree-bytes
  [^ClassNode node]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_MAXS)]
    (.accept node cw)
    (.toByteArray cw)))

(defn class-from-internal [name]
  (-> name (.replace \/ \.) (Class/forName)))

(defn write-tree
  [^ClassNode node]
  (Compiler/writeClassFile (.replace (.name node) \. \/)
                           (get-tree-bytes node)))

(defn load-tree
  [^ClassNode node]
  (load-bytes (.replace (.name node) \/ \.)
              (get-tree-bytes node)
              (-> (.superName node) class-from-internal)
              (into-array Class (map class-from-internal (.interfaces node)))))

(defn- ^String munged [obj]
  (Compiler/munge obj))

(defn- extra-munged [obj]
  (-> obj munged
      (.replace ";" "_SEMICOLON_")
      (.replace "(" "_LPAREN_")
      (.replace ")" "_RPAREN_")))

(defn- is-annotation? [c]
  (and (class? c)
       (.isAssignableFrom Annotation c)))

(defn descriptor [^Class c]
  (Type/getDescriptor c))

(defn to-type [^Class c]
  (Type/getType c))

(defn- is-runtime-annotation?
  "Copied from core."
  [^Class c]
  (boolean
    (and (is-annotation? c)
         (when-let [^Retention r
                    (.getAnnotation c Retention)]
           (= (.value r) RetentionPolicy/RUNTIME)))))

(declare process-annotation)
(defn- add-annotation
  "Copied from core."
  [^AnnotationVisitor av name v]
  (cond
    (vector? v) (let [avec (.visitArray av name)]
                  (doseq [vval v]
                    (add-annotation avec "value" vval))
                  (.visitEnd avec))
    (symbol? v) (let [ev (eval v)]
                  (cond
                    (instance? Enum ev)
                    (.visitEnum av name (descriptor (class ev)) (str ev))
                    (class? ev) (.visit av name (to-type ev))
                    :else (throw (IllegalArgumentException.
                                   (str "Unsupported annotation value: " v " of class " (class ev))))))
    (seq? v) (let [[nested nv] v
                   c (resolve nested)
                   nav (.visitAnnotation av name (descriptor c))]
               (process-annotation nav nv)
               (.visitEnd nav))
    :else (.visit av name v)))

(defn- process-annotation
  "Copied from core."
  [av v]
  (if (map? v)
    (doseq [[k v] v]
      (add-annotation av (name k) v))
    (add-annotation av "value" v)))

(defn- add-annotations
  "Copied from core."
  ([visitor m] (add-annotations visitor m nil))
  ([visitor metadata param-no]
   (doseq [[k v] metadata]
     (when (symbol? k)
       (when-let [c (resolve k)]
         (when (is-annotation? c)
           (let [av (binding [*warn-on-reflection* false]   ;; this is known duck/reflective as no common base of ASM Visitors
                      (if param-no
                        (.visitParameterAnnotation visitor param-no (descriptor c)
                                                   (is-runtime-annotation? c))
                        (.visitAnnotation visitor (descriptor c)
                                          (is-runtime-annotation? c))))]
             (process-annotation av v)
             (.visitEnd av))))))))

(defmacro defclass
  [^Symbol top-name & forms]
  (let [class-meta (meta top-name)
        top-name (symbol (munged (str top-name)))
        cname (str (namespace-munge *ns*) "." top-name)
        int-cname (.replace cname \. \/)

        obj-type (to-type Object)
        ifn-desc (descriptor IFn)

        node (ClassNode.)

        method-prefix "method$"]
    (.visit node
            Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER
               (if (get :final class-meta) Opcodes/ACC_FINAL 0))
            int-cname
            nil
            (.getInternalName obj-type)
            (make-array String 0))

    (add-annotations node class-meta)

    (.visitSource node *file* *file*)

    (doseq [form forms]
      (assert (keyword? (first form)) "Not a keyword!")
      (let [form-next (next form)]
        (case (first form)
          :extends (let [super-class (eval (first form-next))
                         ;; TODO generics
                         ]
                     (set! (.-superName node) (Type/getInternalName super-class)))

          :implements (doseq [interface form-next]
                        (let [interface-class (eval interface)]
                          (.add (.-interfaces node) (Type/getInternalName interface-class))))

          :field (.add (.-fields node)
                       (let [field-symbol (first form-next)
                             metadata (merge (meta field-symbol) (meta form))
                             field-name (munged (str field-symbol))]
                         (doto (FieldNode. (+ Opcodes/ACC_PUBLIC
                                              (if (get metadata :static) Opcodes/ACC_STATIC 0)
                                              (if (get metadata :final) Opcodes/ACC_FINAL 0))
                                           field-name
                                           (descriptor (get-type-hint field-symbol))
                                           nil              ;; TODO generics
                                           (when (> (count form) 2)
                                             (eval (last form))))
                           (add-annotations metadata))))

          :method (let [method-symbol (first form-next)
                        metadata (merge (meta method-symbol) (meta form))
                        ret-class (tag-class (get (meta method-symbol) :tag 'void))
                        params (second form-next)
                        p-classes (map get-type-hint params)
                        static (get metadata :static)

                        method-name (munged (name method-symbol))
                        method-node (MethodNode. (+ (cond (get metadata :private) Opcodes/ACC_PRIVATE
                                                          (get metadata :protected) Opcodes/ACC_PRIVATE
                                                          :else Opcodes/ACC_PUBLIC)
                                                    (if static Opcodes/ACC_STATIC 0)
                                                    (if (get metadata :final) Opcodes/ACC_FINAL 0))
                                                 method-name
                                                 (Type/getMethodDescriptor (to-type ret-class)
                                                                           (into-array Type (map #(-> ^Class (get-type-hint %)
                                                                                                      (to-type)) params)))
                                                 nil        ;; TODO generics
                                                 (make-array String 0))

                        field-name (str method-prefix
                                        (.name method-node)
                                        (extra-munged (.desc method-node)))

                        ga (GeneratorAdapter. method-node
                                              (.-access method-node)
                                              (.-name method-node)
                                              (.-desc method-node))
                        instructions (.-instructions method-node)]

                    (dotimes [i (count params)]
                      (add-annotations method-node (meta (nth params i)) i))

                    (doseq [param params]
                      (.visitParameter method-node (name param) 0))

                    (.add (.-fields node)
                          (FieldNode. (+ Opcodes/ACC_PUBLIC
                                         Opcodes/ACC_STATIC)
                                      field-name
                                      ifn-desc
                                      nil
                                      nil))

                    (.add instructions
                          (FieldInsnNode. Opcodes/GETSTATIC
                                          int-cname
                                          field-name
                                          ifn-desc))
                    (when-not static
                      (.add instructions
                            (VarInsnNode. Opcodes/ALOAD 0)))

                    ;; breaks with more than 19 arguments...
                    (dotimes [i (count p-classes)]
                      (let [param-type (to-type (nth p-classes i))]
                        (.loadArg ga i)
                        (.valueOf ga param-type)))

                    (.add instructions
                          (MethodInsnNode. Opcodes/INVOKEINTERFACE
                                           (Type/getInternalName IFn)
                                           "invoke"
                                           (Type/getMethodDescriptor obj-type
                                                                     (into-array Type
                                                                                 (repeat ((if static
                                                                                            identity
                                                                                            inc)
                                                                                          (count p-classes))
                                                                                         obj-type)))))

                    (when (= ret-class Void/TYPE)
                      (.add instructions (InsnNode. Opcodes/POP)))

                    (doto ga
                      (.unbox (to-type ret-class))
                      (.returnValue))

                    (.add (.-methods node)
                          method-node)

                    (add-annotations method-node metadata))

          :constructor (let [params (second form)
                             p-types (map (fn [param]
                                            (-> ^Class (get-type-hint param)
                                                (to-type)))
                                          params)
                             method-desc (Type/getMethodDescriptor Type/VOID_TYPE
                                                                   (into-array Type p-types))
                             method-node (MethodNode. Opcodes/ACC_PUBLIC
                                                      "<init>"
                                                      method-desc
                                                      nil (make-array String 0))]
                         (doto (.-instructions method-node)
                           (.add (VarInsnNode. Opcodes/ALOAD 0))
                           (.add (MethodInsnNode. Opcodes/INVOKESPECIAL
                                                  (.-superName node)
                                                  "<init>"
                                                  method-desc))
                           (.add (InsnNode. Opcodes/RETURN)))

                         (.add (.-methods node)
                               method-node)

                         (add-annotations method-node (meta form)))

          )))

    (load-tree node)

    `(do
       (import ~(symbol cname))

       ~@(->> (map #(case (first %)
                      :method (let [[name-symbol params & body] (next %)
                                    fn-meta (meta name-symbol)
                                    ret-class (tag-class (get fn-meta
                                                              :tag 'void))
                                    p-types (map (fn [param]
                                                   (-> ^Class (get-type-hint param)
                                                       (to-type)))
                                                 params)]
                                `(set! (. ~top-name
                                          ~(symbol (str method-prefix
                                                        (name name-symbol)
                                                        (extra-munged
                                                          (Type/getMethodDescriptor
                                                            (to-type ret-class)
                                                            (into-array Type p-types))))))
                                       ~(cons 'fn
                                              (cons (let [p-seq (map fn-hint-safe params)]
                                                      (if (get fn-meta
                                                               :static)
                                                        (apply vector p-seq)
                                                        (apply vector
                                                               (with-meta 'this {:tag top-name})
                                                               p-seq)))
                                                    body))))
                      nil)
                   forms)
              (filter identity))

       ~(symbol cname))))
