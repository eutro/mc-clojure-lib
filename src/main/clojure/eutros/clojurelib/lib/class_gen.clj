(ns eutros.clojurelib.lib.class-gen
  (:import clojure.lang.DynamicClassLoader
           (java.io FileOutputStream
                    DataOutputStream
                    File)
           (org.objectweb.asm ClassWriter Opcodes Type)
           (org.objectweb.asm.tree ClassNode FieldNode MethodNode VarInsnNode FieldInsnNode MethodInsnNode InsnNode)
           (clojure.lang IFn Symbol)
           (org.objectweb.asm.commons GeneratorAdapter))
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

(defn- desc->field [desc]
  (-> desc
      (.replace "." "_DOT_")
      (.replace ";" "_SEMICOLON_")
      (.replace "(" "_OPEN_PAREN_")
      (.replace ")" "_CLOSE_PAREN_")
      (.replace "[" "_OPEN_BRACKET_")
      (.replace "/" "_SLASH_")))

(defmacro defclass
  [^Symbol top-name & forms]
  (let [cname (str (namespace-munge *ns*) "." top-name)
        int-cname (.replace cname \. \/)

        obj-type (Type/getType ^Class Object)
        ifn-desc (Type/getDescriptor IFn)

        node (ClassNode.)

        method-prefix "method$"]
    (.visit node
            Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER)
            int-cname
            nil
            (.getInternalName obj-type)
            (make-array String 0))

    ;; TODO annotations

    (.visitSource node *file* *file*)

    (doseq [form forms]
      (assert (keyword? (first form)) "Not a keyword!")
      (case (first form)
        :extends (let [super-class (eval (second form))
                       ;; TODO generics
                       ]
                   (set! (.-superName node) (Type/getInternalName super-class)))

        :implements (doseq [interface (next form)]
                      (let [interface-class (eval interface)]
                        (.add (.-interfaces node) (Type/getInternalName interface-class))))

        :field (.add (.-fields node)
                     (let [field-symbol (second form)
                           metadata (meta field-symbol)]
                       (FieldNode. (+ Opcodes/ACC_PUBLIC
                                      (if (get metadata :static) Opcodes/ACC_STATIC 0)
                                      (if (get metadata :final) Opcodes/ACC_FINAL 0))
                                   (name field-symbol)
                                   (Type/getDescriptor (get-type-hint field-symbol))
                                   nil                      ;; TODO generics
                                   (when (> (count form) 2)
                                     (eval (last form))))))

        :method (let [method-symbol (second form)
                      metadata (meta method-symbol)
                      ret-class (tag-class (get (meta method-symbol) :tag 'void))
                      params (first (nnext form))
                      p-classes (map get-type-hint params)
                      static (get metadata :static)

                      method-node (MethodNode. (+ (cond (get metadata :private) Opcodes/ACC_PRIVATE
                                                        (get metadata :protected) Opcodes/ACC_PRIVATE
                                                        :else Opcodes/ACC_PUBLIC)
                                                  (if static Opcodes/ACC_STATIC 0)
                                                  (if (get metadata :final) Opcodes/ACC_FINAL 0))
                                               (name method-symbol)
                                               (Type/getMethodDescriptor (Type/getType ^Class ret-class)
                                                                         (into-array Type (map #(-> ^Class (get-type-hint %)
                                                                                                    (Type/getType)) params)))
                                               nil          ;; TODO generics
                                               (make-array String 0))

                      field-name (str method-prefix
                                      (.name method-node)
                                      (desc->field (.desc method-node)))

                      ga (GeneratorAdapter. method-node
                                            (.-access method-node)
                                            (.-name method-node)
                                            (.-desc method-node))
                      instructions (.-instructions method-node)]

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
                    (.loadArg ga i)
                    ;; (Compiler$HostExpr/emitBoxReturn nil ga (nth p-classes i)) TODO boxing
                    )

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
                    (.unbox (Type/getType ^Class ret-class))
                    (.returnValue))

                  (.add (.-methods node)
                        method-node))

        :constructor (let [params (second form)
                           p-types (map (fn [param]
                                          (-> ^Class (get-type-hint param)
                                              (Type/getType)))
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
                             method-node))

        ))

    (load-tree node)

    `(do
       (import ~(symbol cname))

       ~@(->> (map #(case (first %)
                      :method (let [params (first (nnext %))
                                    ret-class (tag-class (get (meta (second %))
                                                              :tag 'void))
                                    p-types (map (fn [param]
                                                   (-> ^Class (get-type-hint param)
                                                       (Type/getType)))
                                                 params)]
                                `(set! (. ~top-name
                                          ~(symbol (str method-prefix
                                                        (name (second %))
                                                        (desc->field
                                                          (Type/getMethodDescriptor
                                                            (Type/getType ^Class ret-class)
                                                            (into-array Type p-types))))))
                                       ~(cons 'fn
                                              (cons
                                                (let [p-seq (map fn-hint-safe params)]
                                                  (if (get (meta (second %))
                                                           :static)
                                                    (apply vector p-seq)
                                                    (apply vector
                                                           (with-meta 'this {:tag top-name})
                                                           p-seq)))
                                                (next (nnext %))))))
                      nil)
                   forms)
              (filter identity))

       ~(symbol cname))))
