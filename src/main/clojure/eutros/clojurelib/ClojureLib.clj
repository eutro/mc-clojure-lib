(ns eutros.clojurelib.ClojureLib
  (:import (clojure.asm ClassWriter Opcodes Type)
           (clojure.asm.commons GeneratorAdapter Method)
           net.minecraftforge.fml.common.Mod))

(when *compile-files*
  (let [class-name (-> (str *ns*)
                       (.replace \. \/))]
    (doto (ClassWriter. ClassWriter/COMPUTE_MAXS)

          ;; start class
          (.visit Opcodes/V1_5
                  (+ Opcodes/ACC_PUBLIC
                     Opcodes/ACC_SUPER)
                  class-name
                  nil
                  (Type/getInternalName Object)
                  (make-array String 0))

          ;; add Mod annotation
          (#(doto
             (.visitAnnotation ^ClassWriter %
                               (Type/getDescriptor Mod)
                               true)

             (.visit "value"
                     "clojurelib")

             (.visitEnd)))

          ;; add constructor
          (#(let [m (Method. "<init>"
                             Type/VOID_TYPE
                             (make-array Type 0))]
             (doto (GeneratorAdapter. Opcodes/ACC_PUBLIC m nil nil %)
                   (.visitCode)
                   ;; invoke super constructor
                   (.loadThis)
                   (.dup)
                   (.invokeConstructor (Type/getType Object) m)
                   (.returnValue)

                   (.endMethod))))

          (.visitEnd)

          ;; save to file
          (#(Compiler/writeClassFile class-name (.toByteArray ^ClassWriter %))))))
