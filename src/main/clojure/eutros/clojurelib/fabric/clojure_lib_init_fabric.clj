(ns eutros.clojurelib.fabric.clojure-lib-init-fabric
  (:use eutros.clojurelib.lib.class-gen
        eutros.clojurelib.lib.core)
  (:import (org.objectweb.asm.tree ClassNode MethodNode VarInsnNode
                                   InsnNode MethodInsnNode LdcInsnNode
                                   LabelNode LineNumberNode)
           (org.objectweb.asm Opcodes Type Label)
           (clojure.java.api Clojure)
           (clojure.lang IFn)))

(when *compile-files*
  (let [node (ClassNode.)
        obj-type (Type/getType ^Class Object)]
    (.visit node
            Opcodes/V1_5
            (+ Opcodes/ACC_PUBLIC
               Opcodes/ACC_SUPER)
            (-> (str *ns*)
                (.replace \. \/)
                (.replace \- \_))
            nil
            (Type/getInternalName Object)
            (make-array String 0))

    (.visitSource node *file* *file*)

    (.add (.-interfaces node)
          "net.fabricmc.api.ModInitializer")

    (.add (.-methods node)
          (let [method (MethodNode. Opcodes/ACC_PUBLIC
                                    "<init>" "()V"
                                    nil (make-array String 0))
                start-label (LabelNode. (Label.))]
            (doto (.-instructions method)
              (.add start-label)
              (.add (LineNumberNode. 37 start-label))

              (.add (VarInsnNode. Opcodes/ALOAD 0))
              (.add (MethodInsnNode. Opcodes/INVOKESPECIAL
                                     (Type/getInternalName Object)
                                     "<init>" "()V"))

              (.add (InsnNode. Opcodes/RETURN)))
            method))

    (.add (.-methods node)
          (let [method (MethodNode. Opcodes/ACC_PUBLIC
                                    "onInitialize" "()V"
                                    nil (make-array String 0))
                start-label (LabelNode. (Label.))]
            (doto (.-instructions method)
              (.add start-label)
              (.add (LineNumberNode. 54 start-label))

              ;; Load core NS
              (.add (LdcInsnNode. (str (symbol #'require))))
              (.add (MethodInsnNode. Opcodes/INVOKESTATIC
                                     (Type/getInternalName Clojure)
                                     "var" (Type/getMethodDescriptor (Type/getType ^Class IFn)
                                                                     (into-array [obj-type]))))
              (.add (LdcInsnNode. (str (symbol #'symbol))))
              (.add (MethodInsnNode. Opcodes/INVOKESTATIC
                                     (Type/getInternalName Clojure)
                                     "var" (Type/getMethodDescriptor (Type/getType ^Class IFn)
                                                                     (into-array [obj-type]))))
              (.add (LdcInsnNode. "eutros.clojurelib.lib.core"))
              (.add (MethodInsnNode. Opcodes/INVOKEINTERFACE
                                     (Type/getInternalName IFn)
                                     "invoke" (Type/getMethodDescriptor obj-type
                                                                        (into-array [obj-type]))))
              (.add (MethodInsnNode. Opcodes/INVOKEINTERFACE
                                     (Type/getInternalName IFn)
                                     "invoke" (Type/getMethodDescriptor obj-type
                                                                        (into-array [obj-type]))))
              (.add (InsnNode. Opcodes/POP))

              (.add (LdcInsnNode. (str (symbol #'load))))
              (.add (MethodInsnNode. Opcodes/INVOKESTATIC
                                     (Type/getInternalName Clojure)
                                     "var" (Type/getMethodDescriptor (Type/getType ^Class IFn)
                                                                     (into-array [obj-type]))))
              (.add (LdcInsnNode. "/eutros/clojurelib/forge/fabric_core"))
              (.add (MethodInsnNode. Opcodes/INVOKEINTERFACE
                                     (Type/getInternalName IFn)
                                     "invoke" (Type/getMethodDescriptor obj-type
                                                                        (into-array [obj-type]))))
              (.add (InsnNode. Opcodes/POP))

              (.add (InsnNode. Opcodes/RETURN)))
            method))

    (write-tree node)))
