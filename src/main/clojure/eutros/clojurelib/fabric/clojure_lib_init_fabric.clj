(ns eutros.clojurelib.fabric.clojure-lib-init-fabric
  (:require eutros.clojurelib.lib.core))

(when-not *compile-files*
  (load "/eutros/clojurelib/fabric/fabric_core"))

(gen-class
  :name eutros.clojurelib.fabric.CljLibInitFabric
  :implements [net.fabricmc.api.ModInitializer]
  :main false)

(defn -onInitialize [_]
  ; NO-OP
  )
