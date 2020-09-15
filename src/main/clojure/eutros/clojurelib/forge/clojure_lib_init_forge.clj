(ns eutros.clojurelib.forge.clojure-lib-init-forge
  (:require eutros.clojurelib.lib.core)
  (:import (net.minecraftforge.fml.common Mod)))

(when-not *compile-files*
  (load "/eutros/clojurelib/forge/forge_core"))

(gen-class
  :name ^{Mod "clojurelib"}
  eutros.clojurelib.forge.clojure-lib-init-forge
  :main false)
