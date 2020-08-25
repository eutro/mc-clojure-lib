(ns eutros.clojurelib.lib.core
  (:import (org.apache.logging.log4j LogManager Logger))
  (:require eutros.clojurelib.lib.type-hints))
;; To be loaded before the modloader-specific core initializers.

(set! *warn-on-reflection* true)

(def ^String MOD_ID "clojurelib")
(def ^Logger LOGGER (LogManager/getLogger MOD_ID))

(declare IS_CLIENT)
(declare IS_FABRIC)
(declare IS_FORGE)

(declare MAPPINGS)
(comment                                                    ;; one of:
  :obf/mcp                                                  ;; MCP (Forge dev)
  :obf/srg                                                  ;; SRG (Forge prod)
  :obf/yrn                                                  ;; Yarn (Fabric dev)
  :obf/itm                                                  ;; Intermediary (Yarn prod)
  )

(defmacro if-client
  ([then] `(if-client ~then nil))
  ([then else] (if IS_CLIENT then else)))
(defmacro if-fabric
  ([then] `(if-fabric ~then nil))
  ([then else] (if IS_FABRIC then else)))
(defmacro if-forge
  ([then] `(if-forge ~then nil))
  ([then else] (if IS_FORGE then else)))

(defmacro when-client [& forms]
  (if-client forms))
(defmacro when-fabric [& forms]
  (if-fabric forms))
(defmacro when-forge [& forms]
  (if-forge forms))

(declare mapped)

(def ^:private RIPPED
  ^{:doc "Sentinel value for forms that have been ripped."}
  (Object.))

(defn rip [form]
  ^{:doc
    "Data reader, under the name rip/rip.

    Omits any forms that are equal to RIPPED,
    i.e. they have been ripped out by the other
    data readers in this namespace."}
  (let [ripped-forms (filterv (partial identical?
                                       RIPPED)
                              form)]
    (if (list? form)
      (apply list ripped-forms)
      ripped-forms)))

(defn rip-client [form]
  ^{:doc
    "Data reader, under the name rip/client

    Omit the form when not on the client.

    To be used in combination with rip/rip."}
  (if-client
    form
    RIPPED))

(defn rip-forge [form]
  ^{:doc
    "Data reader, under the name rip/forge

    Omit the form when not on Forge.

    To be used in combination with rip/rip."}
  (if-forge
    form
    RIPPED))

(defn rip-fabric [form]
  ^{:doc
    "Data reader, under the name rip/fabric

    Omit the form when not on Fabric.

    To be used in combination with rip/rip."}
  (if-fabric
    form
    RIPPED))

(alter-var-root
  #'*data-readers*
  #(assoc %
     'obf/obf #'eutros.clojurelib.lib.core/mapped
     'rip/rip #'eutros.clojurelib.lib.core/rip
     'rip/client #'eutros.clojurelib.lib.core/rip-client
     'rip/forge #'eutros.clojurelib.lib.core/rip-forge
     'rip/fabric #'eutros.clojurelib.lib.core/rip-fabric
     'hint/array #'eutros.clojurelib.lib.type-hints/array-hint))

(def ^:dynamic *class-dump-location* (System/getProperty "eutros.clojurelib.dump_classes"))
