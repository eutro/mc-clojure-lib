(ns eutros.clojurelib.lib.core
  (:import (org.apache.logging.log4j LogManager Logger)))
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

(alter-var-root
  #'*data-readers*
  #(assoc %
     'obf/obf 'eutros.clojurelib.lib.core/mapped
     'rip/rip 'eutros.clojurelib.lib.rip/rip
     'rip/client 'eutros.clojurelib.lib.rip/rip-client
     'rip/forge 'eutros.clojurelib.lib.rip/rip-forge
     'rip/fabric 'eutros.clojurelib.lib.rip/rip-fabric))

(declare mapped)

(def ^:dynamic *class-dump-location* (System/getProperty "eutros.clojurelib.dump_classes"))
