(ns eutros.clojurelib.lib.rip
  (:use eutros.clojurelib.lib.core))

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
