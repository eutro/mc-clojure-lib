(in-ns 'eutros.clojurelib.lib.core)

(import net.fabricmc.loader.api.FabricLoader
        net.fabricmc.api.EnvType)

(def IS_FORGE false)
(def IS_FABRIC true)
(def IS_CLIENT (identical? (.getEnvironmentType (FabricLoader/getInstance))
                           EnvType/CLIENT))
(def MAPPINGS (if (.isDevelopmentEnvironment (FabricLoader/getInstance))
                :obf/yrn :obf/itm))

(def ^:private mapping-resolver (.getMappingResolver (FabricLoader/getInstance)))

(.debug LOGGER
        (str "Loading on FABRIC\n"
             (if IS_CLIENT "CLIENT" "DEDICATED SERVER") " distribution\n"
             (.toUpperCase (name MAPPINGS)) " mappings"))

(defn mapped [form]
  (let [itm-name (get (meta form)
                      :obf/itm
                      form)]
    (symbol (if (= MAPPINGS :obf/itm)
              itm-name
              (case (first (str itm-name))
                \m (.mapMethodName "intermediary"
                                   (get (meta form) :obf/owner)
                                   (str itm-name)
                                   (get (meta form) :obf/desc))
                \f (.mapFieldName "intermediary"
                                  (get (meta form) :obf/owner)
                                  (str itm-name)
                                  (get (meta form) :obf/desc))
                \c (.mapClassName "intermediary" (str itm-name)))))))