(defproject johncowie/mudguard "0.0.16-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/test.check "0.10.0"]]
  :repl-options {:init-ns mudguard.core}
  :profiles {:dev {:source-paths ["src" "examples" "dev" "test"]
                   :dependencies [[leiningen "2.9.5"]]
                   :aliases      {"set-version"       ["run" "-m" "version/set-version"]
                                  "next-snapshot"     ["run" "-m" "version/next-snapshot"]
                                  "current-version"   ["run" "-m" "version/current-version"]}}}

  :repositories {"releases" {:url           "https://repo.clojars.org"
                             :username      :env/deploy_username
                             :password      :env/deploy_token
                             :sign-releases false}}
  )
