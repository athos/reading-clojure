(require '[clojure.string :as str])
(import '[java.lang.reflect Method Member Modifier])

(defn error [msg & vals]
  (throw (Exception. (str msg vals))))

;; something to do with Markdown
(defn itemize [content]
  (str "- " content))

(defn emph [content]
  (str "_" content "_"))

(defn wikiname [content]
  (str "[[" content "]]"))

;; several utilities
(defn class->file-name [^Class class]
  (str "jvm/"
       (str/replace (.getCanonicalName class) "." "/")
       ".java"))

(defn class->path [^Class class]
  (str (class->file-name class) "/" (.getSimpleName class)))

(defn params-string [method]
  (str "("
       (str/join \, (for [param (.getParameterTypes method)] (.getSimpleName param)))
       ")"))

(defn static? [^Member m]
  (Modifier/isStatic (.getModifiers m)))

(defn divide [pred coll]
  (let [{t true, f false} (group-by pred coll)]
    [t f]))

;; for debug
(defmacro ?= [x]
  `(do (println '~x ":" ~x)
       ~x))

(defn make-section [title extractor items]
  (when-not (empty? items)
    [[title (sort (for [item items] (itemize (wikiname (extractor item)))))]]))

(defn make-class-entry [^Class class]
  (let [interface? (.isInterface class)
        path (class->path class)
        member->path (fn [^Member m] (str path "/" (str/replace (.getName m) #".*\." "")))
        method->path #(str (member->path %) (params-string %))
        inner-class->path #(str (class->path (.getEnclosingClass %)) "/" (.getSimpleName %))
        in-same-package? #(= (.getPackage class) (.getPackage %))
        superclass (.getSuperclass class)
        superclass (when (and (not interface?) (in-same-package? superclass))
                     [superclass])
        [inner-interfaces inner-classes] (divide #(.isInterface %) (.getDeclaredClasses class))
        [static-fields instance-fields] (divide static? (.getDeclaredFields class))
        [static-methods instance-methods] (divide static? (.getDeclaredMethods class))]
    [[(if interface? "インタフェース" "クラス")
      [(str (emph (.getCanonicalName class))
            " [" (class->file-name class) "](url goes here)")]]
     ["概要" ["ここに概要を書く"]]
     ["関連"
      (concat
       (make-section "スーパークラス" class->path superclass)
       (make-section (if interface? "スーパーインタフェース" "インタフェース")
                     class->path
                     (filter in-same-package? (.getInterfaces class)))
       (make-section "内部クラス" inner-class->path inner-classes)
       (make-section "内部インタフェース" inner-class->path inner-interfaces)
       (make-section "クラスフィールド" member->path static-fields)
       (make-section "クラスメソッド" method->path static-methods)
       (make-section "コンストラクタ" method->path (.getDeclaredConstructors class))
       (make-section "フィールド" member->path instance-fields)
       (make-section "メソッド" method->path instance-methods))]]))

(defn print-entry [entry]
  (letfn [(print-section [section depth]
            (let [[title contents] section]
              (println (str/join (repeat depth \#)) title)
              (doseq [content contents]
                (if (vector? content)
                  (print-section content (inc depth))
                  (println content)))
              (newline)))]
    (doseq [section entry]
      (print-section section 2))))

(defn main [[command & args]]
  (if-let [command (keyword command)]
    (case command
      :class (let [[classname] args
                   class (resolve (symbol classname))]
               (if (class? class)
                 (print-entry (make-class-entry class))
                 (error "unknown class: " classname))))))

(main *command-line-args*)
