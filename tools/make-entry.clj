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

;; for debug
(defmacro ?= [x]
  `(do (println '~x ":" ~x)
       ~x))

(defn make-section [title extractor items]
  (when-not (empty? items)
    `[[~title
       ~@(sort (for [item items] (itemize (wikiname (extractor item)))))]]))

(defn make-class-entry [^Class class]
  (let [interface? (.isInterface class)
        path (class->path class)
        member->path (fn [^Member m]
                       (str path "/" (str/replace (.getName m) #".*\." "")))
        method->path #(str (member->path %) (params-string %))
        inner-class->path #(str (class->path (.getEnclosingClass %)) "/" (.getSimpleName %))
        in-same-package? #(= (.getPackage class) (.getPackage %))
        superclass (.getSuperclass class)
        superclass (when (and (not interface?) (in-same-package? superclass))
                     [superclass])
        interfaces (filter in-same-package? (.getInterfaces class))
        {inner-interfaces true
         inner-classes false} (group-by #(.isInterface %) (.getDeclaredClasses class))
        fields (.getDeclaredFields class)
        methods (.getDeclaredMethods class)
        constructors (.getDeclaredConstructors class)
        {static-fields true, instance-fields false} (group-by static? fields)
        {static-methods true, instance-methods false} (group-by static? methods)]
    `[[~(if interface? "インタフェース" "クラス")
       ~(str (emph (.getCanonicalName class))
             " [" (class->file-name class) "](url goes here)")]
      ["概要" "ここに概要を書く"]
      ["関連"
       ~@(make-section "スーパークラス" class->path superclass)
       ~@(make-section (if interface? "スーパーインタフェース" "インタフェース")
                       class->path
                       interfaces)
       ~@(make-section "内部クラス" inner-class->path inner-classes)
       ~@(make-section "内部インタフェース" inner-class->path inner-interfaces)       
       ~@(make-section "クラスフィールド" member->path static-fields)
       ~@(make-section "クラスメソッド" method->path static-methods)
       ~@(make-section "コンストラクタ" method->path constructors)
       ~@(make-section "フィールド" member->path instance-fields)
       ~@(make-section "メソッド" method->path instance-methods)]]))

(defn print-entry [entry]
  (letfn [(print-section [section depth]
            (let [[title & contents] section]
              (println (str/join (repeat depth \#)) title)
              (doseq [content contents]
                (if (coll? content)
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
