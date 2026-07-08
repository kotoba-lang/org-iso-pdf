(ns pdf.core
  "PDF COS object parser (ISO 32000). PDF is not a linear struct grammar, so
   this is a hand-written recursive descent reader over the byte stream
   (dict/array/string/name/number/ref/stream). Stream filters route
   FlateDecode to org-ietf-deflate; other filters are opaque passthrough.
   Extracted from kotoba-lang/kasane (kasane.cos, ADR-2606272100) as
   `org-iso-pdf`.

   R0 collects top-level `N G obj … endobj` objects by scan (robust against
   classic or rebuilt xref), resolves the page tree, decodes content streams
   and extracts shown text. Limitations: objects packed inside /ObjStm object
   streams and DCTDecode/JPX image pixels are not decoded (opaque — DCTDecode
   bytes can be fed to org-iso-jpeg's decoder, kept decoupled here)."
  (:require [clojure.string :as str]
            [deflate.core :as deflate]))

;; ---- character classes ---------------------------------------------------
(defn- code [c] #?(:clj (int c) :cljs (.charCodeAt c 0)))
(defn- digit? [c] (and c (<= (code \0) (code c) (code \9))))
(defn- oct?   [c] (and c (<= (code \0) (code c) (code \7))))
(defn- alpha? [c] (and c (let [i (code c)] (or (<= (code \a) i (code \z)) (<= (code \A) i (code \Z))))))
(def ^:private ws-set    #{\space \tab \newline \return \formfeed (char 0)})
(def ^:private delim-set #{\( \) \< \> \[ \] \{ \} \/ \%})
(defn- ws?    [c] (contains? ws-set c))
(defn- delim? [c] (contains? delim-set c))

;; ---- reader state (index atom over a Latin-1 string + parallel bytes) -----
(defn- peek-c [st] (let [i @(:i st)] (when (< i (:len st)) (nth (:s st) i))))
(defn- at     [st k] (let [i @(:i st)] (when (< (+ i k) (:len st)) (nth (:s st) (+ i k)))))
(defn- next-c [st] (let [c (peek-c st)] (swap! (:i st) inc) c))
(defn- looking-at? [st ^String word]
  (let [i @(:i st)] (and (<= (+ i (count word)) (:len st))
                         (= word (subs (:s st) i (+ i (count word)))))))

(defn- skip-ws [st]
  (loop []
    (let [c (peek-c st)]
      (cond
        (ws? c)   (do (next-c st) (recur))
        (= c \%)  (do (loop [] (let [d (peek-c st)] (when (and d (not= d \newline) (not= d \return)) (next-c st) (recur)))) (recur))
        :else nil))))

(defn- parse-num [t]
  (let [t (if (str/starts-with? t "+") (subs t 1) t)
        t (cond (str/starts-with? t ".")  (str "0" t)
                (str/starts-with? t "-.") (str "-0" (subs t 1))
                :else t)
        t (if (str/ends-with? t ".") (str t "0") t)]
    (if (str/includes? t ".")
      #?(:clj (Double/parseDouble t) :cljs (js/parseFloat t))
      #?(:clj (Long/parseLong t)     :cljs (js/parseInt t 10)))))

(declare parse-value parse-dict)

(defn- parse-name [st]
  (next-c st)                                                    ; /
  (loop [acc []]
    (let [c (peek-c st)]
      (if (or (nil? c) (ws? c) (delim? c))
        (keyword (apply str acc))
        (do (next-c st) (recur (conj acc c)))))))

(defn- parse-literal-string [st]
  (next-c st)                                                    ; (
  (loop [depth 1 acc []]
    (let [c (next-c st)]
      (cond
        (nil? c) (apply str acc)
        (= c \\) (let [e (next-c st)]
                   (cond
                     (= e \n) (recur depth (conj acc \newline))
                     (= e \r) (recur depth (conj acc \return))
                     (= e \t) (recur depth (conj acc \tab))
                     (= e \b) (recur depth (conj acc \backspace))
                     (= e \f) (recur depth (conj acc \formfeed))
                     (= e \() (recur depth (conj acc \())
                     (= e \)) (recur depth (conj acc \)))
                     (= e \\) (recur depth (conj acc \\))
                     (or (= e \newline) (= e \return)) (recur depth acc) ; line continuation
                     (oct? e) (let [d2 (when (oct? (peek-c st)) (next-c st))
                                    d3 (when (and d2 (oct? (peek-c st))) (next-c st))
                                    n  (reduce (fn [a d] (+ (* a 8) (- (code d) (code \0)))) 0 (remove nil? [e d2 d3]))]
                                (recur depth (conj acc (char (bit-and n 0xff)))))
                     :else (recur depth (conj acc e))))
        (= c \() (recur (inc depth) (conj acc \())
        (= c \)) (if (= depth 1) (apply str acc) (recur (dec depth) (conj acc \))))
        :else (recur depth (conj acc c))))))

(defn- parse-hex-string [st]
  (next-c st)                                                    ; <
  (loop [hx []]
    (let [c (next-c st)]
      (cond
        (or (nil? c) (= c \>)) (let [hx (if (odd? (count hx)) (conj hx \0) hx)]
                                 (apply str (map (fn [[a b]] (char (#?(:clj Long/parseLong :cljs js/parseInt) (str a b) 16)))
                                                 (partition 2 hx))))
        (ws? c) (recur hx)
        :else   (recur (conj hx c))))))

(defn- parse-array [st]
  (next-c st)                                                    ; [
  (loop [acc []]
    (skip-ws st)
    (if (or (nil? (peek-c st)) (= (peek-c st) \]))
      (do (next-c st) acc)
      (recur (conj acc (parse-value st))))))

(defn- parse-keyword [st]
  (loop [acc []]
    (let [c (peek-c st)]
      (if (and c (alpha? c))
        (do (next-c st) (recur (conj acc c)))
        (let [w (apply str acc)]
          (case w "true" true "false" false "null" nil (keyword w)))))))

(defn- parse-number-or-ref [st]
  (let [t (loop [acc []]
            (let [c (peek-c st)]
              (if (and c (or (digit? c) (#{\+ \- \.} c)))
                (do (next-c st) (recur (conj acc c)))
                (apply str acc))))
        n1 (parse-num t)]
    (if (integer? n1)
      (let [save @(:i st)]
        (skip-ws st)
        (if (digit? (peek-c st))
          (let [t2 (loop [acc []] (let [c (peek-c st)] (if (digit? c) (do (next-c st) (recur (conj acc c))) (apply str acc))))
                save2 @(:i st)]
            (skip-ws st)
            (if (and (= (peek-c st) \R)
                     (let [a (at st 1)] (or (nil? a) (ws? a) (delim? a))))
              (do (next-c st) {::ref [n1 (parse-num t2)]})
              (do (reset! (:i st) save) n1)))
          (do (reset! (:i st) save) n1)))
      n1)))

(defn- parse-stream [st dict]
  (swap! (:i st) + 6)                                            ; consume "stream"
  (when (= (peek-c st) \return) (next-c st))
  (when (= (peek-c st) \newline) (next-c st))
  (let [start @(:i st)
        len   (get dict :Length)
        endkw (str/index-of (:s st) "endstream" start)
        data-end (if (integer? len)
                   (+ start len)
                   (let [e endkw]                               ; trim trailing EOL before endstream
                     (cond (= (nth (:s st) (dec e)) \newline) (if (= (nth (:s st) (- e 2)) \return) (- e 2) (dec e))
                           (= (nth (:s st) (dec e)) \return)  (dec e)
                           :else e)))
        raw (subvec (:bv st) start data-end)]
    (reset! (:i st) (+ endkw 9))                                ; past "endstream"
    {::stream true :dict dict :raw raw}))

(defn- parse-dict [st]
  (next-c st) (next-c st)                                        ; <<
  (let [m (loop [m {}]
            (skip-ws st)
            (cond
              (nil? (peek-c st)) m                              ; EOF guard
              (and (= (peek-c st) \>) (= (at st 1) \>)) (do (next-c st) (next-c st) m)
              (not= (peek-c st) \/) (do (next-c st) (recur m))  ; resync to next /name
              :else (let [k (parse-name st) v (parse-value st)]
                      (recur (assoc m k v)))))]
    (skip-ws st)
    (if (looking-at? st "stream") (parse-stream st m) m)))

(defn- parse-value [st]
  (skip-ws st)
  (let [c (peek-c st)]
    (cond
      (nil? c) ::eof
      (= c \<) (if (= (at st 1) \<) (parse-dict st) (parse-hex-string st))
      (= c \() (parse-literal-string st)
      (= c \[) (parse-array st)
      (= c \/) (parse-name st)
      (or (digit? c) (#{\+ \- \.} c)) (parse-number-or-ref st)
      (alpha? c) (parse-keyword st)
      :else (do (next-c st) (parse-value st)))))

;; ---- top-level: scan objects, find trailer/root --------------------------
(defn- ws-back [s i]   (loop [j i] (if (and (>= j 0) (ws? (nth s j))) (recur (dec j)) j)))
(defn- int-back [s i]  (loop [j i] (if (and (>= j 0) (digit? (nth s j))) (recur (dec j))
                                       (let [start (inc j)]
                                         (when (<= start i) [(parse-num (subs s start (inc i))) start])))))

(defn resolve-ref
  "Resolve an indirect reference (one or more hops) against the object table."
  [objs v]
  (if (and (map? v) (contains? v ::ref))
    (recur objs (get objs (::ref v)))
    v))

(defn- find-trailer [s bv objs]
  (if-let [k (str/last-index-of s "trailer")]
    (parse-value {:s s :bv bv :i (atom (+ k 7)) :len (count s)})
    (if-let [cat (some (fn [[ref v]] (when (and (map? v) (= (:Type v) :Catalog)) ref)) objs)]
      {:Root {::ref cat}} {})))

(defn parse
  "Parse PDF `data` (seq of unsigned bytes) into {:objects :trailer :root}."
  [data]
  (let [bv (vec data)
        s  (apply str (map char bv))
        n  (count s)
        objs (loop [from 0 acc {}]
               (if-let [k (str/index-of s "obj" from)]
                 (let [after (if (< (+ k 3) n) (nth s (+ k 3)) \space)]
                   (if-not (or (ws? after) (delim? after) (= after \<) (= after \[))
                     (recur (+ k 3) acc)
                     (let [g  (int-back s (ws-back s (dec k)))
                           o  (when g (int-back s (ws-back s (dec (second g)))))]
                       (if (and g o)
                         (let [st {:s s :bv bv :i (atom (+ k 3)) :len n}
                               v  (parse-value st)]
                           (recur @(:i st) (assoc acc [(first o) (first g)] v)))
                         (recur (+ k 3) acc)))))
                 acc))
        trailer (find-trailer s bv objs)]
    {:objects objs :trailer trailer :root (resolve-ref objs (:Root trailer))}))

;; ---- page tree -----------------------------------------------------------
(defn- collect-pages [objs node inherited]
  (let [node (resolve-ref objs node)
        inh  (merge inherited (select-keys node [:MediaBox :Resources]))]
    (cond
      (= (:Type node) :Page)  [(merge inh node)]
      (or (= (:Type node) :Pages) (:Kids node))
      (vec (mapcat #(collect-pages objs % inh) (resolve-ref objs (:Kids node))))
      :else [(merge inh node)])))

(defn pages
  "Return the leaf page dicts (with inherited MediaBox/Resources merged)."
  [parsed]
  (let [objs (:objects parsed)]
    (collect-pages objs (resolve-ref objs (:Pages (:root parsed))) {})))

;; ---- stream decode + text extraction -------------------------------------
(defn decode-stream
  "Decode a stream object's bytes by applying its /Filter chain. Unknown
   filters (e.g. DCTDecode) pass through opaquely."
  [objs stream]
  (let [f  (resolve-ref objs (:Filter (:dict stream)))
        fs (cond (nil? f) [] (keyword? f) [f] (vector? f) f :else [])]
    (reduce (fn [b filt] (case filt :FlateDecode (deflate/inflate b) b))
            (:raw stream) fs)))

(defn page-content-str [objs page]
  (let [c     (:Contents page)
        items (cond (vector? c) c (nil? c) [] :else [c])
        streams (keep #(let [x (resolve-ref objs %)] (when (::stream x) x)) items)]
    (apply str (map char (vec (mapcat #(decode-stream objs %) streams))))))

(defn extract-text
  "Collect strings shown between BT/ET in a content stream (R0 heuristic)."
  [content]
  (let [st {:s content :bv nil :i (atom 0) :len (count content)}]
    (loop [in? false runs []]
      (let [c (peek-c st)]
        (cond
          (nil? c) runs
          (= c \()                       (let [s (parse-literal-string st)] (recur in? (if in? (conj runs s) runs)))
          (and (= c \<) (not= (at st 1) \<)) (let [s (parse-hex-string st)] (recur in? (if in? (conj runs s) runs)))
          (and (= c \B) (looking-at? st "BT")) (do (swap! (:i st) + 2) (recur true runs))
          (and (= c \E) (looking-at? st "ET")) (do (swap! (:i st) + 2) (recur false runs))
          :else (do (next-c st) (recur in? runs)))))))

(defn page-text [objs page]
  (extract-text (page-content-str objs page)))

;; ---- image XObjects ------------------------------------------------------
(defn page-images
  "Image XObjects referenced from a page's /Resources /XObject. Returns
   {:name :w :h :bpc :colorspace :filter :stream}."
  [objs page]
  (let [res  (resolve-ref objs (:Resources page))
        xobj (resolve-ref objs (:XObject res))]
    (when (map? xobj)
      (keep (fn [[k v]]
              (let [s (resolve-ref objs v)]
                (when (and (map? s) (::stream s) (= (:Subtype (:dict s)) :Image))
                  {:name       (name k)
                   :w          (:Width (:dict s))
                   :h          (:Height (:dict s))
                   :bpc        (:BitsPerComponent (:dict s))
                   :colorspace (resolve-ref objs (:ColorSpace (:dict s)))
                   :filter     (resolve-ref objs (:Filter (:dict s)))
                   :stream     s})))
            xobj))))

(defn decode-image
  "Decode an image XObject (from page-images). FlateDecode → raw samples;
   DCTDecode → the embedded JPEG bytes (feed to org-iso-jpeg's decode-rgb,
   kept decoupled); other filters → opaque. JPXDecode/CCITT are opaque (R0)."
  [_objs img]
  (let [raw (:raw (:stream img))
        f   (:filter img)
        fs  (cond (nil? f) [] (keyword? f) [f] (vector? f) f :else [])]
    (cond
      (some #{:DCTDecode} fs)   {:fmt :jpeg :jpeg raw :w (:w img) :h (:h img)}
      (some #{:FlateDecode} fs) {:fmt :raw  :samples (deflate/inflate raw) :w (:w img) :h (:h img) :bpc (:bpc img)}
      :else                     {:fmt :opaque :opaque raw :w (:w img) :h (:h img)})))
