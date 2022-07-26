(ns quickthing
  "Helpers for plotting with `thing.geom.viz`"
  (:require [clojure.data.csv     :as csv]
            [clojure.java.io      :as io]
            [thi.ng.geom
             [core                :as geom]
             [matrix              :as matrix]]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.math.core     :as math]
            [thi.ng.ndarray.core  :as ndarray]
            [thi.ng.color.core    :as col]))

(defn
  svg2xml
  "This takes the SVG hiccup,
  - serializes it
  - inserts new line characters
  If the SVG is output as one one long line.. most editors are unhappy"
  [svg]
  (->
    svg
    svg/serialize
    (clojure.string/replace
      #"><"
      ">\n<")))

#_(defn
    set-coords
    "Wrap the `svg-element` with an `<svg>` container.
  `coord-width` and `coord-height` specify the viewport coordinate ranges.
  ie. the local coordinates where the element is located.
  The width/height are left unspecified..
   "
    [svg-element
     [coord-width
      coord-height]]
    (->>
      svg-element
      (svg/svg
        {:viewBox
         (str
           "0 0 "
           coord-width
           " "
           coord-height)})))

#_
(defn
  set-display-size
  "Wrap the `svg-element` with an `<svg>` container.
  `coord-width` and `coord-height` specify the viewport coordinate ranges.
  ie. the local coordinates where the element is located.
  The width/height are left unspecified..
   "
  ([svg-element
    [coord-width
     coord-height]
    display-width]
   (->> svg-element
        (svg/svg
          {:width
           display-width
           :viewBox
           (str
             "0 0 "
             coord-width
             " "
             coord-height)})))
  ([svg-element
    [coord-width
     coord-height]
    display-width
    display-height]
   (->> svg-element
        (svg/svg
          {:width
           display-width
           ;;
           :height
           display-height
           :viewBox
           (str
             "0 0 "
             coord-width
             " "
             coord-height)}))))

(defn
  set-display-width
  "Wrap the `svg-element` with an `<svg>` container
  and set it to a fixed  display `width` (as it'll appear by renderer)
  The height will be adjusted to preserve the aspect ratio.
  `coord-width` and `coord-height` specify the viewport coordinate ranges.
  ie. the local coordinates where the element is located"
  [svg-element
   display-width
   [coord-width
    coord-height]]
  (let [aspect-ratio (/
                       coord-width
                       coord-height)]
    (->>
      svg-element
      (svg/svg
        {:width   display-width
         :height  (/
                    display-width
                    aspect-ratio)
         :viewBox (str
                    "0 0 "
                    coord-width
                    " "
                    coord-height)}))))

(defn
  serialize-with-line-breaks
  "Takes a SVG string (in XML format) and inserts some line breaks
  This prevents the SVG from being just one super long line"
  [svg-xml-str]
  (-> svg-xml-str
      svg/serialize
      (#(clojure.string/replace
          %
          #"><"
          ">\n<"))))

(defn
  tick-formatter
  "Returns a function that formats a value to a given spacing
  .Ex:
  * 123 at 0.1 spacing gives: 123.0°
  * 123.3 at 0.01 spacing gives: 123.30°"
  [tick-spacing]
  (let [power (Math/log10
                tick-spacing)]
    (fn
      [tick-value]
      (str
        ((viz/value-formatter
           (if (neg?
                 power)
             (->
               power
               Math/abs
               long)
             0))
         tick-value)))))

(defn
  standard-axis
  "A 2D scatter plot with circles.
  Data points are either just a element vector
  [`x` `y` `r` `attrib`]
  Where `r` and `attrib` are optional
  TODO: Explain their defaults"
  ([data
    width
    height]
   (standard-axis
     data
     width
     height
     0.1
     36))
  ([data
    width
    height
    margin-frac
    scale]
   (let[#_#_
        tick-size  (/
                     (max
                       width
                       height)
                     scale-factor)
        xs      (->>
                  data
                  (map
                    first))
        ys      (->>
                  data
                  (map
                    second))
        x-min   (let [min (apply
                            min
                            xs)]
                  (if (pos?
                        min)
                    0.0
                    min))   
        x-max   (let [max (apply
                            max
                            xs)]
                  (if (neg?
                        max)
                    0.0
                    max))
        y-min   (let [min (apply
                            min
                            ys)]
                  (if (pos?
                        min)
                    0.0
                    min))   
        y-max   (let [max (apply
                            max
                            ys)]
                  (if (neg?
                        max)
                    0.0
                    max))
        x-range (max
                  (Math/abs
                    x-max)
                  (Math/abs
                    x-min))
        y-range (max
                  (Math/abs
                    y-max)
                  (Math/abs
                    y-min))]
     {:x-axis (viz/linear-axis
                {:domain      [(-
                                 (if (zero?
                                       x-min)
                                   0.0
                                   (-
                                     x-range))
                                 (*
                                   x-range
                                   margin-frac))
                               (+
                                 (if (zero?
                                       x-max)
                                   0.0
                                   x-range)
                                 (*
                                   x-range
                                   margin-frac))]
                 :range       [0
                               width]
                 :major       (->>
                                x-range
                                Math/log10
                                long
                                (Math/pow
                                  10))
                 :label       (viz/default-svg-label 
                                (tick-formatter
                                  (->>
                                    x-range
                                    Math/log10
                                    long
                                    (Math/pow
                                      10))))
                 :pos         (let [data-height (/
                                                  height
                                                  (+
                                                    1.0
                                                    (*
                                                      2.0
                                                      margin-frac)))
                                    offset      (*
                                                  margin-frac
                                                  data-height)]
                                (if (zero?
                                      y-max)
                                  offset
                                  (if (zero?
                                        y-min)
                                    (-
                                      height
                                      offset)
                                    (/
                                      height
                                      2.0))))
                 :label-dist  (/
                                scale
                                1.75)
                 :label-style {:fill        "black"
                               :stroke      "none"
                               :font-family "Arial, sans-serif"
                               :font-size   (/
                                              scale
                                              2.0)
                               :text-anchor "start"
                               :transform   (str
                                              "translate("
                                              (/
                                                scale
                                                4.0)
                                              " "
                                              0.0
                                              ")")}})
      :y-axis (viz/linear-axis
                {:domain       [(-
                                  (if (zero?
                                        y-min)
                                    0.0
                                    (-
                                      y-range))
                                  (*
                                    y-range
                                    margin-frac))
                                (+
                                  (if (zero?
                                        y-max)
                                    0.0
                                    y-range)
                                  (*
                                    y-range
                                    margin-frac))]
                 :range        [height
                                0]
                 :major        (->>
                                 y-range
                                 Math/log10
                                 long
                                 (Math/pow
                                   10))
                 :label        (viz/default-svg-label 
                                 (tick-formatter
                                   (->>
                                     y-range
                                     Math/log10
                                     long
                                     (Math/pow
                                       10))))
                 :pos          (let [data-width (/
                                                  width
                                                  (+
                                                    1.0
                                                    (*
                                                      2.0
                                                      margin-frac)))
                                     offset     (*
                                                  margin-frac
                                                  data-width)]
                                 (if (zero?
                                       x-max)
                                   (-
                                     width
                                     offset)
                                   (if (zero?
                                         x-min)
                                     offset
                                     (/
                                       width
                                       2.0))))
                 :label-dist   (/
                                 scale
                                 4.0)
                 :label-y      (/
                                 scale
                                 -4.0)
                 :label-style  {:fill   "black"
                                :stroke "none"
                                :font-family "Arial, sans-serif"
                                :font-size   (/
                                               scale
                                               2.0)
                                :text-anchor "end"}})
      :grid   {:attribs {:stroke
                         "#caa"}
               :minor-x true
               :minor-y true}
      :data   []})))

(defn
  adjustable-circles
  ([data]
   (adjustable-circles
     data
     36))
  ([data
    scale]
   {:values data
    :shape  (fn
              [[[plot-x
                 plot-y]
                [_ ;; data-x
                 _ ;; data-y
                 r
                 attribs]]]
              (svg/circle
                [plot-x
                 plot-y]
                (if (nil?
                      r)
                  (/
                    scale
                    3.0)
                  r)
                attribs))
    :layout viz/svg-scatter-plot}))

(defn
  process-points-less
  [{:keys [x-axis y-axis project]}
   {:keys [values item-pos shape]}]
  (let [[ry1
         ry2] (:range
               y-axis)]
    (->>
      values
      #_
      (if item-pos
        (sort-by (comp first item-pos) values)
        (sort-by first values))
      (sequence
        (viz/value-transducer
          {:cull-domain (:domain
                         x-axis)
           :cull-range  (if (<
                              ry1
                              ry2)
                          [ry1
                           ry2]
                          [ry2
                           ry1])
           :item-pos    item-pos
           :scale-x     (:scale
                         x-axis)
           :scale-y     (:scale
                         y-axis)
           :project     project
           :shape       shape})))))

(defn
  svg-trueline-plot
  [v-spec
   d-spec]
  (svg/line-strip
    (map
      first
      (process-points-less
        v-spec
        d-spec))
    (:attribs
     d-spec)))

(defn
  dashed-line
  ([data]
   (dashed-line
     data
     36))
  ([data
    scale]
   {:values  data
    :attribs {:stroke-dasharray (str
                                  (/
                                    scale
                                    10.0)
                                  " "
                                  (/
                                    scale
                                    10.0))
              :stroke-width     (/
                                  scale
                                  10.0)
              :stroke           "#aaa"}
    :layout  svg-trueline-plot}))

(defn
  adjustable-text
  "add a customizable text display
  `data` is a vector of [x,y,text,attribs]"
  ([data]
   (adjustable-text
     data
     36))
  ([data
    scale]
   {:values data             ;; each data point as an [a b text attrib]]
    :shape  (fn              ;; the `fn` takes [[x y] [a b text attrib]]
              [[[plot-x      ;; [x y] are the drawing coords on the plot
                 plot-y]
                [_ ;; data-x ;; [a b] are the original data point values
                 _ ;; data-y
                 text        ;; "extra" provided data
                 attribs]]]
              (svg/text
                [plot-x
                 plot-y]
                (str
                  text)
                (merge
                  {:fill              "#444"
                   :stroke            "none"
                   :text-anchor       "middle"
                   :dominant-baseline "central"
                   :font-size         (/
                                        scale
                                        3.0)}
                  attribs)))
    :layout viz/svg-scatter-plot}))

(defn
  index-text
  "Calls `adjustable-text` but inserts the index automatically"
  ([data]
   (index-text
     data
     36))
  ([data
    scale]
   (adjustable-text
     (map-indexed
       #(conj
          %2
          %1) 
       data)
     scale)))

;; see: https://github.com/thi-ng/color/issues/10
(prefer-method
  clojure.pprint/simple-dispatch
  clojure.lang.IPersistentMap
  clojure.lang.IDeref)

;; color maps are from
;; https://colorcet.com/download/index.html
;; and are under the Creative Commons BY License
(def
  cyclic-colors
  (->>
    "colorcet/CET-C2.csv"
    io/resource ;; loads from local classpath
    slurp
    csv/read-csv
    (map
      (fn
        [color]
        (map
          #(Double/parseDouble
             %)
          color)))
    (mapv
      #(apply
         col/rgba
         %))))

(defn
  color-cycle
  "Colors are selected cyclically. 1.0 is one period
  For instance 34.56 will get the 0.56th part of the cycle
  (ie. a little past the middle)"
  [value]
  (get
    cyclic-colors
    (->
      value
      double
      (rem       ; get the number past the decimal
        1)
      (*         ; get a value on the 0-255 range
        255)
      (Math/abs)
      (Math/round)))) ; get an integer value

(def red-blue-colors
  (->>
    "colorcet/CET-D01.csv"
    io/resource ;; loads from local classpath
    slurp
    csv/read-csv
    (map
      (fn
        [color]
        (map
          #(Double/parseDouble
             %)
          color)))
    (mapv
      #(apply
         col/rgba
         %))))


#_
(let[ sample-projections [[-0.04614217920447917 0.06826029947827388]
                          [-0.014056893042169616 0.011246558242909377]
                          [-0.18018705939173477 0.14419877479058196]
                          [-0.034558336531382704 -0.01972281558736888]
                          [-0.07826916038382854 -0.043453595365700134]
                          [-0.07595106933462975 -0.036233456688949346]
                          [-0.10658686700273674 -0.10651013658864265]
                          [-0.11161825055563879 -0.03034312002254201]
                          [-0.10363849165890278 -0.05600494146146514]
                          [-0.08944582472308728 0.0617341201300339]
                          [-0.0805716316475419 0.0920094992495933]
                          [-0.058556362585488235 0.10023194353547178]
                          [-0.08963113107308585 0.0669861996517699]
                          [-0.03480928077498672 0.008549803537571342]
                          [-0.05147942183462424 0.015436188315113902]
                          [-0.0856798832821775 -0.015085561150563798]
                          [-0.09359460674074901 -0.08814280875033947]
                          [-0.08708987351082897 -0.0939955237752148]
                          [-0.11960465052978983 -0.06152107951593767]
                          [-0.0864640297123831 -0.07694585125450254]
                          [-0.12750306575887743 -0.12714906884494703]
                          [-0.07159674930805907 0.03064382348394822]
                          [-0.10942569016066482 0.10024531333139809]
                          [-0.07449241266693139 0.11639335005087514]
                          [-0.024479080272290125 0.01925205753658918]
                          [-0.03748310584755013 0.0421901113794045]
                          [-0.00820997191997754 0.010183455336463795]
                          [-0.09942447600273058 0.024155884679020825]
                          [-0.09570939699304745 -0.07368149729846367]
                          [-0.11129915033057107 -0.12233329137297463]
                          [-0.13336408221299142 -0.15592325052594858]
                          [-0.08019732945739468 -0.048008902046311065]
                          [-0.10300119313457952 -0.05423642087815547]
                          [-0.11486517250904549 -0.014490808839586951]
                          [-0.12846745915459556 0.15602707023026274]
                          [-0.03176131657737967 0.06588158153074793]
                          [-0.013428037849488005 0.02292721202263743]
                          [-0.002361972669023225 0.002272720387289139]
                          [-0.010904058215213515 0.0033052962924459955]
                          [-0.04771473281388967 0.007244885688156339]
                          [-0.07613420894289795 -0.028684209398376542]
                          [-0.10535831378486148 -0.10700746395552906]
                          [-0.11490809338651006 -0.16173223467283657]
                          [-0.10172363266775356 -0.022571977181865426]
                          [-0.10530772703658517 -0.094707236190058]
                          [-0.1358923384122383 0.004969343812837888]
                          [-0.10809943032015985 0.09892070690492538]
                          [-0.09166142798940678 0.14293850402203517]
                          [-0.01722977377333619 0.025542554223672374]
                          [-0.010228563422289226 2.0013682388425748E-4]
                          [-0.005628812707297133 -8.957769418456989E-4]
                          [-0.05160441451859218 0.032602712404620635]
                          [-0.07010625994541916 -0.028714875978810003]
                          [-0.10315419778593654 -0.06583480517745689]
                          [-0.08465557882448017 -0.047093037526882084]
                          [-0.11768578291364805 -0.09419181475764743]
                          [-0.14952725879427117 -0.11307881126986363]
                          [-0.0752265993428652 0.0636710539704381]
                          [-0.11880470567824283 0.16711318789939464]
                          [-0.04994379523510075 0.07547595033578237]
                          [-0.027380062918987827 0.024186601603888306]
                          [-0.012349773925011315 0.01710314618920225]
                          [-0.0035227804778297533 -1.461235170169339E-4]
                          [-0.006823807564509562 0.004033254072001074]
                          [-0.14094457594772872 -0.19333227221850102]
                          [-0.11782195943877609 -0.10391519706476034]
                          [-0.12029550370076246 -0.058964408384443716]
                          [-0.08725135260509324 -0.06514325307321894]
                          [-0.12022865582109155 -0.16061625755428907]
                          [-0.12787821806386515 -0.05773756060738432]
                          [-0.059314813820759836 0.044693924659005166]
                          [-0.16995057691880694 0.3605218488540876]
                          [-0.17529290021297028 0.26603503013894536]
                          [-0.02025730883222568 0.0165625433517073]
                          [-0.04459469414928244 0.03778774677515535]
                          [-0.07469003039361882 0.05742297096220064]
                          [-0.12471533533289647 -0.09058339264953447]
                          [-0.08486918031351369 -0.061461197015212854]
                          [-0.07751606793847889 -0.049983639887724376]
                          [-0.10950635332021763 -0.07419392500341594]
                          [-0.13303412470122158 -0.05769861561604688]
                          [-0.07269818604620934 -0.013068445607436224]
                          [-0.1781789254464426 0.3372513198328054]
                          [-0.06773672464324973 0.09273081789443394]
                          [-0.05829671840832622 0.03533665946351557]
                          [-0.01279977841548696 0.006478466954344174]
                          [-0.02161748933980635 0.001115288392603268]
                          [-0.08127554087552595 -0.021761152298522827]
                          [-0.09005073602829375 -0.023175377256335217]
                          [-0.10445741219998042 -0.10031054138776305]
                          [-0.09423647013253034 -0.08534848949562604]
                          [-0.09750440577848028 -0.10757722412853606]
                          [-0.09395422500672321 -0.05151230875244816]
                          [-0.1133038123024795 0.05331392681683984]
                          [-0.095287458428633 0.10135034749566281]
                          [-0.10106784239233887 0.18473856447867862]
                          [-0.06637742229426154 0.12568011955745667]
                          [-0.005319369786013747 0.0011308456906113447]
                          [-0.009607088414258618 0.007701357255388795]
                          [-0.039447998255923565 -0.012072647209001032]
                          [-0.08505394630015928 -0.04567655143379476]
                          [-0.11413363662538044 -0.08178975037439364]
                          [-0.07563502253707266 -0.03587524640156776]
                          [-0.11643757752313696 -0.1378060757345358]
                          [-0.09429716673584863 -0.03326578223222165]
                          [-0.1006573827872608 0.0787817804893973]
                          [-0.07496590276689227 0.02191346387152431]
                          [-0.018163944466763338 0.02687080086243359]
                          [-0.0048297210278633014 0.002296895857323778]
                          [-0.014792007428904654 0.01255017190961904]
                          [-0.004957489686918548 0.003136236551692455]
                          [-0.05774828184109498 0.05657172795956388]
                          [-0.10192295912899658 -0.019456291684133313]
                          [-0.11616142874332579 -0.03919225355543778]
                          [-0.10099056180148522 3.0925720914045606E-6]
                          [-0.08020474233352687 -0.03336777158340903]
                          [-0.15362336379631628 -0.06361794343009722]
                          [-0.14690961790698875 -0.06429230926537097]
                          [-0.10099303440764114 0.12223651116682559]
                          [-0.09737276162805063 0.1688277553150298]]]
  (let [width  1000
        height 1000
        data   sample-projections]
    (->>
      (->
        (quickthing/standard-axis ;; create the axis
          data
          1000
          1000)
        (assoc                    ;; add data to the plot
          :data
          [(dashed-line           ;; dashed line
             data)
           (adjustable-circles    ;; circles that change color
             (map-indexed
               (fn
                 [index
                  data-point]
                 (conj
                   data-point
                   nil ;; default radius
                   {:fill   (color-cycle
                              (-
                                12.0
                                (/
                                  (+
                                    index
                                    3.0) ;; so it starts in blue
                                  12.0)))
                    :stroke "#777"}))
               data))
           (index-text            ;; text label
             data)])
        (assoc                    ;; turn off grid
          :grid
          nil))
      (viz/svg-plot2d-cartesian)  ;; turns the plot to svg hiccup
      (svg/svg                    ;; wraps in an `<svg>` element
        {:width  width
         :height height})
      (svg/serialize)             ;; turns it to XML
      (spit                       ;; writes to file
        "out/test.svg"))))

(defn group-plots-row
  "Take a vector of plots and groups them into a row.
      For now it must take a `widths` parameter.
      There is no way to determine the width from the svg elements.
      Hence all plots are the same width"
  [row-of-plots]
  (let [max-height-in-row (->>
                            row-of-plots
                            (map
                              (fn
                                [plot]
                                (->
                                  plot
                                  (get
                                    1)
                                  :height)))
                            (apply
                              max))
        [total-offset
         adjusted-plots]  (reduce
                            (fn
                              [[horizontal-offset
                                horizontal-plots]
                               next-plot]
                              (println
                                horizontal-offset)
                              (let [next-plot-width (->
                                                      next-plot
                                                      (get
                                                        1)
                                                      :width)
                                    next-offset     (+
                                                      horizontal-offset
                                                      next-plot-width)]
                                [next-offset
                                 (conj
                                   horizontal-plots 
                                   (svg/group
                                     {:transform (geom/translate
                                                   matrix/M32
                                                   [horizontal-offset
                                                    0])}
                                     next-plot))]))
                            [0
                             []]
                            row-of-plots)]
    [total-offset
     max-height-in-row
     (apply
       svg/group
       {}
       adjusted-plots)]))

(defn
  group-plots-grid
  "Given a vector of vectors of plots
  Arrange the plots row by row into on SVG
  All plots must have the same `widths` and `heights`
  .[Note]
  Plot dimensions can't be determined from the plot's SVG group"
  [plots]
  (let [[total-height
         final-horizontal-offsets
         adjusted-row-groups] (reduce
                                (fn
                                  [[vertical-offset
                                    horizontal-offsets
                                    vertical-plot-rows]
                                   row-of-plots]
                                  (let [[horizontal-offset
                                         max-height-in-row
                                         concatenated-row] (group-plots-row
                                                             row-of-plots)]
                                    [(+
                                       vertical-offset
                                       max-height-in-row)
                                     (conj
                                       horizontal-offsets
                                       horizontal-offset)
                                     (conj
                                       vertical-plot-rows
                                       (svg/group
                                         {:transform (geom/translate
                                                       matrix/M32
                                                       [0
                                                        vertical-offset])}
                                         concatenated-row))]))
                                [0 ;; initial for `reduce`
                                 []
                                 []]
                                plots)
        total-width           (apply
                                max
                                final-horizontal-offsets)]
    (->>
      (apply
        svg/group
        {}
        adjusted-row-groups)
      (svg/svg
        {:width   total-width
         :height  total-height
         :viewBox (str
                    "0 0 "
                    total-width
                    " "
                    total-height)}))))

(defn
  histogram
  "`geom-viz` bar graphs assume only positive values
  To plot negative values you need to generate two bar graphs
  And then have them glued together"
  ([data
    width
    height
    left-margin]
   (histogram
     data
     width
     height
     left-margin
     false))                 ;; default not inverted
  ([data
    width
    height
    left-margin
    inverted?]
   (let [xs    (->>
                 data
                 (map
                   first))
         ys    (->>
                 data
                 (map
                   second))
         min-x (apply
                 min
                 xs)
         max-x (apply
                 max
                 xs)
         min-y (apply
                 min
                 ys)
         max-y (apply
                 max
                 ys)]
     (histogram
       data
       width
       height
       left-margin
       inverted?
       [(-
          min-x
          0.5)
        (+
          max-x
          0.5)
        (if (pos?
              min-y)
          -0.5
          (-
            min-y
            0.5))
        (if (neg?
              max-y)
          0.5
          (+
            max-y
            0.5))])))
  ([data
    width
    height
    left-margin
    inverted?
    [min-x
     max-x
     min-y
     max-y]]
   ;; There is a small bug in `geom-viz`
   ;; where the axis isn't drawn when it's inverted
   ;; so we draw the axis separately
   (let[axis (viz/svg-plot2d-cartesian
               {:x-axis (viz/linear-axis
                          {:domain      [min-x
                                         max-x]
	                   :range       [left-margin
                                         width]
	                   :minor       0
	                   :label-style {:fill "none"}
	                   :pos         (*
                                          height
                                          (/
                                            (if inverted?
                                              (Math/abs
                                                min-y)
                                              max-y)
                                            (+
                                              max-y
                                              (Math/abs
                                                min-y))))})
                :y-axis (viz/linear-axis
                          {:domain [min-y
                                    max-y]
	                   :range  (if inverted?
                                     [0
                                      height]
                                     [height
                                      0])
	                   :major  1
	                   :minor  1
	                   :pos    left-margin})
                :grid   {:minor-x true
                         :minor-y false}
                :data   []})
        bars (viz/svg-plot2d-cartesian
               {:x-axis (viz/linear-axis
                          {:domain      [min-x
                                         max-x]
	                   :range       [left-margin
                                         width]
                           :visible true
	                   :pos         (*
                                          height
                                          (/
                                            (if inverted?
                                              (Math/abs
                                                min-y)
                                              max-y)
                                            (+
                                              max-y
                                              (Math/abs
                                                min-y))))})
                :y-axis (viz/linear-axis
                          {:domain [0
                                    (if inverted?
                                      min-y
                                      max-y)]
	                   :range  [(*
                                      height
                                      (/
                                        (if inverted?
                                          (Math/abs
                                            min-y)
                                          max-y)
                                        (+
                                          max-y
                                          (Math/abs
                                            min-y))))
                                    0]
                           :visible true
	                   :pos    left-margin})
                :grid   {:minor-x true
                         :minor-y false}
                :data   [{:values data
                          :attribs {:fill         "none"
                                    :stroke-width (* 0.75
                                                     (/
                                                       (-
                                                         width
                                                         left-margin)
                                                       max-x))
                                    :stroke       "#ffb2b0"}
                          :layout  viz/svg-bar-plot}]})]
     (svg/group
       {}
       axis
       bars))))
#_
(->>
  (histogram
    [[0 1]
     [1 2]
     [2 3]
     [3 4]
     [4 5]]
    1000
    400
    40
    #_
    [-0.5
     4.5
     0
     6])
  (svg/svg
    {:width  1000
     :height 400})
  serialize-with-line-breaks
  (spit
    "out/hist-test.svg"))
