(ns quickthing
  "Helpers for plotting with `thing.geom.viz`"
  (:use [clojure.math])
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

(def
  default-width 1000)

(def
  default-height 600)

(defn
  svg2xml
  "This takes the SVG hiccup,
  - serializes it
  - inserts new line characters
  If the SVG is output as one one long line.. most editors are unhappy"
  [svg]
  (-> svg
      svg/serialize
      (clojure.string/replace #"><"
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
    (->> svg-element
         (svg/svg {:viewBox
                   (str "0 0 "
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
        (svg/svg {:width
                  display-width
                  :viewBox
                  (str "0 0 "
                       coord-width
                       " "
                       coord-height)})))
  ([svg-element
    [coord-width
     coord-height]
    display-width
    display-height]
   (->> svg-element
        (svg/svg {:width
                  display-width
                  ;;
                  :height
                  display-height
                  :viewBox
                  (str "0 0 "
                       coord-width
                       " "
                       coord-height)}))))

(defn
  svg-wrap
  "Wrap the `svg-element` with an `<svg>` container
  and set it to a fixed  display `width` (as it'll appear by renderer)
  The height will be adjusted to preserve the aspect ratio.
  `coord-width` and `coord-height` specify the viewport coordinate ranges.
  ie. the local coordinates where the element is located"
  ([svg-element]
   (svg-wrap svg-element
             [default-width, default-height]))
  ([svg-element
    [coord-width, coord-height]]
   (->> svg-element
        (svg/svg {:viewBox (str "0 0 "
                                coord-width
                                " "
                                coord-height)})))
  ([svg-element
    [coord-width
     coord-height]
    display-width]
   (let [aspect-ratio (/ coord-width
                         coord-height)]
     (->> svg-element
          (svg/svg {:width   display-width
                    :height  (/ display-width
                                aspect-ratio)
                    :viewBox (str "0 0 "
                                  coord-width
                                  " "
                                  coord-height)})))))

#_
(defn
  serialize
  "DOES THIS WORK???
  Takes a SVG string (in XML format) and inserts some line breaks
  This prevents the SVG from being just one super long line"
  [svg-xml-str]
  (-> svg-xml-str
      svg/serialize
      (#(clojure.string/replace %
                                #"><"
                                ">\n<"))))

(defn
  svg-title
  "adding the `title` element
  useful for tooltips
  https://developer.mozilla.org/en-US/docs/Web/SVG/Element/title "
  [text]
  [:title
   nil
   text])

(defn
  adjustable-text
  "add a customizable text display
  `data` is a vector of [x,y,text,attribs]"
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  [{:values data             ;; each data point as an [a b text attrib]]
    :shape  (fn [[[plot-x            ;; the `fn` takes [[x y] [a b text attrib]]
                   plot-y]  ;; [x y] are the drawing coords on the plot
                  [_ ;; data-x ;; [a b] are the original data point values
                   _ ;; data-y
                   text        ;; "extra" provided data
                   inner-attribs]]]
              (let [tooltip      (:tooltip inner-attribs)
                    text-element (svg/text [plot-x, plot-y]
                                           (str text)
                                           (merge {:fill              "#444"
                                                   :stroke            "none"
                                                   :text-anchor       "middle"
                                                   :dominant-baseline "central"
                                                   :font-size         (/ scale
                                                                         3.0)}
                                                  inner-attribs
                                                  attribs))]
                (if (nil? tooltip)
                  text-element
                  (svg/group {}
                             text-element
                             (svg-title tooltip)))))
    :layout viz/svg-scatter-plot}])

#_
(format "%.2G" 9900.0)
;; => "1.8E+03"
(defn
  tick-formatter
  "Returns a function that formats a value to a given spacing
  .Ex:
  * 123 at 0.1 spacing gives: 123.0°
  * 123.3 at 0.01 spacing gives: 123.30°"
  [tick-spacing]
  (let [power (Math/log10
                tick-spacing)]
    (fn [tick-value]
      (if (> (abs tick-value)
             10000)
        (->> tick-value
             (format "%.2G")
             str)
      (str ((viz/value-formatter (if (neg? power)
                                   (-> power
                                       Math/abs
                                       long)
                                   0))
            tick-value))))))

(defn
  zero-axis
  "A ~standard~ axis. With the zero point int he plot
  Will adjust to data .. ex: if x/y data is positive..
  then the intersection point will be at the bottom left"
  [data
   & [{:keys [width
              height
              margin-frac
              scale
              x-name ;; names for the axis
              y-name
              color
              title]
       :or   {width       default-width
              height      default-height
              margin-frac nil
              scale       36
              color       "#0008"}}]]
  (let [margin-frac (if (nil? margin-frac)
                      (/ scale
                         100.0)
                      margin-frac)
        xs      (->> data
                     (map first))
        ys      (->> data
                     (map second))
        x-min   (let [min (apply min
                                 xs)]
                  (if (pos? min)
                    0.0
                    min))
        x-max   (let [max (apply max
                                 xs)]
                  (if (neg? max)
                    0.0
                    max))
        y-min   (let [min (apply min
                                 ys)]
                  (if (pos? min)
                    0.0
                    min))
        y-max   (let [max (apply  max
                                  ys)]
                  (if (neg? max)
                    0.0
                    max))
        x-range (max (Math/abs x-max)
                     (Math/abs x-min))
        y-range (max (Math/abs y-max)
                     (Math/abs y-min))]
    {:x-axis (viz/linear-axis
               {:domain      [(- (if (zero? x-min)
                                   0.0
                                   (- x-range))
                                 (* x-range
                                    margin-frac))
                              (+ (if (zero? x-max)
                                   0.0
                                   x-range)
                                 (* x-range
                                    margin-frac))]
                :range       [0, width]
                :major       (->> x-range
                                  Math/log10
                                  long
                                  (Math/pow 10))
                :label       (viz/default-svg-label (->> x-range
                                                         Math/log10
                                                         long
                                                         (Math/pow 10)
                                                         tick-formatter))
                :pos         (let [data-height (/ height
                                                  (+ 1.0
                                                     (* 2.0
                                                        margin-frac)))
                                   offset      (* margin-frac
                                                  data-height)]
                               (if (zero? y-max)
                                 offset
                                 (if (zero? y-min)
                                   (- height
                                      offset)
                                   (/ height
                                      2.0))))
                :label-dist  (/ scale
                                1.75)
                :label-style {:fill        "black"
                              :stroke      "none"
                              :font-family "Arial, sans-serif"
                              :font-size   (/ scale
                                              2.0)
                              :text-anchor "start"
                              :transform   (str "translate("
                                                (/ scale
                                                   4.0)
                                                " "
                                                0.0
                                                ")")}})
     :y-axis (viz/linear-axis
               {:domain      [(- (if (zero? y-min)
                                   0.0
                                   (- y-range))
                                 (* y-range
                                    margin-frac))
                              (+ (if (zero? y-max)
                                   0.0
                                   y-range)
                                 (* y-range
                                    margin-frac))]
                :range       [height
                              0]
                :major       (->> y-range
                                  Math/log10
                                  long
                                  (Math/pow 10))
                :label       (viz/default-svg-label (->> y-range
                                                         Math/log10
                                                         long
                                                         (Math/pow 10)
                                                         tick-formatter))
                :pos         (let [data-width (/ width
                                                 (+ 1.0
                                                    (* 2.0
                                                       margin-frac)))
                                   offset     (* margin-frac
                                                 data-width)]
                               (if (zero? x-max)
                                 (- width
                                    offset)
                                 (if (zero? x-min)
                                   offset
                                   (/ width
                                      2.0))))
                :label-dist  (/ scale
                                4.0)
                :label-y     (/ scale
                                -4.0)
                :label-style {:fill        "black"
                              :stroke      "none"
                              :font-family "Arial, sans-serif"
                              :font-size   (/ scale
                                              2.0)
                              :text-anchor "end"}})
     :grid   {:attribs {:stroke
                        "#caa"
                        :stroke-dasharray (str (/ scale
                                                  10.0)
                                               " "
                                               (/ scale
                                                  5.0))
                        :stroke-width     (/ scale
                                             50.0)}
              :minor-x true
              :minor-y true}
     :data   (cond-> []
               (some? title)  (conj (quickthing/adjustable-text[[x-max
                                                                 y-max
                                                                 title
                                                                 {:dx                (- (/ scale
                                                                                           2.0))
                                                                  :dy                (/ scale
                                                                                        2.0)
                                                                  :font-size         scale
                                                                  :font-family       "Arial, sans-serif"
                                                                  :fill              "#0004"
                                                                  :text-anchor       "end"
                                                                  :dominant-baseline "hanging"}]]))
               (some? x-name) (conj (quickthing/adjustable-text [[(/ x-max
                                                                     2.0)
                                                                  0
                                                                  x-name
                                                                  {:dy                (/ scale
                                                                                         1.5)
                                                                   :font-size         (/ scale
                                                                                         2.75)
                                                                   :font-family       "Arial, sans-serif"
                                                                   :text-anchor       "middle"
                                                                   :dominant-baseline "hanging"}]]))
               (some? y-name) (conj (quickthing/adjustable-text [[0
                                                                  (/ y-max
                                                                     2.0)
                                                                  y-name
                                                                  {:dx                (/ scale
                                                                                         -1.0)
                                                                   :writing-mode      "vertical-lr"
                                                                   :text-orientation  "sideways"
                                                                   :font-size         (/ scale
                                                                                         2.75)
                                                                   :font-family       "Arial, sans-serif"
                                                                   :text-anchor       "middle"
                                                                   :dominant-baseline "bottom"}]])))}))

(defn
  primary-axis
  "A primary axis in the bottom left
  Zero point might be out of bounds
  Will adjust to data .. ex: if x/y data is positive..
  then the intersection point will be at the bottom left"
  [data
   & [{:keys [width
              height
              logarithmic-y?
              margin-frac
              y-breathing-room
              scale
              x-name ;; names for the axis
              y-name
              x-ticks
              y-ticks
              main-color
              color
              title]
       :or   {width            default-width
              height           default-height
              margin-frac      nil
              y-breathing-room 0.1
              scale            36
              main-color       "black"
              color            "black"}}]]
  (let [margin-frac (if (nil? margin-frac)
                      (/ scale
                         400.0)
                      margin-frac)
        xs         (->> data
                        (map first))
        ys         (->> data
                        (map second))
        x-min      (apply min
                          xs)
        x-max      (apply max
                          xs)
        y-min      (apply min
                          ys)
        y-max      (apply max
                          ys)
        x-range    (- x-max
                      x-min)
        y-range    (- y-max
                      y-min)
        y-full-max (+ y-max
                      (* y-range
                         y-breathing-room))
        y-full-min (let [y-min-with-buffer (- y-min
                                              (* y-range
                                                 y-breathing-room))]
                     ;; don't add bottom buffer space if you're around zero
                     (if (and (>= y-min
                                  0)
                              (neg? y-min-with-buffer))
                       0.0
                       y-min-with-buffer))]
    {:x-axis (viz/linear-axis {:domain      [x-min, x-max]
                               :range       [(* margin-frac
                                                width)
                                             (- width
                                                (* margin-frac
                                                   width))]
                               :major       (->> x-range
                                                 Math/log10
                                                 long
                                                 (Math/pow 10))
                               :label       (->> x-range
                                                 Math/log10
                                                 long
                                                 (Math/pow 10)
                                                 tick-formatter
                                                 viz/default-svg-label)
                               :pos         (- height
                                               (* margin-frac
                                                  height))
                               :label-dist  (/ scale
                                               1.75)
                               :attribs     {:stroke main-color}
                               :label-style {:fill        main-color
                                             :stroke      "none"
                                             :font-family "Arial, sans-serif"
                                             :font-size   (/ scale
                                                             2.0)
                                             :text-anchor "start"
                                             :transform   (str "translate("
                                                               (/ scale
                                                                  4.0)
                                                               " "
                                                               0.0
                                                               ")")}})
     :y-axis (viz/linear-axis {:domain      [y-full-min
                                             y-full-max]
                               :range       [(- height
                                                (* margin-frac
                                                   height))
                                             (* margin-frac
                                                height)]
                               :major       (->> y-range
                                                 Math/log10
                                                 long
                                                 (Math/pow 10))
                               :label       (->> y-range
                                                 Math/log10
                                                 long
                                                 (Math/pow 10)
                                                 tick-formatter
                                                 viz/default-svg-label)
                               :pos         (* margin-frac
                                               width)
                               :major-size  (/ scale
                                               3.0)
                               :label-dist  (/ scale
                                               4.0)
                               :label-y     (/ scale
                                               -4.0)
                               :attribs     {:stroke color}
                               :label-style {:fill        main-color
                                             :stroke      "none"
                                             :font-family "Arial, sans-serif"
                                             :font-size   (/ scale
                                                             2.0)
                                             :text-anchor "end"}})
     :grid   {:attribs {:stroke           "#caa"
                        :stroke-dasharray (str (/ scale
                                                  10.0)
                                               " "
                                               (/ scale
                                                  5.0))
                        :stroke-width     (/ scale
                                             50.0)}
              :minor-x true
              :minor-y true}
     :data   (cond-> []
               (some? title)  (into (quickthing/adjustable-text [[x-max
                                                                  y-max
                                                                  title
                                                                  {:dx                (- (/ scale
                                                                                            1.0))
                                                                   :dy                (/ scale
                                                                                         2.0)
                                                                   :font-size         scale
                                                                   :font-family       "Arial, sans-serif"
                                                                   :fill              "#0004"
                                                                   :text-anchor       "end"
                                                                   :dominant-baseline "hanging"}]]))
               (some? x-name) (into (quickthing/adjustable-text [[(+ x-min
                                                                     (/ x-range
                                                                        2.0))
                                                                  y-full-max
                                                                  x-name
                                                                  {:dy                0
                                                                   :font-size         (/ scale
                                                                                         2.0)
                                                                   :font-family       "Arial, sans-serif"
                                                                   :font-style        "italic"
                                                                   :fill              main-color
                                                                   :text-anchor       "middle"
                                                                   :dominant-baseline "text-bottom"}]]))
               (some? y-name) (into (quickthing/adjustable-text [[x-min
                                                                  y-full-max
                                                                  y-name
                                                                  {:dx                (/ scale
                                                                                         2.0)
                                                                   :dy                (/ scale
                                                                                         2.0)
                                                                   ;;:writing-mode      "vertical-lr"
                                                                   :text-orientation  "sideways"
                                                                   :font-size         (/ scale
                                                                                         2.0)
                                                                   :fill              color
                                                                   :font-family       "Arial, sans-serif"
                                                                   :font-style        "italic"
                                                                   :text-anchor       "beginning"
                                                                   :dominant-baseline "hanging"}]]))
               #_#_ ;; this is the vertival `outside` look
               (some? y-name) (into (quickthing/adjustable-text [[x-max
                                                                  (+ y-full-min
                                                                     (/ (- y-full-max
                                                                           y-full-min)
                                                                        2.0))
                                                                  y-name
                                                                  {:dx               (/ scale
                                                                                        3.0)
                                                                   :dy               (/ scale
                                                                                        3.0)
                                                                   :writing-mode     "vertical-lr"
                                                                   :text-orientation "sideways"
                                                                   :font-size        (/ scale
                                                                                        2.0)
                                                                   :fill             color
                                                                   :font-family      "Arial, sans-serif"
                                                                   :font-style       "italic"
                                                                   :text-anchor      "middle"}]])))}))

(defn
  secondary-axis
  "An axis that always goes in the top right.
  If your x/y data is in one quadrant then..
  This can be drawn on top of a plot which uses the `standard-axis`
  This way you can display to things at once
  To make sense.. x-axis/dimensions should be the same
  Default `color` is grey. "
  [data
   & [{:keys [width
              height
              margin-frac
              y-breathing-room
              scale
              x-name ;; names for the axis
              y-name
              color
              main-color]
       :or   {width            default-width
              height           default-height
              margin-frac      nil
              y-breathing-room 0.1
              scale            36
              color            "#0088"
              main-color       "#0088"}}]]
  (let [margin-frac (if (nil? margin-frac)
                      (/ scale
                         400.0)
                      margin-frac)
        xs         (->> data
                        (map first))
        ys         (->> data
                        (map second))
        x-min      (apply min
                          xs)
        x-max      (apply max
                          xs)
        y-min      (apply min
                          ys)
        y-max      (apply max
                          ys)
        x-range    (- x-max
                      x-min)
        y-range    (- y-max
                      y-min)
        y-full-max (+ y-max
                      (* y-range
                         y-breathing-room))
        y-full-min (let [y-min-with-buffer (- y-min
                                              (* y-range
                                                 y-breathing-room))]
                     ;; don't add bottom buffer space if you're around zero
                     (if (and (>= y-min
                                  0)
                              (neg? y-min-with-buffer))
                       0.0
                       y-min-with-buffer))]
    {:x-axis  (viz/linear-axis {:domain      [x-min, x-max]
                                :range       [(* margin-frac
                                                 width)
                                              (- width
                                                 (* margin-frac
                                                    width))]
                                :major       (->> x-range
                                                  Math/log10
                                                  long
                                                  (Math/pow 10))
                                :label       (->> x-range
                                                  Math/log10
                                                  long
                                                  (Math/pow 10)
                                                  tick-formatter
                                                  viz/default-svg-label)
                                :pos         (* margin-frac
                                                height)
                                :major-size  0 #_ (/ scale
                                                     -6.0)
                                :label-dist  (/ scale
                                                -5.75)
                                :attribs     {:stroke main-color}
                                :label-style {:fill        "none" #_main-color
                                              :stroke      "none"
                                              :font-family "Arial, sans-serif"
                                              :font-size   (/ scale
                                                              2.0)
                                              :text-anchor "start"
                                              :transform   (str "translate("
                                                                (/ scale
                                                                   4.0)
                                                                " "
                                                                0.0
                                                                ")")}})
     :y-axis  (viz/linear-axis {:domain      [y-full-min
                                              y-full-max]
                                :range       [(- height
                                                 (* margin-frac
                                                    height))
                                              (* margin-frac
                                                 height)]
                                :major       (->> y-range
                                                  Math/log10
                                                  long
                                                  (Math/pow 10))
                                :label       (->> y-range
                                                  Math/log10
                                                  long
                                                  (Math/pow 10)
                                                  tick-formatter
                                                  viz/default-svg-label)
                                :pos         (- width
                                                (* margin-frac
                                                   width))
                                :major-size  (/ scale
                                                -3.0)
                                :label-dist  (/ scale
                                                -4.0)
                                :label-y     (/ scale
                                                -4.0)
                                :attribs     {:stroke color}
                                :label-style {:fill        main-color
                                              :stroke      "none"
                                              :font-family "Arial, sans-serif"
                                              :font-size   (/ scale
                                                              2.0)
                                              :text-anchor "beginning"}})
     :attribs {:stroke color
               :fill   color}
     #_#_
     :grid    {:attribs {:stroke           "#caa"
                         :stroke-dasharray (str (/ scale
                                                   10.0)
                                                " "
                                                (/ scale
                                                   5.0))
                         :stroke-width     (/ scale
                                              50.0)}
               :minor-x true
               :minor-y true}
     :data    (cond-> []
                #_#_
                (some? title)  (into (quickthing/adjustable-text [[x-max
                                                                   y-max
                                                                   title
                                                                   {:dx                (- (/ scale
                                                                                             1.0))
                                                                    :dy                (/ scale
                                                                                          2.0)
                                                                    :font-size         scale
                                                                    :font-family       "Arial, sans-serif"
                                                                    :fill              "#0004"
                                                                    :text-anchor       "end"
                                                                    :dominant-baseline "hanging"}]]))
                #_#_
                (some? x-name) (into (quickthing/adjustable-text [[(+ x-min
                                                                      (/ x-range
                                                                         2.0))
                                                                   y-full-max
                                                                   x-name
                                                                   {:dy                (/ scale
                                                                                          -3.0)
                                                                    :font-size         (/ scale
                                                                                          2.0)
                                                                    :font-family       "Arial, sans-serif"
                                                                    :font-style        "italic"
                                                                    :fill              main-color
                                                                    :text-anchor       "middle"
                                                                    :dominant-baseline "text-bottom"}]]))
                (some? y-name) (into (quickthing/adjustable-text [[x-max
                                                                   y-full-max
                                                                   y-name
                                                                   {:dx                (/ scale
                                                                                          -2.0)
                                                                    :dy                (/ scale
                                                                                          2.0)
                                                                    ;;:writing-mode      "vertical-lr"
                                                                    :text-orientation  "sideways"
                                                                    :font-size         (/ scale
                                                                                          2.0)
                                                                    :fill              color
                                                                    :font-family       "Arial, sans-serif"
                                                                    :font-style        "italic"
                                                                    :text-anchor       "end"
                                                                    :dominant-baseline "hanging"}]]))
                #_#_ ;; this is the vertical `outside` look
                (some? y-name) (into (quickthing/adjustable-text [[x-max
                                                                   (+ y-full-min
                                                                      (/ (- y-full-max
                                                                            y-full-min)
                                                                         2.0))
                                                                   y-name
                                                                   {:dx               (/ scale
                                                                                         3.0)
                                                                    :dy               (/ scale
                                                                                         3.0)
                                                                    :writing-mode     "vertical-lr"
                                                                    :text-orientation "sideways"
                                                                    :font-size        (/ scale
                                                                                         2.0)
                                                                    :fill             color
                                                                    :font-family      "Arial, sans-serif"
                                                                    :font-style       "italic"
                                                                    :text-anchor      "middle"}]])))})
  #_
  (let [xs      (->> data
                     (map first))
        ys      (->> data
                     (map second))
        x-min   (let [min (apply min
                                 xs)]
                  (if (pos? min)
                    0.0
                    min))
        x-max   (let [max (apply max
                                 xs)]
                  (if (neg? max)
                    0.0
                    max))
        y-min   (let [min (apply min
                                 ys)]
                  (if (pos? min)
                    0.0
                    min))
        y-max   (let [max (apply max
                                 ys)]
                  (if (neg? max)
                    0.0
                    max))
        x-range (max (Math/abs x-max)
                     (Math/abs x-min))
        y-range (max (Math/abs y-max)
                     (Math/abs y-min))]
    {:x-axis (viz/linear-axis {:domain      [x-min, x-max]
                               :range       [(* margin-frac
                                                width)
                                             (- width
                                                (* margin-frac
                                                   width))]
                               :major       (->> x-range
                                                 Math/log10
                                                 long
                                                 (Math/pow 10))
                               :major-size  (- 10) ;; 10 is default from `geom/viz`
                               :label       (viz/default-svg-label (tick-formatter
                                                                     (->> x-range
                                                                          Math/log10
                                                                          long
                                                                          (Math/pow 10))))
                               :pos         (* margin-frac
                                               height)
                               :attribs     {:stroke main-color}
                               :label-dist  (/ scale
                                               -5.75)
                               :label-style {:fill        main-color
                                             :stroke      "none"
                                             :font-family "Arial, sans-serif"
                                             :font-size   (/ scale
                                                             2.0)
                                             :text-anchor "start"
                                             :transform   (str "translate("
                                                               (/ scale
                                                                  4.0)
                                                               " "
                                                               0.0
                                                               ")")}})
     :y-axis (viz/linear-axis {:domain      [(* y-min
                                                y-breathing-room)
                                             (* y-max
                                                y-breathing-room)]
                               :range       [(- height
                                                (* margin-frac
                                                   height))
                                             (* margin-frac
                                                height)]
                               :major       (->> y-range
                                                 Math/log10
                                                 long
                                                 (Math/pow 10))
                               :major-size  (- 10)
                               :label       (viz/default-svg-label (tick-formatter (->> y-range
                                                                                        Math/log10
                                                                                        long
                                                                                        (Math/pow 10))))
                               :pos         (- width
                                               (* margin-frac
                                                  width))
                               :attribs     {:stroke color}
                               :label-dist  (/ scale
                                               -2.0)
                               :label-y     (/ scale
                                               -4.0)
                               :label-style {:fill        main-color
                                             :stroke      "none"
                                             :font-family "Arial, sans-serif"
                                             :font-size   (/ scale
                                                             2.0)
                                             :text-anchor "end"}})
     :data   (cond-> []
               ;; no title on secondary axis
               (some? y-name) (into (quickthing/adjustable-text [[x-max
                                                                  (+ y-min
                                                                     (/ y-range
                                                                        2.0))
                                                                  y-name
                                                                  {:dx                (/ scale
                                                                                         1.0)
                                                                   :font-size         (/ scale
                                                                                         2.75)
                                                                   :writing-mode      "vertical-rl"
                                                                   :fill              color
                                                                   :stroke            "none"
                                                                   :font-family       "Arial, sans-serif"
                                                                   :text-anchor       "middle"
                                                                   :dominant-baseline "bottom"}]]))
               (some? x-name) (into (quickthing/adjustable-text [[(/ x-range
                                                                     2.0)
                                                                  y-max
                                                                  x-name
                                                                  {:dy                (/ scale
                                                                                         -1.5)
                                                                   :font-size         (/ scale
                                                                                         2.75)
                                                                   :fill              main-color
                                                                   :stroke            "none"
                                                                   :font-family       "Arial, sans-serif"
                                                                   :text-anchor       "middle"
                                                                   :dominant-baseline "bottom"}]])))}))

(defn
  adjustable-circles
  "Draws circles.."
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  [{:values data
    :shape  (fn [[[plot-x, plot-y]
                  [_ ;; data-x
                   _ ;; data-y
                   r
                   inner-attribs]]]
              (let [tooltip (:tooltip inner-attribs)
                    circle  (svg/circle [plot-x, plot-y]
                                        (if (nil? r)
                                          (/ scale
                                             3.0)
                                          r)
                                        (merge attribs ;; point-specific `:attribs` overwrite
                                               inner-attribs))]
                (if (nil? tooltip)
                  circle
                  (svg/group {}
                             circle
                             (svg-title tooltip)))))
    :layout viz/svg-scatter-plot}])

(defn-
  process-points-less
  "Based on `process-points` from `geom/viz`
  Just disables the sorting"
  [{:keys [x-axis y-axis project]}
   {:keys [values item-pos shape]}]
  (let [[ry1
         ry2] (:range y-axis)]
    (->> values ;;disable sorting
         #_
         (if item-pos
           (sort-by (comp first item-pos) values)
           (sort-by first values))
         (sequence (viz/value-transducer {:cull-domain (:domain x-axis)
                                          :cull-range  (if (< ry1
                                                              ry2)
                                                         [ry1, ry2]
                                                         [ry2, ry1])
                                          :item-pos    item-pos
                                          :scale-x     (:scale x-axis)
                                          :scale-y     (:scale y-axis)
                                          :project     project
                                          :shape       shape})))))

(defn-
  svg-trueline-plot
  "Based on `svg-line-plot` from `geom/viz`
  Normal `svg-line-plot` seems to sort the [x,y] point by
  their x coord. So it can only plot 1-to-1 y=f(x) functions"
  [v-spec
   d-spec]
  (svg/line-strip (map first
                       (process-points-less v-spec
                                            d-spec))
                  (:attribs d-spec)))

(defn
  dashed-line
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  [{:values  data
    :attribs (merge {:stroke-dasharray (str (/ scale
                                               10.0)
                                            " "
                                            (/ scale
                                               10.0))
                     :stroke-width     (/ scale
                                          10.0)
                     :stroke           "#aaa"}
                    attribs)
    :layout  svg-trueline-plot}])

(defn
  polyline
  "Given a vector of polunomial factors
  Draw a line
  TODO: For now only supports y = ax + b
  But leaving room to expand this to more polynomials
  when i have time.."
  [reference-xy-data
   polynomial-factors-vec
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  (if (or (nil? reference-xy-data)
          (nil? polynomial-factors-vec)
          (empty? polynomial-factors-vec)
          (empty? reference-xy-data) ;;#_#_
          (->> polynomial-factors-vec
               (not-every? some?))
          (->> reference-xy-data
               (not-every? some?)))
    []
    (let [[b a] polynomial-factors-vec
          xs    (->> reference-xy-data
                     (mapv first))
          ys    (->> reference-xy-data
                     (mapv second))]
      (let [x-min (apply min
                         xs)
            x-max (apply max
                         xs)
            y-min (apply min
                         ys)
            y-max (apply max
                         ys)]
        (let [y4x-min (+ (* a
                            x-min)
                         b)
              y4x-max (+ (* a
                            x-max)
                         b)]
          (let [two-end-points [(if (> y4x-min
                                       y-min)
                                  [x-min
                                   y4x-min]
                                  [(/ (- y-min
                                         b)
                                      a)
                                   y-min])
                                (if (< y4x-max
                                       y-max)
                                  [x-max
                                   y4x-max]
                                  [(/ (- y-max
                                         b)
                                      a)
                                   y-max])]]
            [{:values  two-end-points
              :attribs (merge {:stroke-width (/ scale
                                                10.0)
                               :stroke       "#aaa"}
                              attribs)
              :layout  svg-trueline-plot}]))))))
#_
(polyline [[0 0] [9000 100]] [10.0 1.0])

(defn
  error-bars
  "Error bars are drawn on x y values
  Error can be specified with single values or double (for asymmetric bars)
  [x, y, x-err, y-err]
  [x, y, [x-err-neg, x-err-pos], [y-err-neg, y-err-pos]]
  Where the sigle `x-err` is a one sided error
  NOTE: Maybe doesn't work correctly on logarithmic plots?
  "
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}
       :as   options}]]
  (->> data
       (map (fn [[x, y, x-err, y-err]]
              (into (adjustable-circles (->> data
                                             (mapv #(take 2
                                                          %)))
                                        (-> options
                                            (assoc-in [:attribs
                                                       :fill]
                                                      "black")
                                            (assoc :scale
                                                    (/ scale
                                                       10.0))))
                    (into (if (nil? x-err)
                            []
                            (let [x-start (- x
                                             (if (seq? x-err)
                                               (first x-err)
                                               x-err))
                                  x-end   (+ x
                                             (if (seq? x-err)
                                               (second x-err)
                                               x-err))]
                              [{:values  [[x-start, y]
                                          [x-end, y]]
                                :attribs (merge {#_#_:stroke-dasharray (str (/ scale
                                                                               10.0)
                                                                            " "
                                                                            (/ scale
                                                                               10.0))
                                                 :stroke-width         (/ scale
                                                                          50.0)
                                                 :stroke               "black"}
                                                attribs)
                                :layout  svg-trueline-plot}]))
                          (if (nil? y-err)
                            []
                            (let [y-start (- y
                                             (if (seq? y-err)
                                               (first y-err)
                                               y-err))
                                  y-end   (+ y
                                             (if (seq? y-err)
                                               (second y-err)
                                               y-err))]
                              [{:values  [[x, y-start]
                                          [x, y-end]]
                                :attribs (merge {#_#_:stroke-dasharray (str (/ scale
                                                                               10.0)
                                                                            " "
                                                                            (/ scale
                                                                               10.0))
                                                 :stroke-width         (/ scale
                                                                          50.0)
                                                 :stroke               "black"}
                                                attribs)
                                :layout  svg-trueline-plot}]))))))))
#_
(error-bars [[0 1 1 1]
             [1 1 1 1]
             [2 3 2 2]])
;; => ([{:values [[-1 1] [1 1]],
;;       :attribs
;;       {:stroke-dasharray "3.6 3.6", :stroke-width 3.6, :stroke "#aaa"},
;;       :layout #function[quickthing/svg-trueline-plot]}
;;      {:values [[0 0] [0 2]],
;;       :attribs
;;       {:stroke-dasharray "3.6 3.6", :stroke-width 3.6, :stroke "#aaa"},
;;       :layout #function[quickthing/svg-trueline-plot]}]
;;     [{:values [[0 1] [2 1]],
;;       :attribs
;;       {:stroke-dasharray "3.6 3.6", :stroke-width 3.6, :stroke "#aaa"},
;;       :layout #function[quickthing/svg-trueline-plot]}
;;      {:values [[1 0] [1 2]],
;;       :attribs
;;       {:stroke-dasharray "3.6 3.6", :stroke-width 3.6, :stroke "#aaa"},
;;       :layout #function[quickthing/svg-trueline-plot]}]
;;     [{:values [[0 3] [4 3]],
;;       :attribs
;;       {:stroke-dasharray "3.6 3.6", :stroke-width 3.6, :stroke "#aaa"},
;;       :layout #function[quickthing/svg-trueline-plot]}
;;      {:values [[2 1] [2 5]],
;;       :attribs
;;       {:stroke-dasharray "3.6 3.6", :stroke-width 3.6, :stroke "#aaa"},
;;       :layout #function[quickthing/svg-trueline-plot]}])

#_
(defn
  events-2-rates
  [event-size
   events]
  (->> events
       set
       (into [])
       sort
       (partition 2
                  1)
       (mapcat #(let [start (first %)
                      end   (second %)]
                  (let [event-time (- (second %)
                                      (first %))
                        rate       (/ event-size
                                      event-time)]
                    [[start, rate]
                     [end,   rate]])))
       (into [])))
#_
(events-2-rates 100
                [1, 2, 4, 5, 6])

(defn
  vector2d
  "Draw a vector"
  [[x
    y]
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  [{:values  [[0.0, 0.0]
              [x, y]]
    :attribs (merge {:stroke-width (/ scale
                                      10.0)
                     :stroke       "#aaa"}
                    attribs)
    :layout  svg-trueline-plot}])
#_
(vector2d [1.0
           1.0])
#_
(->> (-> (quickthing/zero-axis [[ 2.0,  2.0]
                                [-2.0, -2.0]])
         (assoc :data
                (vector2d [1.0, 1.0]))
         (viz/svg-plot2d-cartesian)
         svg-wrap)
     svg2xml
     (spit "./out/test-vector.svg"))

(defn-
  stretch-vector2d-to-data
  "Draw a vector [VEC-X VEC-Y]
  and stretch it so it's in the span of the 2d data given"
  [data
   [vec-x, vec-y]]
  (let [x-max (->> data
                   (mapv first)
                   (into [0])
                   (apply max))
        x-min (->> data
                   (mapv first)
                   (into [0])
                   (apply min))
        y-max (->> data
                   (mapv second)
                   (into [0])
                   (apply max))
        y-min (->> data
                   (mapv second)
                   (into [0])
                   (apply min))]
    (cond (and (pos? vec-x)
               (or (neg? x-max)
                   (zero? x-max))) [0.0, 0.0]
          (and (pos? vec-y)
               (or (zero? y-max)
                   (neg? y-max)))  [0.0, 0.0]
          (and (neg? vec-x)
               (or (zero? x-min)
                   (pos? x-min)))  [0.0, 0.0]
          (and (neg? vec-y)
               (or (zero? y-min)
                   (pos? y-min)))  [0.0, 0.0]
          :else                    (if (zero? vec-y) ;; degenerate case
                                     (if (pos? vec-x)
                                       [x-max
                                        0]
                                       [x-min
                                        0])
                                     (let[vec-ratio  (/ vec-x
                                                        vec-y)
                                          data-ratio (/ (if (pos? vec-x)
                                                          x-max
                                                          x-min)
                                                        (if (pos? vec-y)
                                                          y-max
                                                          y-min))]
                                       (if (> (abs vec-ratio)
                                              (abs data-ratio)) ;; vector intersects the left/right side
                                         (let [x-bound (if (pos? vec-x)
                                                         x-max
                                                         x-min)]
                                           [x-bound
                                            (/ x-bound
                                               vec-ratio)])
                                         (let [y-bound (if (pos? vec-y)
                                                         y-max
                                                         y-min)]
                                           [(* y-bound
                                               vec-ratio)
                                            y-bound])))))))
#_
(line-through-point [[2,  1.5]
                     [1, -2]
                     [-1, 1]
                     [-1, -2]]
                    [0.3 0.2])
#_
(defn-
  angle-to-unitvector
  "Given an angle in radians
  Return a 2D vector of length 1"
  [angle]
  [(cos angle)
   (sin angle)])
#_
(-> [[1,  1]
     [1, -2]
     [-1, 1]
     [-1, -2]]
    angle-dichotomies
    first ;; => 0.9462734405957693
    angle-to-unitvector) ;; => [0.584710284663765 0.8112421851755608]

#_
(let [lil-vec (-> (* clojure.math/PI
                     1.9)             ;; PLAY WITH VALUE TO TURN
                  angle-to-unitvector)
      data    [[2,  1.5]
               [1, -2]
               [-1.2, 1]
               [-1, -2]]]
  (let [fit-vec (->> lil-vec
                     (fit-vect2d-to-data-span data))]
    (->> (-> (quickthing/zero-axis data)
             (assoc :data
                    (into [(quickthing/adjustable-circles data)]
                          (quickthing/vector2d fit-vec))))
         thi.ng.geom.viz.core/svg-plot2d-cartesian
         quickthing/svg-wrap
         quickthing/serialize
         (spit "out/test-dots.svg"))
    (->> fit-vec ;; sanity check
         reverse
         (apply /)
         atan)))

(defn
  line-through-point
  "Draw a vector/line that goes through the point POINT2D (ie. [VEC-X VEC-Y])
  as well as the origin [0, 0]
  The line is fitted to the span of the data (so should always display)
  Made out of 2 `vector2d` draws
  Optionally pass a `:attribs` to style the line"
  [data
   point2d
   & [{:keys [attribs]}]]
  (into (quickthing/vector2d (->> point2d
                                  (stretch-vector2d-to-data data))
                             {:attribs attribs})
        (quickthing/vector2d (->> point2d
                                  (map -)
                                  (stretch-vector2d-to-data data))
                             {:attribs attribs})))
#_
(let [lil-vec (-> (* clojure.math/PI
                     1.6)             ;; PLAY WITH VALUE TO TURN
                  angle-to-unitvector)
      data    [[2,  1.5]
               [1, -2]
               [-1.2, 1]
               [-1, -2]]]
  (->> (-> (quickthing/zero-axis data)
           (assoc :data
                  (into [(quickthing/adjustable-circles data)]
                        (line-through-point data
                                            lil-vec
                                            {:attribs {:stroke-dasharray (str 4.0
                                                                              " "
                                                                              4.0)}})))
           thi.ng.geom.viz.core/svg-plot2d-cartesian
           quickthing/svg-wrap
           quickthing/serialize)
       (spit "out/test-dots.svg")))

(defn
  index-text
  "Calls `adjustable-text` but inserts the index automatically"
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  (adjustable-text (map-indexed #(conj (into []
                                             (take 2
                                                   %2))
                                       %1)
                                data)
                   {:attribs attribs
                    :scale   scale}))

;; see: https://github.com/thi-ng/color/issues/10
(prefer-method clojure.pprint/simple-dispatch
               clojure.lang.IPersistentMap
               clojure.lang.IDeref)

;; color maps are from
;; https://colorcet.com/download/index.html
;; and are under the Creative Commons BY License
(def
  cyclic-colors
  (->> "colorcet/CET-C2.csv"
       io/resource ;; loads from local classpath
       slurp
       csv/read-csv
       (map (fn [color]
              (map #(Double/parseDouble %)
                   color)))
       (mapv #(apply col/rgba
                     %))))
#_
(identity cyclic-colors)

(defn
  color-cycle
  "Colors are selected cyclically. 1.0 is one period
  For instance 34.56 will get the 0.56th part of the cycle
  (ie. a little past the middle)
  If `nil` return `black`"
  [value]
  (if (nil? value)
    (col/rgba 1 1 1)
    (get cyclic-colors
         (-> value
             double
             (+ 0.25)
             (rem 1)    ; get the number past the decimal
             (-)
             (+ 1)
             (* 255)    ; get a value on the 0-255 range
             (Math/abs)
             (Math/round))))) ; get an integer value
#_
(let [data   [[0.0, 0.0]
              [0.1, 0.1]
              [0.2, 0.2]
              [0.3, 0.3]
              [0.4, 0.4]
              [0.5, 0.5]
              [0.6, 0.6]
              [0.7, 0.7]
              [0.8, 0.8]
              [0.9, 0.9]
              [1.0, 1.0]]
      width  default-width
      height default-height]
  (->> (-> (quickthing/zero-axis data
                                 1000
                                 1000)
           (assoc :data
                  (adjustable-circles (map-indexed (fn [index
                                                        data-point]
                                                     (conj data-point
                                                           nil ;; default radius
                                                           {:fill   (color-cycle (first data-point))
                                                            :stroke "#777"}))
                                                   data))))
       (viz/svg-plot2d-cartesian)  ;; turns the plot to svg hiccup
       (svg/svg {:width  width
                 :height height})
       (svg/serialize)             ;; turns it to XML
       (spit "debug/color-cycle.svg")))

(def
  red-blue-colors
  (->> "colorcet/CET-D01.csv"
       io/resource ;; loads from local classpath
       slurp
       csv/read-csv
       (map
         (fn [color]
           (map #(Double/parseDouble
                   %)
                color)))
       (mapv #(apply col/rgba
                     %))))

(defn
  group-plots-row
  "Take a vector of plots and groups them into a row.
      For now it must take a `widths` parameter.
      There is no way to determine the width from the svg elements.
      Hence all plots are the same width"
  [row-of-plots]
  (let [max-height-in-row (->> row-of-plots
                               (map (fn [plot]
                                      (-> plot
                                          (get 1)
                                          :height)))
                               (apply max))
        [total-offset
         adjusted-plots]  (reduce (fn [[horizontal-offset
                                        horizontal-plots]
                                       next-plot]
                                    (println horizontal-offset)
                                    (let [next-plot-width (-> next-plot
                                                              (get 1)
                                                              :width)
                                          next-offset     (+ horizontal-offset
                                                             next-plot-width)]
                                      [next-offset
                                       (conj horizontal-plots 
                                             (svg/group {:transform (geom/translate matrix/M32
                                                                                    [horizontal-offset, 0])}
                                                        next-plot))]))
                                  [0
                                   []]
                                  row-of-plots)]
    [total-offset
     max-height-in-row
     (apply svg/group
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
         adjusted-row-groups] (reduce (fn [[vertical-offset
                                            horizontal-offsets
                                            vertical-plot-rows]
                                           row-of-plots]
                                        (let [[horizontal-offset
                                               max-height-in-row
                                               concatenated-row] (group-plots-row row-of-plots)]
                                          [(+ vertical-offset
                                              max-height-in-row)
                                           (conj horizontal-offsets
                                                 horizontal-offset)
                                           (conj vertical-plot-rows
                                                 (svg/group {:transform (geom/translate matrix/M32
                                                                                        [0, vertical-offset])}
                                                            concatenated-row))]))
                                      [0 ;; initial for `reduce`
                                       []
                                       []]
                                      plots)
        total-width           (apply max
                                     final-horizontal-offsets)]
    (->> (apply svg/group
                {}
                adjusted-row-groups)
         (svg/svg {:width   total-width
                   :height  total-height
                   :viewBox (str "0 0 "
                                 total-width
                                 " "
                                 total-height)}))))

(defn
  hist
  [data
   & [{:keys [attribs]}]]
  [{:values  data
    :attribs (merge {:fill         "none"
                     :stroke-width 10
                     :stroke       "#ffb2b0"}
                    attribs)
    :layout  viz/svg-bar-plot}])
#_
(let [width     1000
      height    500
      fake-data (map (fn [i]
                       [i
                        (rand 100)])
                     (range 2000
                            2016))
      axis      (-> fake-data
                    (quickthing/primary-axis {:x-name "YEAR"
                                              :y-name "RAND"
                                              :title  "TEST-PLOT"
                                              :color  "#0008"})
                    (assoc-in [:x-axis
                               :major]
                              (range 2000
                                     2016)))]
  (spit "out/test-hist.svg"
        (-> axis
            (assoc :data
                   (quickthing/hist fake-data))
            viz/svg-plot2d-cartesian
            (quickthing/svg-wrap [width
                                  height])
            quickthing/svg2xml)))

(defn
  histogram
  "DEPRECATED
  `geom-viz` bar graphs assume only positive values
  To plot negative values you need to generate two bar graphs
  And then have them glued together"
  ([data
    width
    height
    left-margin]
   (histogram data
              width
              height
              left-margin
              false))                 ;; default not inverted
  ([data
    width
    height
    left-margin
    inverted?]
   (let [xs    (->> data
                    (map first))
         ys    (->> data
                    (map second))
         min-x (apply min
                      xs)
         max-x (apply max
                      xs)
         min-y (apply min
                      ys)
         max-y (apply max
                      ys)]
     (histogram data
                width
                height
                left-margin
                inverted?
                [(- min-x
                    0.5)
                 (+ max-x
                    0.5)
                 (if (pos? min-y)
                   -0.5
                   (- min-y
                      0.5))
                 (if (neg? max-y)
                   0.5
                   (+ max-y
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
   (let [axis (viz/svg-plot2d-cartesian {:x-axis (viz/linear-axis {:domain      [min-x, max-x]
	                                                           :range       [left-margin, width]
	                                                           :minor       0
	                                                           :label-style {:fill "none"}
	                                                           :pos         (* height
                                                                                   (/ (if inverted?
                                                                                        (Math/abs min-y)
                                                                                        max-y)
                                                                                      (+ max-y
                                                                                         (Math/abs min-y))))})
                                         :y-axis (viz/linear-axis {:domain [min-y, max-y]
	                                                           :range  (if inverted?
                                                                             [0, height]
                                                                             [height, 0])
	                                                           ;; :major  1
	                                                           ;; :minor  1
	                                                           :pos    left-margin})
                                         :grid   {:minor-x true
                                                  :minor-y false}
                                         :data   []})
         bars (viz/svg-plot2d-cartesian {:x-axis (viz/linear-axis {:domain  [min-x, max-x]
	                                                           :range   [left-margin, width]
                                                                   :visible true
	                                                           :pos     (* height
                                                                               (/ (if inverted?
                                                                                    (Math/abs min-y)
                                                                                    max-y)
                                                                                  (+ max-y
                                                                                     (Math/abs min-y))))})
                                         :y-axis (viz/linear-axis
                                                   {:domain  [0
                                                              (if inverted?
                                                                min-y
                                                                max-y)]
	                                            :range   [(* height
                                                                 (/ (if inverted?
                                                                      (Math/abs min-y)
                                                                      max-y)
                                                                    (+ max-y
                                                                       (Math/abs min-y))))
                                                              0]
                                                    :visible true
	                                            :pos     left-margin})
                                         :grid   {:minor-x false
                                                  :minor-y false}
                                         :data   [{:values  data
                                                   :attribs {:fill         "none"
                                                             :stroke-width (* 0.75
                                                                              (/ (- width
                                                                                    left-margin)
                                                                                 max-x))
                                                             :stroke       "#ffb2b0"}
                                                   :layout  viz/svg-bar-plot}]})]
     (svg/group {}
                axis
                bars))))
#_
(->>
  (histogram [[0, 1]
              [1, 2]
              [2, 3]
              [3, 4]
              [4, 5]]
             1000
             400
             40
             #_
             [-0.5, 4.5, 0, 6])
  (svg/svg {:width  1000
            :height 400})
  serialize-with-line-breaks
  (spit "out/hist-test.svg"))
