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
  labels
  "add a customizable labels display
  `data` is a vector of [x,y,attribs]
  `attribs`"
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
                   inner-attribs]]]
              (let [tooltip      (:tooltip inner-attribs)
                    text-element (svg/text [plot-x, plot-y]
                                           (str (:text inner-attribs))
                                           (merge {:fill              "#000"
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
              legend
              color
              title]
       :or   {width       default-width
              height      default-height
              margin-frac nil
              scale       36
              color       "#000f"}}]]
  (let [margin-frac (if (nil? margin-frac)
                      (/ scale
                         400.0)
                      margin-frac)
        xs          (->> data
                         (map first))
        ys          (->> data
                         (map second))
        x-min       (let [min (apply min
                                     xs)]
                      (if (pos? min)
                        0.0
                        min))
        x-max       (let [max (apply max
                                     xs)]
                      (if (neg? max)
                        0.0
                        max))
        y-min       (let [min (apply min
                                     ys)]
                      (if (pos? min)
                        0.0
                        min))
        y-max       (let [max (apply  max
                                      ys)]
                      (if (neg? max)
                        0.0
                        max))
        x-range     (max (Math/abs x-max)
                         (Math/abs x-min))
        y-range     (max (Math/abs y-max)
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
                                  Math/floor
                                  (Math/pow 10)
                                  tick-formatter
                                  viz/default-svg-label)
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
                :attribs     {:stroke       color
                              :stroke-width (/ scale
                                               10)}
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
                                  Math/floor
                                  (Math/pow 10))
                :label       (->> x-range
                                  Math/log10
                                  Math/floor
                                  (Math/pow 10)
                                  tick-formatter
                                  viz/default-svg-label)
                :pos         (let [data-width (/ width
                                                 (+ 1.0
                                                    (* 2.0
                                                       margin-frac)))
                                   offset     (* margin-frac
                                                 width)]
                               (if (zero? x-max)
                                 (- width
                                    (* 2.0
                                       offset))
                                 (if (zero? x-min)
                                   offset
                                   (/ width
                                      2.0))))
                :attribs     {:stroke       color
                              :stroke-width (/ scale
                                               10)}
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
               (some? title)
               (into (labels [[x-max
                               y-max
                               {:text              title
                                :dx                (- (/ scale
                                                         1.0))
                                :dy                (/ scale
                                                      1.0)
                                :font-size         scale
                                :font-family       "Arial, sans-serif"
                                :fill              "#0004"
                                :stroke-width      (/ scale
                                                      30.0)
                                :text-anchor       "end"
                                :dominant-baseline "hanging"}]]))
               (some? legend)
               (into (->> legend
                          (map-indexed (fn add-legend-item
                                         [row
                                          [legend-text
                                           legend-attribs]]
                                         (labels [[(+ x-min
                                                      (* x-range
                                                         0.001))
                                                   (- y-max
                                                      (* y-range
                                                         0.0001))
                                                   (merge {:text              legend-text
                                                           :dx                (/ scale
                                                                                 2.0)
                                                           :dy                (+ (* scale
                                                                                    1.5)
                                                                                 (*  row
                                                                                     (/ scale
                                                                                        1.5)))
                                                           ;;:text-orientation  "sideways"
                                                           :font-size         (/ scale
                                                                                 2.0)
                                                           :fill              color
                                                           :stroke-width      (/ scale
                                                                                 100.0)
                                                           :stroke            "#000f"
                                                           #_#_:font-family
                                                           "Arial, sans-serif"
                                                           :font-style        "italic"
                                                           :text-anchor       "beginning"
                                                           :dominant-baseline "hanging"}
                                                          legend-attribs)]])))
                          (mapcat identity)
                          (into [])))
               (some? x-name)
               (into (labels [[(+ x-min
                                  (/ x-range
                                     2.0))
                               (* y-max
                                  0.999)
                               {:text              x-name
                                :dy                (/ scale
                                                      -4.0)
                                :font-size         (/ scale
                                                      2.0)
                                :font-family       "Arial, sans-serif"
                                :font-style        "italic"
                                :fill              color
                                :text-anchor       "middle"
                                :dominant-baseline "text-bottom"}]]))
               (some? y-name)
               (into (labels [[x-min
                               (* y-max
                                  0.999)
                               {:text              y-name
                                :dx                (/ scale
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
                                :dominant-baseline "hanging"}]])))}))

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
              legend
              x-ticks
              y-ticks
              main-color
              color
              title]
       :or   {width            default-width
              height           default-height
              logarithmic-y?   false
              margin-frac      nil
              y-breathing-room 0.1
              scale            36
              main-color       "black"
              color            "black"}}]]
  (let [margin-frac  (if (nil? margin-frac)
                       (/ scale
                          400.0)
                       margin-frac)
        y-axis-func  (if logarithmic-y?
                       viz/log-axis
                       viz/linear-axis)
        xs           (->> data
                          (map first))
        ys           (->> data
                          (map second))
        x-min        (apply min
                            xs)
        x-max        (apply max
                            xs)
        y-min        (apply min
                            ys)
        y-max        (apply max
                            ys)
        x-range      (- x-max
                        x-min)
        y-range      (- y-max
                        y-min)
        y-full-max   (+ y-max
                        (* y-range
                           y-breathing-room))
        y-full-min   (let [y-min-with-buffer (- y-min
                                                (* y-range
                                                   y-breathing-room))]
                       ;; don't add bottom buffer space if you're around zero
                       (if logarithmic-y?
                         y-min
                         (if (and (>= y-min
                                      0)
                                  (neg? y-min-with-buffer))
                           0.0
                           y-min-with-buffer)))
        y-full-range (- y-full-max
                        y-full-min)]
    {:x-axis  (viz/linear-axis {:domain      [x-min, x-max]
                                :range       [(* margin-frac
                                                 width)
                                              (- width
                                                 (* margin-frac
                                                    width))]
                                :major       (->> x-range
                                                  Math/log10
                                                  Math/floor
                                                  (Math/pow 10))
                                :label       (->> x-range
                                                  Math/log10
                                                  Math/floor
                                                  (Math/pow 10)
                                                  tick-formatter
                                                  viz/default-svg-label)
                                :pos         (- height
                                                (* margin-frac
                                                   height))
                                :attribs     {:stroke color}
                                :label-dist  (/ scale
                                                1.75)
                                #_#_
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
     :y-axis  (y-axis-func {:domain      [y-full-min
                                          y-full-max]
                            :range       [(- height
                                             (* margin-frac
                                                height))
                                          (* margin-frac
                                             height)]
                            :major       (->> y-range
                                              Math/log10
                                              Math/floor
                                              (Math/pow 10))
                            :label       (->> y-range
                                              Math/log10
                                              Math/floor
                                              (Math/pow 10)
                                              tick-formatter
                                              viz/default-svg-label)
                            :pos         (* margin-frac
                                            width)
                            :attribs     {:stroke color}
                            :major-size  (/ scale
                                            3.0)
                            :label-dist  (/ scale
                                            4.0)
                            :label-y     (/ scale
                                            -4.0)
                            :label-style {:fill        color
                                          :stroke      "none"
                                          :font-family "Arial, sans-serif"
                                          :font-size   (/ scale
                                                          2.0)
                                          :text-anchor "end"}})
     :attribs {:stroke color
               :fill   color}
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
                (some? title)
                (into (labels [[x-max
                                y-max
                                {:text              title
                                 :dx                (- (/ scale
                                                          1.0))
                                 :dy                (/ scale
                                                       1.0)
                                 :font-size         scale
                                 :font-family       "Arial, sans-serif"
                                 :fill              "#0004"
                                 :stroke-width      (/ scale
                                                       30.0)
                                 :text-anchor       "end"
                                 :dominant-baseline "hanging"}]]))
                (some? legend)
                (into (->> legend
                           (map-indexed (fn add-legend-item
                                          [row
                                           [legend-text
                                            legend-attribs]]
                                          (labels [[(+ x-min
                                                       (* x-range
                                                          0.001))
                                                    (- y-full-max
                                                       (* y-full-range
                                                          0.0001))
                                                    (merge {:text              legend-text
                                                            :dx                (/ scale
                                                                                  2.0)
                                                            :dy                (+ (* scale
                                                                                     1.5)
                                                                                  (*  row
                                                                                      (/ scale
                                                                                         1.5)))
                                                            ;;:text-orientation  "sideways"
                                                            :font-size         (/ scale
                                                                                  2.0)
                                                            :fill              color
                                                            :stroke-width      (/ scale
                                                                                  100.0)
                                                            :stroke            "#000f"
                                                            #_#_:font-family
                                                            "Arial, sans-serif"
                                                            :font-style        "italic"
                                                            :text-anchor       "beginning"
                                                            :dominant-baseline "hanging"}
                                                           legend-attribs)]])))
                           (mapcat identity)
                           (into [])))
                (some? x-name)
                (into (labels [[(+ x-min
                                   (/ x-range
                                      2.0))
                                (* y-full-max
                                   0.999)
                                {:text              x-name
                                 :dy                (/ scale
                                                       -4.0)
                                 :font-size         (/ scale
                                                       2.0)
                                 :font-family       "Arial, sans-serif"
                                 :font-style        "italic"
                                 :fill              main-color
                                 :text-anchor       "middle"
                                 :dominant-baseline "text-bottom"}]]))
                (some? y-name)
                (into (labels [[x-min
                                (* y-full-max
                                   0.999)
                                {:text              y-name
                                 :dx                (/ scale
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
                (some? y-name) (into (quickthing/text [[x-max
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
        xs          (->> data
                         (map first))
        ys          (->> data
                         (map second))
        x-min       (apply min
                           xs)
        x-max       (apply max
                           xs)
        y-min       (apply min
                           ys)
        y-max       (apply max
                           ys)
        x-range     (- x-max
                       x-min)
        y-range     (- y-max
                       y-min)
        y-full-max  (+ y-max
                       (* y-range
                          y-breathing-room))
        y-full-min  (let [y-min-with-buffer (- y-min
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
                                :attribs     {:stroke color}
                                :label-style {:fill        color
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
                                                  Math/floor
                                                  (Math/pow 10))
                                :label       (->> y-range
                                                  Math/log10
                                                  long
                                                  dec
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
                                :label-style {:fill        color
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
                (some? title)  (into (quickthing/labels [[x-max
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
                (some? x-name) (into (quickthing/labels [[(+ x-min
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
                (some? y-name) (into (quickthing/labels [[x-max
                                                          y-full-max
                                                          {:text              y-name
                                                           :dx                (/ scale
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
                (some? y-name) (into (quickthing/labels [[x-max
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
               (some? y-name) (into (quickthing/labels [[x-max
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
               (some? x-name) (into (quickthing/labels [[(/ x-range
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
  no-axis
  [data
   & options]
  (-> data
      (primary-axis #_ (-> options
                           (assoc :margin-frac
                                  0)))
      (assoc-in [:x-axis
                 :visible]
                false)
      (assoc-in [:y-axis
                 :visible]
                false)
      (assoc-in [:grid]
                false)
      (assoc-in [:grid]
                false)))

(defn
  circles
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
                   {:keys [radius]
                    :as   inner-attribs}]]]
              (let [radius (if (nil? radius)
                             (/ scale
                                3.0)
                             radius)]
                (let [tooltip (:tooltip inner-attribs)
                      circle  (svg/circle [plot-x, plot-y]
                                          (if (nil? radius)
                                            (/ scale
                                               3.0)
                                            radius)
                                          (merge {:stroke-width (/ radius
                                                                   10.0)
                                                  :stroke       "#000f"}
                                                 attribs ;; point-specific `:attribs` overwrite
                                                 inner-attribs))]
                  (if (nil? tooltip)
                    circle
                    (svg/group {}
                               circle
                               (svg-title tooltip))))))
    :layout viz/svg-scatter-plot}])

;; DOESN"T QUITE WORK: b/c the `err-x` and `err-y` are in SVG coordinates.
;; They aren't in the plotting data coordinates
#_
(defn
  adjustable-ellipses
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
                   err-x
                   err-y
                   inner-attribs]]]
              (let [tooltip (:tooltip inner-attribs)
                    circle  (svg/ellipse [plot-x, plot-y]
                                         err-x
                                         err-y
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
         (sequence (viz/value-transducer {:cull-domain nil #_ (:domain x-axis)
                                          :cull-range  nil #_ (if (< ry1
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
  solid-line
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  [{:values  data
    :attribs (merge {#_#_:stroke-dasharray (str (/ scale
                                                   10.0)
                                                " "
                                                (/ scale
                                                   10.0))
                     :stroke-width         (/ scale
                                              10.0)
                     :stroke               "#aaa"}
                    attribs)
    :layout  svg-trueline-plot}])

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
;; => [{:values [[0 10.0] [90.0 100]],
;;      :attribs {:stroke-width 3.6, :stroke "#aaa"},
;;      :layout #function[quickthing/svg-trueline-plot]}]

(defn
  error-bars
  "Error bars are drawn on x y values
  [x
   y
  {:err-x
   :err-y}]
  Either `x-err` or `y-err` can be null
  NOTE: Has not been tested on logarithmic plots.
  But should work..
  "
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}
       :as   options}]]
  (->> data
       (mapv (fn [[x
                   y
                   {:keys [err-x
                           err-y]}]]
               (into [] #_(circles (->> data
                                        (mapv #(take 2
                                                     %)))
                                   (-> options
                                       (assoc-in [:attribs
                                                  :fill]
                                                 "transparent")
                                       (assoc :scale
                                              (/ scale
                                                 10.0))))
                     (into (if (nil? err-x)
                             []
                             (let [x-start (- x
                                              err-x)
                                   x-end   (+ x
                                              err-x)]
                               [{:values  [[x-start, y]
                                           [x-end, y]]
                                 :attribs (merge {:stroke-width (/ scale
                                                                   50.0)
                                                  :stroke       "black"}
                                                 attribs)
                                 :layout  svg-trueline-plot}]))
                           (if (nil? err-y)
                             []
                             (let [y-start (- y
                                              err-y)
                                   y-end   (+ y
                                              err-y)]
                               [{:values  [[x, y-start]
                                           [x, y-end]]
                                 :attribs (merge {:stroke-width (/ scale
                                                                   50.0)
                                                  :stroke       "black"}
                                                 attribs)
                                 :layout  svg-trueline-plot}]))))))
       flatten
       vec))
#_
(error-bars [[0 1 {
                   :err-y 2}]])
;; => [{:values [(0 1)],
;;      :shape #function[quickthing/circles/fn--51470],
;;      :layout #function[thi.ng.geom.viz.core/svg-scatter-plot]}
;;     {:values [[0 -1] [0 3]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}]#_
(error-bars [[0 1 {:err-x 1
                   :err-y 2}]
             [1 1  {:err-x 1
                    :err-y 1}]
             [2 3  {:err-x 2
                    :err-y 2}]])
;; => [{:values [(0 1) (1 1) (2 3)],
;;      :shape #function[quickthing/adjustable-circles/fn--50461],
;;      :layout #function[thi.ng.geom.viz.core/svg-scatter-plot]}
;;     {:values [[-1 1] [1 1]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values [[0 -1] [0 3]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values [(0 1) (1 1) (2 3)],
;;      :shape #function[quickthing/adjustable-circles/fn--50461],
;;      :layout #function[thi.ng.geom.viz.core/svg-scatter-plot]}
;;     {:values [[0 1] [2 1]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values [[1 0] [1 2]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values [(0 1) (1 1) (2 3)],
;;      :shape #function[quickthing/adjustable-circles/fn--50461],
;;      :layout #function[thi.ng.geom.viz.core/svg-scatter-plot]}
;;     {:values [[0 3] [4 3]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values [[2 1] [2 5]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}]


(defn-
  ellipse-radius-at-angle
  "Radius of an ellipse at a given angle away from the major-axis"
  [angle-from-major-axis-rad
   semi-major-axis-length
   semi-minor-axis-length]
  (/ (* 1.0
        semi-major-axis-length
        semi-minor-axis-length)
     (sqrt (+ (pow (* semi-minor-axis-length
                      (cos angle-from-major-axis-rad))
                   2.0)
              (pow (* semi-major-axis-length
                      (sin angle-from-major-axis-rad))
                   2.0)))))
#_
(ellipse-radius-at-angle (/ PI
                            4.0)
                         10.0
                         2.0)
;; => 1.2649110640673518

(defn-
  angle-of-orthogonal
  [point-x
   point-y]
  (mod (+ (/ PI
             2.0)
          (if (zero? point-x)
            (/ PI
               2.0)
            (atan (/ point-y
                     point-x))))
       PI))
#_
(-> (angle-of-orthogonal 0.0
                         1.0)
    to-degrees)
#_
(-> (angle-of-orthogonal 1.0
                         1.0)
    to-degrees)
;; => 135.0
#_
(-> (angle-of-orthogonal -1.0
                         1.0)
    to-degrees)
;; => 45.0
#_
(-> (angle-of-orthogonal -1.0
                         -1.0)
    to-degrees)
;; => 135.0

#_
(-> (angle-of-orthogonal 1.0
                         -1.0)
    to-degrees)
;; => 45.0

(defn
  orthogonal-error-length
  "Assuming orthogonal errors in X and Y,
  `err-x` `err-y` for point at `x` and `y`
  Calculate an angular error.
  Take the error ellipse..
  The diameter at an angle `u` is
  2ab/sqrt( (b cos(u))^2 + (a sin(u))^2 )
  Where
  `a` is the semi-major axis
  `b` is the semi-minor axis (ie. divided by 2)
  "
  [[cent-x
    cent-y
    {:keys [err-x
            err-y]}
    :as data-point]]
  (let [fat-ellipse?     (> err-x
                            err-y)
        orthogonal-angle (angle-of-orthogonal cent-x
                                              cent-y)]
    (let [semi-major (if fat-ellipse?
                       err-x
                       err-y)
          semi-minor (if fat-ellipse?
                       err-y
                       err-x)]
      (if fat-ellipse?
        (ellipse-radius-at-angle orthogonal-angle
                                 semi-major
                                 semi-minor)
        (ellipse-radius-at-angle (+ orthogonal-angle
                                    (/ PI
                                       2.0))
                                 semi-major
                                 semi-minor)))))
#_
(orthogonal-error-length [-2.0
                          1.0
                          {:err-x 10
                           :err-y 2}])
;; => 2.2249707974499238
#_
(orthogonal-error-length -2.0
                         1.0
                         2
                         10)
;; => Execution error (ArityException) at quickthing/eval111592 (REPL:1476).
;;    Wrong number of args (4) passed to: quickthing/orthogonal-error-length;; => 4.152273992686997;; => 2.7735009811261455

(defn-
  orthogonal-error-coords
  "The coordinated of the line illustrating the orthogonal error"
  [[cent-x
    cent-y
    {:keys [err-x
            err-y]}
    :as data-point]]
  (let [length    (orthogonal-error-length data-point)
        angle-rad (angle-of-orthogonal cent-x
                                       cent-y)]
    (let [x-shift (* length
                     (cos angle-rad))
          y-shift (* length
                     (sin angle-rad))]
      [[(- cent-x
           x-shift)
        (- cent-y
           y-shift)]
       [(+ cent-x
           x-shift)
        (+ cent-y
           y-shift)]])))
#_
(orthogonal-error-coords [-0.2
                          0.2
                          {:err-x 0.02
                           :err-y 0.01}])
;; => [[-0.20894427190999917 0.19105572809000085]
;;     [-0.19105572809000085 0.20894427190999917]]

(defn
  orthogonal-error-bars
  "Assume `x-err` and `y-err` are defined.
  Calculated the orthogonal error.
  This is thee error that forms the angular error of a point"
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}
       :as   options}]]
  (->> data
       (mapv (fn [[x
                   y
                   {:keys [err-x
                           err-y]}]]
               [{:values  (orthogonal-error-coords [x
                                                    y
                                                    {:err-x err-x
                                                     :err-y err-y}])
                 :attribs (merge {:stroke-width (/ scale
                                                   50.0)
                                  :stroke       "black"}
                                 attribs)
                 :layout  svg-trueline-plot}]))
       flatten
       vec))
#_
(orthogonal-error-bars [[0 1 {:err-x 1
                              :err-y 2}]
                        [1 1  {:err-x 1
                               :err-y 1}]
                        [2 3  {:err-x 2
                               :err-y 2}]])
;; => [{:values [[-1.0 1.0] [1.0 1.0]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values
;;      [[1.7071067811865475 0.2928932188134524]
;;       [0.29289321881345254 1.7071067811865475]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values
;;      [[3.664100588675687 1.8905996075495417]
;;       [0.33589941132431256 4.109400392450459]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}]

(defn
  parallel-error-length
  "Assuming orthogonal errors in X and Y,
  `err-x` `err-y` for point at `x` and `y`
  Calculate an angular error.
  Take the error ellipse..
  The diameter at an angle `u` is
  2ab/sqrt( (b cos(u))^2 + (a sin(u))^2 )
  Where
  `a` is the semi-major axis
  `b` is the semi-minor axis (ie. divided by 2)
  "
  [angle
   [cent-x
    cent-y
    {:keys [err-x
            err-y]}
    :as data-point]]
  (let [fat-ellipse?     (> err-x
                            err-y)]
    (let [semi-major (if fat-ellipse?
                       err-x
                       err-y)
          semi-minor (if fat-ellipse?
                       err-y
                       err-x)]
      (if fat-ellipse?
        (ellipse-radius-at-angle angle
                                 semi-major
                                 semi-minor)
        (ellipse-radius-at-angle (+ angle
                                    (/ PI
                                       2.0))
                                 semi-major
                                 semi-minor)))))

(defn-
  parallel-error-coords
  "The coordinated of the line illustrating the parallel error,
  parallel to the vector defined by `angle`"
  [angle
   [cent-x
    cent-y
    {:keys [err-x
            err-y]}
    :as data-point]]
  (let [length    (parallel-error-length angle
                                         data-point)]
    (let [x-shift (* length
                     (cos angle))
          y-shift (* length
                     (sin angle))]
      [[(- cent-x
           x-shift)
        (- cent-y
           y-shift)]
       [(+ cent-x
           x-shift)
        (+ cent-y
           y-shift)]])))

(defn
  parallel-error-bars
  "Assume `x-err` and `y-err` are defined.
  Calculated the error parallel to a vector,
  as defined by `angle`"
  [angle
   data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}
       :as   options}]]
  (->> data
       (mapv (fn [[x
                   y
                   {:keys [err-x
                           err-y]}]]
               [{:values  (parallel-error-coords angle
                                                 [x
                                                  y
                                                  {:err-x err-x
                                                   :err-y err-y}])
                 :attribs (merge {:stroke-width (/ scale
                                                   50.0)
                                  :stroke       "black"}
                                 attribs)
                 :layout  svg-trueline-plot}]))
       flatten
       vec))
#_
(parallel-error-bars 1.75
                     [[0 1 {:err-x 1
                              :err-y 2}]
                        [1 1  {:err-x 1
                               :err-y 1}]
                        [2 3  {:err-x 2
                               :err-y 2}]])
;; => [{:values
;;      [[0.34062805322444173 -0.8803962660636477]
;;       [-0.34062805322444173 2.8803962660636477]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values
;;      [[1.178246055649492 0.016014053126063077]
;;       [0.821753944350508 1.983985946873937]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values
;;      [[2.356492111298984 1.0320281062521262]
;;       [1.643507888701016 4.967971893747874]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}]

;; => [{:values [[-1.0 1.0] [1.0 1.0]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values
;;      [[1.7071067811865475 0.2928932188134524]
;;       [0.29289321881345254 1.7071067811865475]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}
;;     {:values
;;      [[3.664100588675687 1.8905996075495417]
;;       [0.33589941132431256 4.109400392450459]],
;;      :attribs {:stroke-width 0.72, :stroke "black"},
;;      :layout #function[quickthing/svg-trueline-plot]}]


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
(quickthing/zero-axis [[ 2.0,  2.0]
                       [-2.0, -2.0]])
;; => {:x-axis
;;     {:scale #function[thi.ng.geom.viz.core/linear-scale/fn--49576],
;;      :major-size 10,
;;      :pos 300.0,
;;      :major (-2.0 -1.0 0.0 1.0 2.0),
;;      :label-dist 20.571428571428573,
;;      :attribs {:stroke "#000f", :stroke-width 18/5},
;;      :label #function[thi.ng.geom.viz.core/default-svg-label/fn--49548],
;;      :label-style
;;      {:fill "black",
;;       :stroke "none",
;;       :font-family "Arial, sans-serif",
;;       :font-size 18.0,
;;       :text-anchor "start",
;;       :transform "translate(9.0 0.0)"},
;;      :minor nil,
;;      :domain [-2.18 2.18],
;;      :minor-size 5,
;;      :visible true,
;;      :range [90.0 910.0]},
;;     :y-axis
;;     {:scale #function[thi.ng.geom.viz.core/linear-scale/fn--49576],
;;      :major-size 10,
;;      :pos 500.0,
;;      :major (-2.0 -1.0 0.0 1.0 2.0),
;;      :label-dist 9.0,
;;      :attribs {:stroke "#000f", :stroke-width 18/5},
;;      :label #function[thi.ng.geom.viz.core/default-svg-label/fn--49548],
;;      :label-style
;;      {:fill "black",
;;       :stroke "none",
;;       :font-family "Arial, sans-serif",
;;       :font-size 18.0,
;;       :text-anchor "end"},
;;      :minor nil,
;;      :domain [-2.18 2.18],
;;      :minor-size 5,
;;      :label-y -9.0,
;;      :visible true,
;;      :range [600 0]},
;;     :grid
;;     {:attribs
;;      {:stroke "#caa", :stroke-dasharray "3.6 7.2", :stroke-width 0.72},
;;      :minor-x true,
;;      :minor-y true},
;;     :data []}
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
  index-labels
  "Calls `adjustable-text` but inserts the index automatically"
  [data
   & [{:keys [attribs
              scale]
       :or   {attribs nil
              scale   36}}]]
  (labels (map-indexed (fn [idx
                            data-point]
                         (assoc-in data-point
                                   [2
                                    :text]
                                   idx))
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
#_
(identity red-blue-colors)
#_
(count red-blue-colors)
#_
(col/rgba 0 0 0 0)

(def
  purple-yellow
  "Goes from Yellow to purple.
  However the zero point is set to black.
  This is for if there is a mask"
  (assoc (->> "colorcet/CET-L17.csv"
              io/resource ;; loads from local classpath
              slurp
              csv/read-csv
              (map
                (fn [color]
                  (map #(Double/parseDouble
                          %)
                       color)))
              (mapv #(apply col/rgba
                            %)))
         0
         (col/rgba 0 0 0 0)))
#_
(identity purple-yellow)

(def
  rainbow
  "Rainbow pallet, with firt and last values black"
  (let [rainbow (->> "colorcet/CET-R2.csv"
                     io/resource ;; loads from local classpath
                     slurp
                     csv/read-csv
                     (map
                       (fn [color]
                         (map #(Double/parseDouble
                                 %)
                              color)))
                     (mapv #(apply col/rgba
                                   %)))]
    (-> rainbow
        (assoc 0
               (col/rgba 0 0 0 1.0))
        (assoc (-> rainbow
                   count
                   dec)
               (col/rgba 0 0 0 1.0)))))
#_
(identity rainbow)

(defn
  from-colorvec
  "Given a 0-1 value
  Find the equivalent in a vector of colors"
  [color-vec
   fraction]
  (let [num-colors (count color-vec)]
    (if (= fraction
           1.0)
      (last color-vec)
      (->> fraction
           (* num-colors)
           floor
           int
           (get color-vec)))))
#_
(from-colorvec red-blue-colors
               0.7)
;; => {:r 0.924, :g 0.624, :b 0.551, :a 1.0}

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
  sturges-bin-num
  "Uses Sturges' rule to detemine an optimal number of bins
  NOTE: You re-`count` the data.. so a bit suboptimal"
  [data-vec]
  (-> data-vec
      count
      clojure.math/log
      (/ (clojure.math/log 2))
      clojure.math/ceil
      inc))


#_#_
counts-for-each-index (->> indeces
                           (mapv (fn [time-index]
                                   (let [rain-vector (-> data-matrix
                                                         (uncomplicate.neanderthal.core/col time-index)
                                                         seq
                                                         vec)]
                                     (let [bin-size (-> rain-vector
                                                        count
                                                        clojure.math/log
                                                        (/ (clojure.math/log 2))
                                                        clojure.math/ceil
                                                        inc)] ;; Sturges' rule
                                       (->> (update-vals (->> (uncomplicate.neanderthal.core/col data-matrix
                                                                                                 time-index)
                                                              seq
                                                              vec
                                                              (mapv #(/ %
                                                                        bin-size))
                                                              (mapv int )
                                                              (mapv (partial *
                                                                             bin-size))
                                                              (group-by identity))
                                                         count)
                                            (into (sorted-map-by <))))))))

(defn
  bin-data
  "The data is expected to be 1D
  [a,b,c,d,e,..]
  It gets binned according to the Sturges Rule bin size.
  You get back a vector of
  [[x,w]
   [y,u]
   [z,v]
  ..]
  Where  x y z are the bin centers
  And w u v are the counts"
  [data-vec
   & [{:keys [binfactor]
       :or   {binfactor 5}}]]
  (let [data-min (apply min
                        data-vec)
        data-max (apply max
                        data-vec)]
    (let [bin-size    (/ (- data-max
                            data-min)
                         (* binfactor
                            (sturges-bin-num data-vec)))
          zeroed-data (->> data-vec
                           (mapv #(- %
                                     data-min)))]
      (let [bin-indeces (->> zeroed-data
                             (mapv (fn [data-val]
                                     (-> data-val
                                         (/ bin-size)
                                         clojure.math/floor
                                         int))))]
        (vec (update-keys (update-vals (group-by identity
                                                 bin-indeces)
                                       count)
                          (fn [bin-index]
                            (+ (* bin-index
                                  bin-size)
                               (/ bin-size
                                  2.0)
                               data-min))))))))
#_
(vec (bin-data [1.1 2.2 3.4 5.5 6.7 8.8 9.9 4.4 3.4 5.6]))
;; => [[1.276 1]
;;     [2.332 1]
;;     [3.3880000000000003 2]
;;     [5.5 2]
;;     [6.556000000000001 1]
;;     [8.668000000000001 1]
;;     [10.076 1]
;;     [4.444000000000001 1]]

(defn
  bin-weighted-data
  "The data is expected to be 1D
  [a,b,c,d,e,..]
  with an equivalent vector of weights
  [u,v,w,x,y,..]
  It gets binned according to the Sturges Rule bin size.
  You get back a vector of
  [[x,w]
   [y,u]
   [z,v]
  ..]
  Where  x y z are the bin centers
  And w u v are the sum of weights"
  [data-vec
   weights-vec
   & [{:keys [bin-number]
       :or   {bin-number (sturges-bin-num data-vec)}}]]
  (let [data-min (apply min
                        data-vec)
        data-max (apply max
                        data-vec)]
    (let [bin-size    (/ (- data-max
                            data-min)
                         bin-number)
          zeroed-data (->> data-vec
                           (mapv #(- %
                                     data-min)))]
      (let [bin-indeces (->> zeroed-data
                             (mapv (fn [data-val]
                                     (-> data-val
                                         (/ bin-size)
                                         clojure.math/floor ;; this bins it
                                         int))))]
        (let [bin-index-weight-pairs (mapv vector
                                           bin-indeces
                                           weights-vec)]
          (vec (update-keys (update-vals (group-by first
                                                   bin-index-weight-pairs)
                                         (fn [points-in-bin-vec]
                                           (->> points-in-bin-vec
                                                (mapv second)
                                                (apply +))))
                            (fn [bin-index]
                              (+ (* bin-index
                                    bin-size)
                                 (/ bin-size
                                    2.0)
                                 data-min)))))))))
#_
(vec (bin-weighted-data [1.1 2.2 3.4 5.5 6.7 8.8 9.9 4.4 3.4 5.6]
                        [1.0 2.0 3.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0]))
;; => [[1.276 1.0]
;;     [2.332 2.0]
;;     [3.3880000000000003 4.0]
;;     [5.5 2.0]
;;     [6.556000000000001 1.0]
;;     [8.668000000000001 1.0]
;;     [10.076 1.0]
;;     [4.444000000000001 1.0]]

(defn
  ecdf
  "Take a sequence [a, b, c, d, ..]
  And return a
  [[x,w]
   [y,u]
   [z,v]
  ..]
  that represents the eCDF
  NOTE: The integration direction can be reversed"
  [data-vec
   & [{:keys [reversed?]
       :or   {reversed? false}}]]
  (let [step-size (/ 1.0
                     (count data-vec))]
    (->> data-vec
         (sort (if reversed?
                 >
                 <))
         (mapv (fn [cumulative-probability
                    data-point]
                 [data-point
                  cumulative-probability])
               (range step-size
                      (+ 1.0
                         step-size)
                      step-size))
         (into [[0.0
                 0.0]])
         (partition 2
                    1)
         (mapv (fn [[[bottom-x
                      bottom-y]
                     [top-x
                      top-y]]]
                 [[top-x
                   bottom-y]
                  [top-x
                   top-y]]))
         (reduce into ;; a 1-level `flatten`
                 []))))
#_
(ecdf [1.1
       1.3
       1.5
       2.0
       3.0
       3.4
       3.5
       3.6])
;; => [[1.1 0.0]
;;     [1.1 0.125]
;;     [1.3 0.125]
;;     [1.3 0.25]
;;     [1.5 0.25]
;;     [1.5 0.375]
;;     [2.0 0.375]
;;     [2.0 0.5]
;;     [3.0 0.5]
;;     [3.0 0.625]
;;     [3.4 0.625]
;;     [3.4 0.75]
;;     [3.5 0.75]
;;     [3.5 0.875]
;;     [3.6 0.875]
;;     [3.6 1.0]]

(defn
  ecdf-weighted
  "Take a sequence [a, b, c, d, ..]
  And return a
  [[x,w]
   [y,u]
   [z,v]
  ..]
  that represents the eCDF
  NOTE: The integration direction can be reversed"
  [data-vec
   weights-vec
   & [{:keys [reversed?]
       :or   {reversed? false}}]]
  (let [sorted-pairs (->> (mapv vector
                                data-vec
                                weights-vec)
                          (sort-by first))]
    (let [[sum-of-weights
           with-cummulative-weights] (->> sorted-pairs
                                          (into [[(first (first sorted-pairs))
                                                  0.0]])
                                          (#(if reversed?
                                              (reverse %)
                                              %))
                                          (reduce (fn [[old-sum-of-weights
                                                        adjusted-vec]
                                                       [next-coord
                                                        next-weight]]
                                                    (let [new-sum-of-weights (+ old-sum-of-weights
                                                                                next-weight)]
                                                      [new-sum-of-weights
                                                       (conj adjusted-vec
                                                             [next-coord
                                                              new-sum-of-weights])]))
                                                  [0.0
                                                   []]))]
      (->> with-cummulative-weights
           (mapv (fn [[coord
                       weight-sum]]
                   [coord
                    (/ weight-sum
                       sum-of-weights)]))
           (partition 2
                      1)
           (mapv (fn [[[bottom-x
                        bottom-y]
                       [top-x
                        top-y]]]
                   [[top-x
                     bottom-y]
                    [top-x
                     top-y]]))
           (reduce into ;; a 1-level `flatten`
                   [])))))
#_
(ecdf-weighted [1.1
                1.3
                1.5
                2.0
                3.0
                3.4]
               [1.0
                2.0
                1.0
                3.0
                2.0
                1.0])
;; => [[1.1 0.0]
;;     [1.1 0.1]
;;     [1.3 0.1]
;;     [1.3 0.3]
;;     [1.5 0.3]
;;     [1.5 0.4]
;;     [2.0 0.4]
;;     [2.0 0.7]
;;     [3.0 0.7]
;;     [3.0 0.9]
;;     [3.4 0.9]
;;     [3.4 1.0]]

#_
(ecdf-weighted [1.1
                1.3
                1.5
                2.0
                3.0
                3.4]
               [1.0
                2.0
                1.0
                3.0
                2.0
                1.0]
               {:reversed? true})
;; => [[3.0 0.1]
;;     [3.0 0.3]
;;     [2.0 0.3]
;;     [2.0 0.6]
;;     [1.5 0.6]
;;     [1.5 0.7]
;;     [1.3 0.7]
;;     [1.3 0.9]
;;     [1.1 0.9]
;;     [1.1 1.0]
;;     [0.0 1.0]
;;     [0.0 1.0]]

;; => [[1.1 0.0]
;;     [1.1 1.0]
;;     [1.3 1.0]
;;     [1.3 3.0]
;;     [1.5 3.0]
;;     [1.5 4.0]
;;     [2.0 4.0]
;;     [2.0 7.0]
;;     [3.0 7.0]
;;     [3.0 9.0]
;;     [3.4 9.0]
;;     [3.4 10.0]]
;; => [[1.1 0.0]
;;     [1.1 1.0]
;;     [1.3 1.0]
;;     [1.3 3.0]
;;     [1.5 3.0]
;;     [1.5 4.0]
;;     [2.0 4.0]
;;     [2.0 7.0]
;;     [3.0 7.0]
;;     [3.0 9.0]
;;     [3.4 9.0]
;;     [3.4 10.0]]

(defn
  hist
  "Histograph of a bunch sequence of points.
  The data is expected to be 1D
  [a,b,c,d,e,..].
  NOTE: b/c you don't have x/y limits a priori,
  this will rarely make sense to run..
  Probably want to `bin-data` manually.
  Then run `bars` "
  [data
   & [{:keys [attribs]}]]
  [{:values  (-> data
                 bin-data)
    :attribs (merge {:fill         "none"
                     :stroke-width 10
                     :stroke       "#ffb2b0"}
                    attribs)
    :layout  viz/svg-bar-plot}])

(defn
  bars
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
