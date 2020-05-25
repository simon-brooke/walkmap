# Dali performance

Notes written while trying to characterise the performance problem in Dali.

## Hypothesis one: it's the way I format the polygons that's the issue

Firstly, with both versions of `stl->svg` using the same version of `facet->svg-poly`, i.e. this one:

    (defn- facet->svg-poly
      [facet]
      [:polygon
       {:points (s/join " " (map #(str (:x %) "," (:y %)) (:vertices facet)))}])

we get this performance using the smaller `isle_of_man` map:

    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :hiccup)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (def hiccup (binary-stl-file->svg "resources/isle_of_man.stl" "resources/isle_of_man.svg")))
    20-05-25 09:21:43 mason INFO [walkmap.svg:82] - Generating SVG for  :hiccup  renderer
    20-05-25 09:21:43 mason INFO [walkmap.svg:96] - Emitting SVG with  :hiccup  renderer
    "Elapsed time: 86.904891 msecs"
    #'walkmap.svg/hiccup
    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :dali)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (def dali (binary-stl-file->svg "resources/isle_of_man.stl" "resources/isle_of_man.svg")))
    20-05-25 09:22:17 mason INFO [walkmap.svg:82] - Generating SVG for  :dali  renderer
    20-05-25 09:22:17 mason INFO [walkmap.svg:96] - Emitting SVG with  :dali  renderer
    "Elapsed time: 890.863814 msecs"
    #'walkmap.svg/dali

If we switch the Dali render to use my original version of `facet->svg-poly`, i.e. this one:

    (defn- dali-facet->svg-poly
      [facet]
      (vec
        (cons
          :polygon
          (map #(vec (list (:x %) (:y %))) (:vertices facet)))))

we get this performance:

    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :hiccup)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (def hiccup (binary-stl-file->svg "resources/isle_of_man.stl" "resources/isle_of_man.svg")))
    20-05-25 09:35:33 mason INFO [walkmap.svg:82] - Generating SVG for  :hiccup  renderer
    20-05-25 09:35:33 mason INFO [walkmap.svg:96] - Emitting SVG with  :hiccup  renderer
    "Elapsed time: 84.09972 msecs"
    #'walkmap.svg/hiccup
    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :dali)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (def dali (binary-stl-file->svg "resources/isle_of_man.stl" "resources/isle_of_man.svg")))
    20-05-25 09:35:41 mason INFO [walkmap.svg:82] - Generating SVG for  :dali  renderer
    20-05-25 09:35:41 mason INFO [walkmap.svg:96] - Emitting SVG with  :dali  renderer
    "Elapsed time: 874.292007 msecs"
    #'walkmap.svg/dali

No significant difference in performance.

If we generate but don't render, we get this:

    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :hiccup)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (def hiccup (binary-stl-file->svg "resources/isle_of_man.stl")))
    20-05-25 09:37:44 mason INFO [walkmap.svg:82] - Generating SVG for  :hiccup  renderer
    "Elapsed time: 52.614707 msecs"
    #'walkmap.svg/hiccup
    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :dali)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (def dali (binary-stl-file->svg "resources/isle_of_man.stl")))
    20-05-25 09:38:07 mason INFO [walkmap.svg:82] - Generating SVG for  :dali  renderer
    "Elapsed time: 49.891043 msecs"
    #'walkmap.svg/dali

This implies that the problem is not in the way polygons are formatted.

The difference between the two versions of `facet->svg-poly` is as follows:

### New version, works with both Hiccup and Dali:

    walkmap.svg=> (def stl (decode-binary-stl "resources/isle_of_man.stl"))
    #'walkmap.svg/stl
    walkmap.svg=> (def facet (first (:facets stl)))
    #'walkmap.svg/facet
    walkmap.svg=> (pprint facet)
    {:normal {:x -0.0, :y 0.0, :z 1.0},
     :vertices
     [{:x 3.0, :y 1.0, :z 1.0}
      {:x 2.0, :y 3.0, :z 1.0}
      {:x 0.0, :y 0.0, :z 1.0}],
     :abc 0}
    nil
    walkmap.svg=> (pprint (facet->svg-poly facet))
    [:polygon {:points "3.0,1.0 2.0,3.0 0.0,0.0"}]
    nil

In other words, the new version constructs the `:points` attribute of the `:polygon` tag by string concatenation, and the renderer just needs to output it.

### Older version, works with Dali only:

    walkmap.svg=> (pprint (dali-facet->svg-poly facet))
    [:polygon [3.0 1.0] [2.0 3.0] [0.0 0.0]]
    nil

This means that the renderer is actually doing more work, since it has to compose the `:points` attribute itself; nevertheless there doesn't seem to be an increased time penalty.

### Conclusion

It doesn't seem that formatting the polygons is the issue.

## Hypothesis two: Dali renderer scales non-linearly with number of objects drawn

To test this, we need some otherwise-similar test files with different numbers of objects:

    walkmap.svg=> (count (:facets stl))
    4416
    walkmap.svg=> (def small-stl (assoc stl :facets (take 400 (:facets stl))))
    #'walkmap.svg/small-stl
    walkmap.svg=> (count (:facets small-stl))
    400
    walkmap.svg=> (def large-stl (decode-binary-stl "../the-great-game/resources/maps/heightmap.stl"))
    #'walkmap.svg/large-stl
    walkmap.svg=> (count (:facets large-stl))
    746585
    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :dali)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (dali.io/render-svg (stl->svg small-stl) "dali-small.svg"))
    20-05-25 10:12:25 mason INFO [walkmap.svg:92] - Generating SVG for  :dali  renderer
    "Elapsed time: 32.55506 msecs"
    nil
    walkmap.svg=> (def ^:dynamic *preferred-svg-render* :hiccup)
    #'walkmap.svg/*preferred-svg-render*
    walkmap.svg=> (time (spit "hiccup-small.svg" (hiccup.core/html (stl->svg small-stl))))
    20-05-25 10:14:07 mason INFO [walkmap.svg:92] - Generating SVG for  :hiccup  renderer
    "Elapsed time: 10.026369 msecs"

So we have

|             | Dali             |             | Hiccup       |             |                     |
| ----------- | ---------------- | ----------- | ------------ | ----------- | ------------------- |
| # of facets | time (msecs)     | objets/msec | time (msecs) | objets/msec | ratio (Dali/Hiccup) |
| ----------- | ---------------- | ----------- | ------------ | ----------- | --------------------|
| 400         | 32.55506         | 12.29       | 10.026369    | 39.89       | 3.35                |
| 4416        | 874.292007       | 5.05        | 84.09972     | 52.51       | 10.40               |
| 746585      | 29,695,695.61    | 0.03        | 16724.848222 | 44.64       | 1775.54             |

### Conclusion

What we're seeing is that Hiccup renders more or less linearly by the number of objects (bear in mind that all of these objects are triangles, so essentially equally complex to render), whereas trhe performance of Dali degrades significantly as the number of objects increases.
