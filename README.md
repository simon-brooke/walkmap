# walkmap

A Clojure library designed to assist in computing walkmaps for games.

## Introduction

This library is written in support of work on
[The Great Game](https://simon-brooke.github.io/the-great-game/codox/Pathmaking.html), but is
separate because it may be of some use in other settings.

## Explanation

*I'm writing this becaue, having returned to this project after four years of 
being distracted by other things, I couldn't even remember what it was for, 
let alone how to use it.*

### 'STL files'

'STL' is not an acronym but an abbreviation; an abreviation of 
'[stereolithography](https://en.wikipedia.org/wiki/Stereolithography)'. Even 
that name is completely misleading, since there is absolutely no sense in
which this is printing with stone.

What an STL file is is in fact a representation of a mesh, originally intended 
for use in 3d printing, and therefore taken to be the bounding mesh of a solid.
This makes it a better format for representing landforms for games than a
simple heightmap, since heightmaps cannot represent things like overhangs and 
caves.

STL files come in two subformats: 'ASCII' (which this library can currently 
read and write), and 'Binary' (which this library can read but not yet write).

STL files are commonly used in game development and can be imported by all of

* [Blender](https://docs.blender.org/manual/en/latest/files/import_export/stl.html)
* [Godot Engine](https://godotengine.org/asset-library/asset/961)
* [Unity](https://github.com/karl-/pb_Stl) (unofficial)
* [Unreal Engine](https://github.com/rdeioris/UnrealSTL) (unofficial)

Additionally, a number of programs exist to convert STL files into other 3D
model file formats.

### 'Superstructure'

I can't remember if 'superstructure' is a name which I found in reading the
literature on STL files, or whether it's one I came up with for my own 
purposes, but essentially in my usage of it:

1. [MicroWorld](https://github.com/simon-brooke/mw-engine) is a two dimensional 
cellular automaton, which I wrote, and which can be used (and which I use) to 
model ecological processes up to and including human settlement;
2. As a two dimensional cellular automaton, it is necessarily essentially a
flat mesh, but I essentially drape that over a 
[heightmap](https://github.com/simon-brooke/mw-engine/blob/master/src/mw_engine/heightmap.clj)
to give a sort of notional two-and-a-half dimensional representation;
3. As such MicroWorld is a moderately useful tool to transform an imagined land
form into a vegetated and populated landscape, with biomes and human habitation
distributed naturalistically.

A 'superstructure', then, in the terms I'm using it, is an intermediary 
representation between a heightmap and a stereolithography model, which carries
data not only about shape, but about biomes, settlement, traversibility and so
on. Thus it is a surface across which routes can efficiently be planned.

A superstructure can currently be constructed from a MicroWorld world, read in
from an [Extensible Data Notation](https://github.com/edn-format/edn). There 
isn't currently a function to construct a superstructure from an in-memory
MicroWorld world, because MicroWorld, when working with models of the size I'm 
using, is hugely memory hungry; but there clearly could be and that is an
addition you should expect.

A superstructure cannot currently be constructed directly from a heightmap, but
that would also be a trivial enhancement and is planned.

```clojure
(use 'cc.journeyman.walkmap.microworld :reload)
(require '[cc.journeyman.walkmap.vertex :refer [vertex]])
(load-microworld-edn 
    "../the-great-game/resources/test/galloway-populated-20240407.edn")
```
or
```clojure
(use 'cc.journeyman.walkmap.microworld :reload)
(require '[cc.journeyman.walkmap.vertex :refer [vertex]])
(load-microworld-edn 
    "../the-great-game/resources/test/galloway-populated-20240407.edn" :mw {} 
    (vertex 1000 1000 10))
```

Note that this is a lot of processing and even on a fast machine is going to 
take significant walkclock time. You cannot reasonably do it, except for 
utterly trivial times, during game initialisation; instead you need to do it
in the 'baking' phase of game development, and consider the STL file as the
resoource to be shipped with the game.

However, although it is time-expensive, it is not, unlike MicroWorld, either
memory or processor hungry. This will not blow up your machine.

A superstructure can be written to an ASCII STL file.

## Usage

What works:

No clojars repo yet, build the jar yourself with

    lein install

Lein dependency:

    [walkmap "0.1.0-SNAPSHOT"]

* [API documentation](https://simon-brooke.github.io/walkmap/codox/index.html)
* [Test coverage](https://simon-brooke.github.io/walkmap/cloverage/index.html)

### Converting heightmaps to STL

Doesn't work yet, and is not a priority. Use
[hmm](https://github.com/fogleman/hmm) instead.

### Reading binary STL files

    (require '[walkmap.stl :refer [decode-binary-stl]])
    (decode-binary-stl "path/to/input-file.stl")

Works, seems good.

### Writing ASCII STL files

    (require '[walkmap.stl :refer [write-ascii-stl]])
    (write-ascii-stl "path/to/output-file.ascii.stl" stl-structure)

Works, seems good, agrees with Python implementation except for different
number of places of decimals printed.

### Converting STL to SVG

    (require '[walkmap.svg :refer [stl->svg]])
    (stl->svg stl-structure)

Works, seems good. Returns a [hiccup](https://github.com/weavejester/hiccup)
representation of the SVG.

**NOTE THAT** the SVG data does not contain height information, which the
STL data does contain. Thus gradient information can only be obtained from
the STL.

### Converting STL file to SVG or SVG file

    (require '[walkmap.core :refer [binary-stl-file->svg]])
    (binary-stl-file->svg "path/to/input-file.stl")

Works, seems good. Returns a [hiccup](https://github.com/weavejester/hiccup)
representation of the SVG.

    (require '[walkmap.core :refer [binary-stl-file->svg]])
    (binary-stl-file->svg "path/to/input-file.stl" "path-to-output-file.svg")

As above, but, as a side effect, writes the SVG to the specified output file.
Works for smaller test files, as above.

### Merging exclusion maps and reserved area maps

It is intended that it should be possible to merge exclusion maps (maps of
areas which should be excluded from the traversable area) with maps derived
from height maps. These exclusion maps will probably be represented as SVG.

This is not yet implemented.

Culling facets in ocean areas is implemented and works:

    (require '[walkmap.core :refer [cull-ocean-facets *sea-level*]])
    (cull-ocean-facets stl)

If sea level in your heightmaps is not zero, e.g. is 5, set it thus:

    (def ^:dynamic *sea-level* 5.0)
    (cull-ocean-facets stl)

It is **strongly recomended** that you set `*sea-level*` to a floating point
number, not an integer, because numbers are specified in the STL file as
floating point, and in Clojure, `(= 5 5.0)` returns `false`.

### Merging road maps and river system maps

It is intended that it should be possible to merge road maps (maps of already
computed routes) with maps derived from height maps. These exclusion maps will
probably be represented as SVG. This is not yet implemented.

River system maps are conceptually similar to road maps; this too is not yet
implemented.

### Computing new routes and roads

It is intended that it should be possible, by simulating agents traversing the
terrain, to compute the courses of new roads/tracks/paths. The routing
algorithm should implement the following rules.

1. No route may pass through any part of a reserved holding, except the holding which is its origin, if any, and the holding which is its destination (and in any case we won't render paths or roads within holdings, although traversal information may be used to determine whether a holding, or part of it, is paved/cobbled;
2. No route may pass through any building, with the exception of a city gate;
3. We don't have bicycles: going uphill costs work, and you don't get that cost back on the down hill. Indeed, downhills are at least as expensive to traverse as flat ground;
4. Any existing route segment costs only a third as much to traverse as open ground having the same gradient;
5. A more used route costs less to traverse than a less used route;
6. There is a significant cost penalty to crossing a watercourse, except at an existing crossing.

This is not yet implemented.

### Writing out computed road maps as SVG

It is intended that, after computing new routes and roads, it should be
possible to expoer an updated road map as SVG. This is not yet implemented.


## License

Copyright © 2020 Simon Brooke

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
