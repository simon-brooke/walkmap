# Introduction to walkmap

This library is written in support of work on
[The Great Game](https://simon-brooke.github.io/the-great-game/codox/Pathmaking.html), but is
separate because it may be of some use in other settings.

## Usage

What works:

No clojars repo yet, build the jar yourself with

    lein install

Lein dependency:

    [walkmap "0.1.0-SNAPSHOT"]

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
