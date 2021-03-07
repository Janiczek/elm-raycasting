# elm-raycasting

This package currently hosts a single function: `Raycast2D.touchedTiles`.

It is an Elm implementation of the DDA algorithm popularized by [OneLoneCoder (Javidx9)](https://twitter.com/javidx9).

* [YouTube video](https://www.youtube.com/watch?v=NbSee-XM7WA)
* [Article](https://lodev.org/cgtutor/raycasting.html)
* [Code](https://github.com/OneLoneCoder/olcPixelGameEngine/blob/61d0e06766c3dbf7571cbf39d1727b1c8b84fedf/Videos/OneLoneCoder_PGE_RayCastDDA.cpp)

The main idea is that instead of walking the ray by small steps and checking which tile we're in, we instead go all the way to the nearest cell boundary. This uses less CPU and doesn't suffer from inaccuracy (overshooting the finishline, as it were).

![Screenshot](https://github.com/Janiczek/elm-raycasting/raw/main/docs/raycasting.png)

You can play with a modified version of this code (such that we can visualize the line segments and seen tiles) in [this Ellie](https://ellie-app.com/cyBMhVp2hv2a1).

