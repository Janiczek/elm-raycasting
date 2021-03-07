# elm-raycasting

This package currently hosts a single function: `Raycast2D.touchedTiles`.

It is an Elm implementation of the DDA algorithm popularized by [OneLoneCoder (Javidx9)](https://twitter.com/javidx9).

* [YouTube video](https://www.youtube.com/watch?v=NbSee-XM7WA)
* [Article](https://lodev.org/cgtutor/raycasting.html)
* [Code](https://github.com/OneLoneCoder/olcPixelGameEngine/blob/61d0e06766c3dbf7571cbf39d1727b1c8b84fedf/Videos/OneLoneCoder_PGE_RayCastDDA.cpp)

The main idea is that instead of walking the ray by small steps and checking which tile we're in, we instead go all the way to the nearest cell boundary. This uses less CPU and doesn't suffer from inaccuracy (overshooting the finishline, as it were).

<img src="data:image/png;base64,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">

You can play with a modified version of this code (such that we can visualize the line segments and seen tiles) in [this Ellie](https://ellie-app.com/cyBMhVp2hv2a1).

