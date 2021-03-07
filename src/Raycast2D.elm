module Raycast2D exposing
    ( PxCoords, TileCoords
    , touchedTiles
    )

{-| A library for raycasting geometric tasks in 2D space.


# Types

@docs PxCoords, TileCoords


# Raycasting

@docs touchedTiles

-}

import Set exposing (Set)


{-| -}
type alias TileCoords =
    ( Int, Int )


{-| -}
type alias PxCoords =
    ( Float, Float )


sign : number -> number
sign n =
    case compare n 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


symFloor : Float -> Int
symFloor n =
    sign (round n) * floor (abs n)


symCeiling : Float -> Int
symCeiling n =
    sign (round n) * ceiling (abs n)


ceilingToMultipleOf : Float -> Float -> Float
ceilingToMultipleOf d n =
    d * toFloat (symCeiling (n / d))


floorToMultipleOf : Float -> Float -> Float
floorToMultipleOf d n =
    d * toFloat (symFloor (n / d))


{-| This is an Elm implementation of the DDA algorithm popularized by OneLoneCoder (Javidx9).

YouTube video: <https://www.youtube.com/watch?v=NbSee-XM7WA>
Article: <https://lodev.org/cgtutor/raycasting.html>
Code: <https://github.com/OneLoneCoder/olcPixelGameEngine/blob/61d0e06766c3dbf7571cbf39d1727b1c8b84fedf/Videos/OneLoneCoder_PGE_RayCastDDA.cpp>

-}
touchedTiles : Float -> PxCoords -> PxCoords -> Set TileCoords
touchedTiles tileSizePx (( fromPxX, fromPxY ) as fromPx) ( toPxX, toPxY ) =
    let
        fromTile : TileCoords
        fromTile =
            ( floor <| fromPxX / tileSizePx
            , floor <| fromPxY / tileSizePx
            )

        ( fromTileX, fromTileY ) =
            fromTile

        toTile : TileCoords
        toTile =
            ( floor <| toPxX / tileSizePx
            , floor <| toPxY / tileSizePx
            )
    in
    if fromTile == toTile then
        Set.singleton fromTile

    else
        let
            moveToTileEdge : Bool -> Float -> Float
            moveToTileEdge goNegative current =
                let
                    fn =
                        if goNegative then
                            floorToMultipleOf tileSizePx

                        else
                            ceilingToMultipleOf tileSizePx

                    potentiallyNew =
                        fn current
                in
                if current == potentiallyNew then
                    if goNegative then
                        potentiallyNew - tileSizePx

                    else
                        potentiallyNew + tileSizePx

                else
                    potentiallyNew

            movePx : Order -> PxCoords -> PxCoords
            movePx xVsY_ ( pxX, pxY ) =
                case xVsY_ of
                    LT ->
                        let
                            newX =
                                moveX pxX
                        in
                        ( newX
                        , pxY + (newX - pxX) * moveYForUnitOfX
                        )

                    EQ ->
                        let
                            newX =
                                moveX pxX

                            newY =
                                moveY pxY
                        in
                        ( newX, newY )

                    GT ->
                        let
                            newY =
                                moveY pxY
                        in
                        ( pxX + (newY - pxY) * moveXForUnitOfY
                        , newY
                        )

            moveX : Float -> Float
            moveX =
                moveToTileEdge rayDirXNegative

            moveY : Float -> Float
            moveY =
                moveToTileEdge rayDirYNegative

            normalize : ( Float, Float ) -> ( Float, Float )
            normalize ( x, y ) =
                let
                    length =
                        sqrt (x ^ 2 + y ^ 2)
                in
                ( x / length
                , y / length
                )

            ( rayDirX, rayDirY ) =
                normalize
                    ( toPxX - fromPxX
                    , toPxY - fromPxY
                    )

            ( moveYForUnitOfX, moveXForUnitOfY ) =
                ( rayDirY / rayDirX
                , rayDirX / rayDirY
                )

            ( rayUnitStepSizeX, rayUnitStepSizeY ) =
                ( abs <| 1 / rayDirX
                , abs <| 1 / rayDirY
                )

            rayDirXNegative : Bool
            rayDirXNegative =
                rayDirX < 0

            rayDirYNegative : Bool
            rayDirYNegative =
                rayDirY < 0

            ( initRayLengthX, initRayLengthY ) =
                ( abs <| (moveX fromPxX - fromPxX) * rayUnitStepSizeX
                , abs <| (moveY fromPxY - fromPxY) * rayUnitStepSizeY
                )

            initXVsY : Order
            initXVsY =
                compare initRayLengthX initRayLengthY

            firstStepPx : PxCoords
            firstStepPx =
                movePx initXVsY ( fromPxX, fromPxY )

            moveALittle : Bool -> Float -> Float
            moveALittle negative coord =
                if negative then
                    coord - 1

                else
                    coord + 1

            tileForPx : PxCoords -> TileCoords
            tileForPx ( x, y ) =
                ( symFloor <| moveALittle rayDirXNegative x / tileSizePx
                , symFloor <| moveALittle rayDirYNegative y / tileSizePx
                )

            firstStepTile : TileCoords
            firstStepTile =
                tileForPx firstStepPx

            go : PxCoords -> TileCoords -> Set TileCoords -> Set TileCoords
            go ( currentX, currentY ) ( x, y ) seenTiles =
                if ( x, y ) == toTile then
                    seenTiles

                else
                    let
                        ( rayLengthX, rayLengthY ) =
                            ( abs <| (moveX currentX - currentX) * rayUnitStepSizeX
                            , abs <| (moveY currentY - currentY) * rayUnitStepSizeY
                            )

                        xVsY : Order
                        xVsY =
                            compare rayLengthX rayLengthY

                        newPx : ( Float, Float )
                        newPx =
                            movePx xVsY ( currentX, currentY )

                        (( newX, newY ) as newTile) =
                            tileForPx newPx

                        newTiles : Set TileCoords
                        newTiles =
                            Set.insert newTile seenTiles
                    in
                    go newPx newTile newTiles
        in
        go firstStepPx firstStepTile (Set.fromList [ fromTile, firstStepTile ])
