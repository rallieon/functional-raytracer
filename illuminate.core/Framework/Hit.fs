namespace Illuminate.Framework
open Illuminate.Framework.Types
open Illuminate.Framework.Math
open FsAlg.Generic

module Hit = 
    let calculateShadowPoint (ray:Direction, point:WorldCoordinate, normal:Normal) = 
        let biasNormal = normal * bias
        point + biasNormal

    let calcHitPoint (origin:WorldCoordinate, ray:Direction, tnear:float) = 
        (ray * tnear) + origin