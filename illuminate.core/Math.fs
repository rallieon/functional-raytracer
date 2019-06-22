namespace Illuminate

open Illuminate.Types

module Math = 
    let deg2rad angle =
        System.Math.PI * angle / 180.0;
    
    let dot multiplier multiplicand = 
        multiplier.x * multiplicand.x + multiplier.y * multiplicand.y + multiplier.z * multiplicand.z
    
    let cross multiplier multiplicand = 
        {x = multiplier.x*multiplicand.z - multiplier.z*multiplicand.y; y = multiplier.z*multiplicand.x - multiplier.x*multiplicand.z; z = multiplier.x*multiplicand.y - multiplier.y*multiplicand.x}
    
    let inverseMag mag2 v = 
        let invMag = 1. / sqrt mag2
        {x = v.x * invMag; y = v.y * invMag; z = v.z * invMag}

    let calculateQ discr a b c = 
        let q =
            match b > 0. with
                | true -> -0.5 * (b + sqrt(discr))
                | false -> -0.5 * (b - sqrt(discr))
        let x0 = q / a
        let x1 = c / q
        let t0 = 
            match x0 > x1 with
                | true -> x1
                | false -> x0
        
        let t1 = 
            match x0 > x1 with
                | true -> x0
                | false -> x1

        (true, t0, t1)
        
    let solveQuadratic a b c =
        let discr = b * b - 4. * a * c
        match discr < 0., discr = 0. with
            | true, _ -> (false, 0., 0.)
            | false, true -> (true, -0.5 * b / a, -0.5 * b / a)
            | false, false -> calculateQ discr a b c

    let normalizeVector v = 
        let mag2 = dot v v
        match mag2 > 0. with
            | true -> inverseMag mag2 v
            | false -> v
    
    let convertCoordinateToDirection n =
        {dirX = n.x; dirY = n.y; dirZ = n.z}
    
    let convertDirectionToCoordinate direction =
        {x = direction.dirX; y = direction.dirY; z = direction.dirZ}
    
    let normalizeDirection d = 
        d |> convertDirectionToCoordinate |> normalizeVector
        
    let normalizeWorld (worldCoordinate,pixel:ScreenCoordinate) = 
        let length = sqrt (worldCoordinate.x * worldCoordinate.x + worldCoordinate.y * worldCoordinate.y + worldCoordinate.z * worldCoordinate.z)
        {dirX = worldCoordinate.x / length; dirY = worldCoordinate.y / length; dirZ = worldCoordinate.z / length; }, pixel
    
    let worldSubWorld p o = 
        {x = p.x - o.x; y = p.y - o.y; z = p.z - o.z}

    let addVectorToPoint p v = 
        {x = p.x + v.x; y = p.y + v.y; z = p.z + v.z}
    
    let subVectorFromPoint p v = 
        {x = p.x - v.x; y = p.y - v.y; z = p.z - v.z}
    
    let multiplyVector v scale = 
        {x = v.x * scale; y = v.y * scale; z = v.z * scale}
    
    let invertDirection dir =
        {dirX = -dir.dirX; dirY = -dir.dirY; dirZ = -dir.dirZ }
    
    let getTriangleNormal t =
        let A = worldSubWorld t.v1 t.v0
        let B = worldSubWorld t.v2 t.v0
        cross A B
    
    let isInTriangle N point t =
        let edge0 = worldSubWorld t.v1 t.v0
        let pv0 = worldSubWorld point t.v0
        let edge0Check = dot N (cross edge0 pv0)

        let edge1 = worldSubWorld t.v2 t.v1
        let pv1 = worldSubWorld point t.v1
        let edge1Check = dot N (cross edge1 pv1)

        let edge2 = worldSubWorld t.v0 t.v2
        let pv2 = worldSubWorld point t.v2
        let edge2Check = dot N (cross edge2 pv2)
        (edge0Check < 0., edge1Check < 0., edge2Check < 0.)