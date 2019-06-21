namespace Illuminate

open Illuminate.Types

module Math = 
    let deg2rad angle =
        System.Math.PI * angle / 180.0;
    
    let dotProduct (multiplier :Vector, multiplicand:Vector) = 
        let x,y,z = multiplier
        let x2, y2, z2 = multiplicand
        x * x2 + y * y2 + z * z2
    
    let cross multiplier multiplicand : Vector = 
        let ax,ay,az = multiplier
        let bx, by, bz = multiplicand
        (ay*bz - az*by, az*bx - ax*bz, ax*by - ay*bx)
    
    let inverseMag (mag2:float, v:Vector) : Normal = 
        let x,y,z = v
        let invMag = 1. / sqrt mag2
        (x * invMag, y * invMag, z * invMag)

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

    let normalizeVector (v:Vector) : Normal = 
        let x,y,z = v
        let mag2 = dotProduct(v, v)
        match mag2 > 0. with
            | true -> inverseMag (mag2, v)
            | false -> (x, y, z)
    
    let convertNormalToDirection (normal:Normal) : Direction =
        let x,y,z = normal
        {dirX = x; dirY = y; dirZ = z}
    
    let convertDirectionToNormal (direction:Direction) : Normal =
        (direction.dirX, direction.dirY, direction.dirZ)

    let convertDirectionToVector (direction:Direction) : Vector =
        (direction.dirX, direction.dirY, direction.dirZ)
    
    let convertNormalToVector (normal:Normal) : Vector =
        let x,y,z = normal
        (x,y,z)
    
    let convertVectorToNormal (vector:Vector) : Normal =
        let x,y,z = vector
        (x,y,z)
    
    let convertWorldCoordinateToVector (wc:WorldCoordinate) : Vector =
        (wc.x, wc.y, wc.z)
    
    let normalizeDirection (v:Direction) : Normal = 
        convertDirectionToVector(v) |> normalizeVector
    
    let getDirectionFromVector vector =
        let x,y,z = vector
        {dirX = x; dirY = y; dirZ = z}
        
    let normalizeWorld (worldCoordinate, pixel:ScreenCoordinate) = 
        let length = sqrt (worldCoordinate.x * worldCoordinate.x + worldCoordinate.y * worldCoordinate.y + worldCoordinate.z * worldCoordinate.z)
        {dirX = worldCoordinate.x / length; dirY = worldCoordinate.y / length; dirZ = worldCoordinate.z / length; }, pixel
    
    let worldSubWorld point origin : Vector = 
        (point.x - origin.x, point.y - origin.y, point.z - origin.z)

    let addVectorToPoint point vector = 
        let x,y,z = vector
        {x = point.x + x; y = point.y + y; z = point.z + z}
    
    let subVectorFromPoint point vector = 
        let x,y,z = vector
        {x = point.x - x; y = point.y + y; z = point.z + z}
    
    let multiplyVector (vector:Vector, scale:float) : Vector = 
        let x,y,z = vector
        (x * scale, y * scale, z * scale)
    
    let calculateColorIntensity color luminosity = 
        {r = color.r * luminosity; g = color.g * luminosity; b = color.b * luminosity}
    
    let invertDirection dir =
        {dirX = -dir.dirX; dirY = -dir.dirY; dirZ = -dir.dirZ }