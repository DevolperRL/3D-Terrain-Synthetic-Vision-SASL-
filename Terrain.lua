local gridSize = 150                 -- Change this for the Distance
local spacingMeters = 100          -- Change this for the size of hte Triangles

local heightScale = 1.3          -- Change this for the Height of the terrain, 1.3 should be right value

local acfX = globalPropertyf("sim/flightmodel/position/local_x")
local acfY = globalPropertyf("sim/flightmodel/position/local_y")
local acfZ = globalPropertyf("sim/flightmodel/position/local_z")
local height = globalProperty("sim/flightmodel2/position/ellipsoid_height")
local heading = globalPropertyf("sim/cockpit2/gauges/indicators/heading_AHARS_deg_mag_pilot")

function vec3(x,y,z) return {x=x,y=y,z=z} end
function vec3_sub(a,b) return {x=a.x-b.x, y=a.y-b.y, z=a.z-b.z} end
function vec3_add(a,b) return {x=a.x+b.x, y=a.y+b.y, z=a.z+b.z} end
function vec3_mul(a,s) return {x=a.x*s, y=a.y*s, z=a.z*s} end
function vec3_dot(a,b) return a.x*b.x + a.y*b.y + a.z*b.z end
function vec3_cross(a,b)
    return {
        x = a.y*b.z - a.z*b.y,
        y = a.z*b.x - a.x*b.z,
        z = a.x*b.y - a.y*b.x
    }
end
function vec3_length(v) return math.sqrt(v.x*v.x + v.y*v.y + v.z*v.z) end
function vec3_normalize(v)
    local len = vec3_length(v)
    if len == 0 then return vec3(0,0,0) end
    return vec3_mul(v, 1/len)
end

function lookAt(pos, target, up)
    local forward = vec3_normalize(vec3_sub(target, pos))
    local right = vec3_normalize(vec3_cross(forward, up))
    local camUp = vec3_cross(right, forward)
    return right, camUp, forward
end

function projectPoint(worldPos, camPos, camRight, camUp, camForward, screenWidth, screenHeight, fovDeg, nearPlane)
    local rel = vec3_sub(worldPos, camPos)

    local x = vec3_dot(rel, camRight)
    local y = vec3_dot(rel, camUp)
    local z = vec3_dot(rel, camForward)

    if z < nearPlane then
        return nil
    end

    local fovRad = math.rad(fovDeg)
    local aspectRatio = screenWidth / screenHeight

    local px = (x / (z * math.tan(fovRad/2))) * (screenWidth/2) + (screenWidth/2)
    local py = (y / (z * math.tan(fovRad/(2*aspectRatio)))) * (screenHeight/2) + (screenHeight/2)

    return px, screenHeight+py
end

function lerpColor(c1, c2, t)
    return {
        c1[1] + (c2[1] - c1[1]) * t,
        c1[2] + (c2[2] - c1[2]) * t,
        c1[3] + (c2[3] - c1[3]) * t,
        c1[4] + (c2[4] - c1[4]) * t,
    }
end

function elevationToColor(elev)
    local maxElev = 3000
    local t = math.min(math.max(elev / maxElev, 0), 1)

    local colors = {
        {0.0, 0.5, 0.0, 1.0},   -- dark green (low)
        {0.0, 1.0, 0.0, 1.0},   -- bright green
        {1.0, 1.0, 0.0, 1.0},   -- bright yellow
        {1.0, 0.5, 0.0, 1.0},   -- orange
        {1.0, 0.0, 0.0, 1.0},   -- bright red (high)
        {1.0, 1.0, 1.0, 1.0},   -- white (extreme high)
    }

    local n = #colors - 1
    local segment = t * n
    local index = math.floor(segment) + 1
    local frac = segment - (index - 1)

    if index >= n + 1 then
        return colors[#colors]
    end

    return lerpColor(colors[index], colors[index + 1], frac)
end

function computeShade(p1, p2, p3)
    local v1 = {p2[1] - p1[1], p2[2] - p1[2], p2[3] - p1[3]}
    local v2 = {p3[1] - p1[1], p3[2] - p1[2], p3[3] - p1[3]}

    local nx = v1[2]*v2[3] - v1[3]*v2[2]
    local ny = v1[3]*v2[1] - v1[1]*v2[3]
    local nz = v1[1]*v2[2] - v1[2]*v2[1]

    local length = math.sqrt(nx*nx + ny*ny + nz*nz)
    if length == 0 then return 1 end

    nx = nx / length
    ny = ny / length
    nz = nz / length

    local lightDir = {0.3, 0.7, 0.6}
    local lightLength = math.sqrt(lightDir[1]^2 + lightDir[2]^2 + lightDir[3]^2)
    lightDir[1] = lightDir[1] / lightLength
    lightDir[2] = lightDir[2] / lightLength
    lightDir[3] = lightDir[3] / lightLength

    local dot = nx*lightDir[1] + ny*lightDir[2] + nz*lightDir[3]

    return math.max(0.3, dot)
end

local lastTerrainDraw = 0
local terrainCache = {}

function drawTerrainProbeMap(screenWidth, screenHeight)
    local px = get(acfX)
    local py = get(acfY)
    local pz = get(acfZ)
    local hdg = math.rad(get(heading))
    local alt = get(height)

    local camPos = vec3(px, py + alt/5, pz)
    local forwardVec = vec3(math.sin(-hdg + math.pi), 0, math.cos(-hdg + math.pi))
    local camTarget = vec3_add(camPos, forwardVec)
    local camUp = vec3(0, 1, 0)
    local camRight, camUp, camForward = lookAt(camPos, camTarget, camUp)

    local cosH = math.cos(-hdg)
    local sinH = math.sin(-hdg)

    local pointsGrid = {}
    local validGrid = {}
    local wetGrid = {}

    for gz = -gridSize / 2, gridSize / 2 do
        pointsGrid[gz] = {}
        validGrid[gz] = {}
        wetGrid[gz] = {}

        for gx = -gridSize / 2, gridSize / 2 do
            local offsetX = gx * spacingMeters
            local offsetZ = gz * spacingMeters

            local rotatedX = offsetX * cosH - offsetZ * sinH
            local rotatedZ = offsetX * sinH + offsetZ * cosH

            local worldX = px + rotatedX
            local worldZ = pz + rotatedZ
            local worldY = py + 10000

            local result, locX, locY, locZ, _, _, _, _, _, _, isWet = sasl.probeTerrain(worldX, worldY, worldZ)
            validGrid[gz][gx] = (result == 0)
            wetGrid[gz][gx] = (isWet == 1)
            pointsGrid[gz][gx] = locY
        end
    end

    for gz = -gridSize / 2, gridSize / 2 - 1 do
        for gx = -gridSize / 2, gridSize / 2 - 1 do
            if validGrid[gz][gx] and validGrid[gz][gx+1] and validGrid[gz+1][gx] and validGrid[gz+1][gx+1] then
                local elevTL = pointsGrid[gz][gx] * heightScale
                local elevTR = pointsGrid[gz][gx+1] * heightScale
                local elevBR = pointsGrid[gz+1][gx+1] * heightScale
                local elevBL = pointsGrid[gz+1][gx] * heightScale

                local function rotatedWorld(gx, gz, elev)
                    local offsetX = gx * spacingMeters
                    local offsetZ = gz * spacingMeters
                    local rotatedX = offsetX * cosH - offsetZ * sinH
                    local rotatedZ = offsetX * sinH + offsetZ * cosH
                    return vec3(px + rotatedX, elev, pz + rotatedZ)
                end

                local worldTL = rotatedWorld(gx, gz, elevTL)
                local worldTR = rotatedWorld(gx + 1, gz, elevTR)
                local worldBR = rotatedWorld(gx + 1, gz + 1, elevBR)
                local worldBL = rotatedWorld(gx, gz + 1, elevBL)

                local sxTL, syTL = projectPoint(worldTL, camPos, camRight, camUp, camForward, screenWidth, screenHeight, 60, 0.1)
                local sxTR, syTR = projectPoint(worldTR, camPos, camRight, camUp, camForward, screenWidth, screenHeight, 60, 0.1)
                local sxBR, syBR = projectPoint(worldBR, camPos, camRight, camUp, camForward, screenWidth, screenHeight, 60, 0.1)
                local sxBL, syBL = projectPoint(worldBL, camPos, camRight, camUp, camForward, screenWidth, screenHeight, 60, 0.1)

                if sxTL and sxTR and sxBR and sxBL then
                    local tri1 = {sxTL, syTL, sxTR, syTR, sxBR, syBR}
                    local tri2 = {sxTL, syTL, sxBR, syBR, sxBL, syBL}

                    local avgElev1 = (elevTL + elevTR + elevBR) / 3
                    local avgElev2 = (elevTL + elevBR + elevBL) / 3

                    local baseColor1 = (wetGrid[gz][gx] and wetGrid[gz][gx+1] and wetGrid[gz+1][gx+1])
                        and {0.2, 0.5, 1.0, 1.0}
                        or elevationToColor(avgElev1)

                    local baseColor2 = (wetGrid[gz][gx] and wetGrid[gz+1][gx+1] and wetGrid[gz+1][gx])
                        and {0.2, 0.5, 1.0, 1.0}
                        or elevationToColor(avgElev2)

                    local shade1 = computeShade({sxTL, syTL, elevTL}, {sxTR, syTR, elevTR}, {sxBR, syBR, elevBR})
                    local shade2 = computeShade({sxTL, syTL, elevTL}, {sxBR, syBR, elevBR}, {sxBL, syBL, elevBL})

                    local color1 = {
                        baseColor1[1] * shade1,
                        baseColor1[2] * shade1,
                        baseColor1[3] * shade1,
                        baseColor1[4]
                    }

                    local color2 = {
                        baseColor2[1] * shade2,
                        baseColor2[2] * shade2,
                        baseColor2[3] * shade2,
                        baseColor2[4]
                    }

                    sasl.gl.drawConvexPolygon(tri1, true, 1, color1)
                    sasl.gl.drawConvexPolygon(tri2, true, 1, color2)
                end
            end
        end
    end
end
