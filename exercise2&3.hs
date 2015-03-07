{- LANGUAGE Arrows -}
import FRP.Yampa


arr2 :: (a -> b -> c) -> SF (a, b) -> c
arr2 = arr . uncurry

-- x = (1/2) * integral ((vr + vl) * cos theta)
xSF :: SF SimBot Distance
xSF = let v = (vrSF &&& vlSF) >>> arr2 (+)
          t = thetaSF >>> arr cos
      in (v &&& t) >>> arr2 (*) >>> integral >>> arr (/2)

-- y = (1/2) * integral ((vr + vl) * sin theta)
ySF :: SF SimBot Distance
ySF = let v = (vrSF &&& vlSF) >>> arr2 (+)
          t = thetaSF >>> arr sin
      in (v &&& t) >>> arr2 (*) >>> integral >>> arr (/2)

-- theta = (1/l) * integral (vr - vl)
thetaSF :: SF SimBot Distance
thetaSF = let v = (vrSF &&& vlSF) >>> arr2 (-)
          in  v >>> arr2 (*) >>> integral >>> arr (/l)

-- now using arrow syntax
xSF' :: SF SimBot Distance
xSF' = proc inp -> do
       vr <- vrSF     -< inp
       vl <- vlSF     -< inp
       t  <- thetaSF  -< inp
       i  <- integral -< (vr + vl) * cos t
       returnA -< (i / 2)

ySF' :: SF SimBot Distance
ySF' = proc inp -> do
       vr <- vrSF     -< inp
       vl <- vlSF     -< inp
       t  <- thetaSF  -< inp
       i  <- integral -< (vr + vl) * sin t
       returnA -< (i / 2)

thetaSF' :: SF SimBot Distance
thetaSF' = proc inp -> do
       vr <- vrSF     -< inp
       vl <- vlSF     -< inp
       i  <- integral -< (vr - vl)
       returnA -< (i / l)
