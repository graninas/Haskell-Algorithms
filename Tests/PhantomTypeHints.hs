module TypeHint where


data Val a b = Val b



data CelsiusHint
data KelvinHint


getTemperature :: Val KelvinHint Float
getTemperature = Val 44.6

toCelsius :: Val KelvinHint Float -> Val CelsiusHint Float
toCelsius (Val k) = Val $ k - 273.15

calculateSmth :: Val CelsiusHint Float -> Float
calculateSmth (Val c) = c * 100.0



calculateSmthElse :: Val KelvinHint Float -> Float
calculateSmthElse (Val c) = c + 100.0

