import Data.Fixed 

type Hsv = (Float,Float,Float)
type Rgb = (Float,Float,Float)

{-First Question-}
rgb2hsv :: Rgb -> Hsv
rgb2hsv (r,g,b)
    | (r > 255 || r < 0) || (g > 255 || g < 0) || (b > 255 || b < 0)  = error "Please give RGB values between 0 and 255"
    | otherwise = (calcHue (r/255 ,g/255,b/255),(calcSaturation (r/255,g/255,b/255)),(cmax (r/255,g/255,b/255)))
        where
            cmax :: Rgb->Float
            cmax (x,y,z) 
                | x >= y && x >= z = x
                | y >= z           = y
                | otherwise        = z
        
            cmin :: Rgb->Float
            cmin (x,y,z) 
                | x <= y && x <= z = x
                | y <= z           = y
                | otherwise        = z

            delta :: Float -> Float -> Float
            delta x y
                    | x < y = error "Subtraction from smaller number, so negative result occurs"
                    | otherwise = x - y 

            calcSaturation :: Rgb->Float
            calcSaturation (x,y,z) 
                            | cmax (x,y,z) == 0  = 0.0
                            | otherwise          = delta (cmax (x,y,z)) (cmin (x,y,z)) / cmax (x,y,z)


            calcHue :: Rgb->Float
            calcHue (x,y,z)
                    | delta (cmax (x,y,z)) (cmin (x,y,z)) == 0 = 0.0
                    | cmax (x,y,z) == x = (((y - z)/ delta (cmax (x,y,z)) (cmin (x,y,z))) `mod'` 6.0) * 60.0
                    | cmax (x,y,z) == y = (((z - x)/ delta (cmax (x,y,z)) (cmin (x,y,z)))+2.0) * 60.0
                    | cmax (x,y,z) == z = (((x - y)/ delta (cmax (x,y,z)) (cmin (x,y,z))) + 4.0) * 60.0

{- Second Question -}
hsv2rgb :: Hsv -> Rgb
hsv2rgb (h,s,v)
            | (h>=0 && h < 360) && (v>=0 && v<= 1) && (s>=0 && s<=1)= calculate (h,s,v) (cases (h,s,v))
            | otherwise = error "Plase check the h,s,v values if they are in range of 0<=h<360,0<=v<1,0<=s<1 "
                where
                    c :: Hsv -> Float
                    c (h,s,v) = v*s

                    x :: Hsv -> Float
                    x (h,s,v) = (c (h,s,v)) * (1- abs(((h/60) `mod'` 2)-1))

                    m :: Hsv -> Float
                    m (h,s,v)= v - (c (h,s,v))

                    cases :: Hsv -> Rgb
                    cases (h,s,v)
                        | (h>=0  && h < 60) = (c (h,s,v),x (h,s,v),0)
                        | (h>=60 && h < 120) = (x (h,s,v),c (h,s,v),0)
                        | (h>=120 && h < 180) = (0,c (h,s,v),x (h,s,v))
                        | (h>=180 && h < 240) = (0,x (h,s,v),c (h,s,v))
                        | (h>=240 && h < 300) = (x (h,s,v),0,c (h,s,v))
                        | (h>=300 && h < 360) = (c (h,s,v),0,x (h,s,v))

                    calculate ::Hsv -> Rgb -> Rgb
                    calculate (h,s,v) (r,g,b) = ((r+(m (h,s,v)))*255,(g+(m (h,s,v)))*255,(b+(m (h,s,v)))*255) 

{-Third Question-}

name2rgb :: String -> Rgb
name2rgb val = case val of 
                "Coral"->(255,127,80)
                "Tomato"->(255,99,71)
                "Orangered"->(255,69,0)
                "Gold"->(255,215,0)
                "Orange"->(255,165,0)
                "DarkOrange"->(255,140,0)
                "LightSalmon"->(255,160,122)
                "Salmon"->(250,128,114)
                "DarkSalmon"->(233,150,122)
                "LightCoral"->(240,128,128)
                "IndianRed"->(205,92,92)
                "Crimson"->(220,20,60)
                "FireBrick"->(178,34,34)
                "Red"->(255,0,0)
                "DarkRed"->(139,0,0)
                "LightYellow"->(255,255,224)
                "LemonChiffon"->(255,250,205)
                "LightGoldenRodYellow"->(250,250,210)
                "PapayaWhip"->(255,239,213)
                "Moccasin"->(255,228,181)
                "PeachPuff"->(255,218,185)
                "PaleGoldenRod"->(238,232,170)
                "Khaki"->(240,230,140)
                "DarkKhaki"->(189,183,107)
                "Yellow"->(255,255,0)
                "LawnGreen"->(124,252,0)
                "Chartreuse"->(127,255,0)
                "LimeGreen"->(50,205,50)
                "Lime"->(0,255,0)
                "ForestGreen"->(34,139,34)
                "Green"->(0,128,0)
                "DarkGreen"->(0,100,0)
                "GreenYellow"->(173,255,47)
                "YellowGreen"->(154,205,50)
                "SpringGreen"->(0,255,127)
                "MediumSpringGreen"->(0,250,154)
                "LightGreen"->(144,238,144)
                "PaleGreen"->(152,251,152)
                "DarkSeaGreen"->(143,188,143)
                "MediumSeaGreen"->(60,179,113)
                "SeaGreen"->(46,139,87)
                "Olive"->(128,128,0)
                "DarkOliveGreen"->(85,107,47)
                "OliveDrab"->(107,142,35)
                "LightCyan"->(224,255,255)
                "Cyan"->(0,255,255)
                "Aqua"->(0,255,255)
                "AquaMarine"->(127,255,212)
                "MediumAquaMarine"->(102,205,170)
                "PaleTurquoise"->(175,238,238)
                "Turquoise"->(64,224,208)
                "MediumTurquoise"->(72,209,204)
                "DarkTurquoise"->(0,206,209)
                "LightSeaGreen"->(32,178,170)
                "CadetBlue"->(95,158,160)
                "DarkCyan"->(0,139,139)
                "Teal"->(0,128,128)
                "PowderBlue"->(176,224,230)
                "LightBlue"->(173,216,230)
                "LightSkyBlue"->(135,206,250)
                "SkyBlue"->(135,206,235)
                "DeepSkyBlue"->(0,191,255)
                "LightSteelBlue"->(176,196,222)
                "DodgerBlue"->(30,144,255)
                "CornFlowerBlue"->(100,149,237)
                "SteelBlue"->(70,130,180)
                "RoyalBlue"->(65,105,225)
                "Blue"->(0,0,255)
                "MediumBlue"->(0,0,205)
                "DarkBlue"->(0,0,139)
                "Navy"->(0,0,128)
                "MidnightBlue"->(25,25,112)
                "MediumSlateBlue"->(123,104,238)
                "SlateBlue"->(106,90,205)
                "DarkSlateBlue"->(72,61,139)
                "Lavender"->(230,230,250)
                "Thistle"->(216,191,216)
                "Plum"->(221,160,221)
                "Violet"->(238,130,238)
                "Orchid"->(218,112,214)
                "Fuchsia"->(255,0,255)
                "Magenta"->(255,0,255)
                "MediumOrchid"->(186,85,211)
                "MediumPurple"->(147,112,219)
                "BlueViolet"->(138,43,226)
                "DarkViolet"->(148,0,211)
                "DarkOrchid"->(153,50,204)
                "DarkMagenta"->(139,0,139)
                "Purple"->(128,0,128)
                "Indigo"->(75,0,130)
                "Pink"->(255,192,203)
                "LightPink"->(255,182,193)
                "HotPink"->(255,105,180)
                "DeepPink"->(255,20,147)
                "PaleVioletRed"->(219,112,147)
                "MediumVioletRed"->(199,21,133)
                "White"->(255,255,255)
                "Snow"->(255,250,250)
                "HoneyDew"->(240,255,240)
                "MintCream"->(245,255,250)
                "Azure"->(240,255,255)
                "AliceBlue"->(240,248,255)
                "GhostWhite"->(248,248,255)
                "WhiteSmoke"->(245,245,245)
                "SeaShell"->(255,245,238)
                "Beige"->(245,245,220)
                "OldLace"->(253,245,230)
                "FloralWhite"->(255,250,240)
                "Ivory"->(255,255,240)
                "AntiqueWhite"->(250,235,215)
                "Linen"->(250,240,230)
                "LavenderBlush"->(255,240,245)
                "MistyRose"->(255,228,225)
                "Gainsboro"->(220,220,220)
                "LightGray"->(211,211,211)
                "Silver"->(192,192,192)
                "DarkGray"->(169,169,169)
                "Gray"->(128,128,128)
                "DimGray"->(105,105,105)
                "LightSlateGray"->(119,136,153)
                "SlateGray"->(112,128,144)
                "DarkSlateGray"->(47,79,79)
                "Black"->(0,0,0) 
                "Cornsilk"->(255,248,220)
                "BlanchedAlmond"->(255,235,205)
                "Bisque"->(255,228,196)
                "NavajoWhite"->(255,222,173)
                "Wheat"->(245,222,179)
                "BurlyWood"->(222,184,135)
                "Tan"->(210,180,140)
                "RosyBrown"->(188,143,143)
                "SandyBrown"->(244,164,96)
                "GoldenRod"->(218,165,32)
                "Peru"->(205,133,63)
                "Chocolate"->(210,105,30)
                "SaddleBrown"->(139,69,19)
                "Sienna"->(160,82,45)
                "Brown"->(165,42,42)
                "Maroon"->(128,0,0)
                _->error "There is NO HTML NAME like that !"

{-Fourth Question-}
hsvGradient :: Hsv -> Hsv -> Float -> [Hsv] 
hsvGradient (sh,ss,sv) (eh,es,ev) steps 
			| steps == 0 = (eh,es,ev) : []
			| ((sh>=0 && sh < 360) && (sv>=0 && sv<= 1) && (ss>=0 && ss<=1)) && ((eh>=0 && eh < 360) && (ev>=0 && ev<= 1) && (es>=0 && es<=1)) = ((sh,ss,sv) : []) ++ (hsvGradient ((interpolate sh eh (abs(sh-eh)/steps)),(interpolate ss es (abs(ss-es)/steps)),(interpolate sv ev (abs(sv-ev)/steps))) (eh,es,ev) (steps-1))
			| otherwise = error "Plase check the h,s,v values if they are in range of 0<=h<360,0<=v<1,0<=s<1 "
				where
					interpolate :: Float->Float->Float->Float
					interpolate s e dif
							| s > e = s - dif
							| otherwise =  s + dif
                            
{-Fifth Question-}
hsv2desc :: Hsv -> String 
hsv2desc (h,s,v)
            | (h>=0 && h < 360) && (v>=0 && v<= 1) && (s>=0 && s<=1) =  (hue h) ++ "," ++ (sat(calcS)) ++ " " ++ (light(calcL))
            | otherwise = error "Plase check the h,s,v values if they are in range of 0<=h<360,0<=v<1,0<=s<1 "
                where
                    calcL = (2-s) * v / 2
                    calcS = (s*v) / (if calcL < 0.5 then calcL*2 else 2-calcL*2)

                    hue :: Float -> String
                    hue val
                        | val>0 && val <= 15      ="red"
                        | val >  15 && val <= 45  = "orange"
                        | val >  45 && val <= 70  = "yellow"
                        | val >  70 && val <= 79  = "lime"
                        | val >  79 && val <= 163 = "green"
                        | val > 163 && val <= 193 = "cyan"
                        | val > 193 && val <= 240 = "blue"
                        | val > 240 && val <= 260 = "indigo"
                        | val > 260 && val <= 270 = "violet"
                        | val > 270 && val <= 291 = "purple"
                        | val > 291 && val <= 327 = "magenta"
                        | val > 327 && val <= 244 = "rose"
                        | val > 344 && val <= 360 = "red"   
                        
                    sat :: Float -> String
                    sat val
                        | val > 0.9                 = "very saturated"
                        | val > 0.8                 = "rather saturated"
                        | val > 0.6                 = "saturated"
                        | val > 0.46                = "rather unsaturated"
                        | val > 0.3                 = "unsaturated"
                        | val > 0.1                 = "very unsaturated"
                        | val > 0.03                = "almost grey"
                        | val <= 0.03               = "grey"
                        | otherwise                 = "grey"
                       
                        
                    light :: Float -> String
                    light val 
                        | val > 0.94                = "almost white"
                        | val > 0.8                 = "very light"
                        | val > 0.6                 = "light"
                        | val > 0.3                 = "normal"
                        | val > 0.22                = "dark"
                        | val < 0.1 && val > 0.09   = "very dark"
                        | val >= 0.0 && val <= 0.09 = "almost black"
                        | otherwise                 = "black"
                        
                        
                   

{-Sixth Question-}
myprogram :: String -> String -> Float -> [Hsv]
myprogram firstname secondname step = hsvGradient (rgb2hsv (name2rgb firstname)) (rgb2hsv (name2rgb secondname)) step 

{-
Seventh Question

closest :: Rgb -> String
closest (a,b,c)
            | a > 255 || (b<1 && b>0) || (c<1 && c>0) = closest(hsv2rgb (a,b,c))
            | otherwise = findmatches -> if no then make prediction

-}


