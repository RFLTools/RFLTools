# Common

AutoLisp General Common Routines

Purpose:  Collection of common routines

RFL:STAPOS : The '+' position of the station text string, 3 = 0+000 / 2 = 0+00

(RFL:STATXT x) : Returns a text string representing the station of a integer/real x

(RFL:ACADVER) : Returns the AutoCAD version number

(RFL:GETC3DSURFACE) : Returns the Civil 3D surface either selected or chosen

(RFL:GETSURFACELINE P1 P2 OBSURFACE) : Returns a point list of surface points between P1 and P2 for OBSURFACE

(RFL:GETSURFACEPOINT P OBSURFACE) : Returns the elevation of point P on OBSURFACE

(RFL:GETC3DALIGNMENT) : Returns the Civil 3D alignment either selected or chosen

(RFL:GETSECTIONSET STASTART STAEND SWATH STEP OBSURFACE ALIGNLIST) : Returns a set of sections

C:FIX+ : Updates a selected text entity such that the '+' location is centered over it's insertion point (horizontal only)

(RFL:FIX+ ENT) : Updates ENT text entity such that the '+' location is centered over it's insertion point (horizontal only)

(RFL:GETPLIST ENT) : Returns a list of '10' code points for ENT (polyline or lwpolyline)

(RFL:POINTINSIDE P PLIST) : Returns T if P is inside 2D bounding PLIST, nil if not

(RFL:MOD A B) : Returns the modulus of A and B (similar to rem function but works more like orther languages for negative values
