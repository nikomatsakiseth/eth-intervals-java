print "package harmonic.compiler;"
print ""
print "/* WARNING: Automatically generated! */"
print "class IntrinsicMathGen {"

mathTys = ["Byte", "Short", "Integer", "Long", "Float", "Double"]
ops = [("plus", "+"), ("minus", "-"), ("times", "*"), ("divide", "*")]

for (methodName, sigil) in ops:
    for leftTy in mathTys:
        for rightTy in mathTys:
            resultTy = mathTys[max(mathTys.index(leftTy), mathTys.index(rightTy), mathTys.index("Integer"))]
        
            print "    /* WARNING: Automatically generated! */"
            print "    %s %s(%s l, %s r) { return l %s r; }" % (
                resultTy, methodName, leftTy, rightTy, sigil
            )
            print ""

print "}"