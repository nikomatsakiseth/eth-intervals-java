#!/bin/bash

# Adjust for your system:
INSERTANNOTATIONS="$HOME/proj/jsr308/annotation-tools/annotation-file-utilities/scripts/insert-annotations"

# Expand to individual .class files in directory tmp:
mkdir -p tmp
cd tmp
jar xf ../java_standard_library.jar
cd ..

# Use annotation tool to edit in place:
cd jdk_annotations
for PKGJAIF in *.jaif
do
    PKG=${PKGJAIF%.jaif}        # Strip .jaif from end
    PKGDIR=${PKG//.//}          # Replace all . with /
    find "../tmp/$PKGDIR" -name '*.class' -exec $INSERTANNOTATIONS {} $PKGJAIF \;
done
cd ..

# Create new .jar file and erase temporary directory
cd tmp
jar cf ../annotated_java_standard_library.jar *
cd ..
rm -rf tmp
