package com.sun.source.tree;

import checkers.javari.quals.*;

public interface SynchronizedTree extends StatementTree {
    @PolyRead ExpressionTree getExpression() @PolyRead;
    @PolyRead BlockTree getBlock() @PolyRead;
}
