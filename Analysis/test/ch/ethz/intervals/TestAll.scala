package ch.ethz.intervals

import org.scalatest.SuperSuite

class TestAll extends SuperSuite(List(
    new TestAnalysis(),
    new TestPlugin()
))
