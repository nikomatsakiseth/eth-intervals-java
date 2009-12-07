import sbt._

class Intervals(info: ProjectInfo) extends DefaultProject(info)
{  
    override def dependencyPath = "lib"
    override def mainScalaSourcePath = "src"
    override def mainJavaSourcePath = "src"
    override def mainResourcesPath = "resources"    
    override def testScalaSourcePath = "test"
    override def testJavaSourcePath = "test"
    override def testResourcesPath = "test-resources"    
    override def compileOptions = Unchecked :: super.compileOptions.toList
}

