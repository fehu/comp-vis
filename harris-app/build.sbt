name := "harris-app"

version := "0.2-SNAPSHOT"

CommonSettings.settings

libraryDependencies += "commons-io" % "commons-io" % "2.4" % "compile, runtime"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"

libraryDependencies += "com.typesafe.slick" %% "slick" % "3.0.0"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"

libraryDependencies += "com.h2database" % "h2" % "1.4.187"

libraryDependencies += "com.zaxxer" % "HikariCP" % "2.3.3"



artifactName in packageBin := ((_, m, _) => name.value + "-" + version.value + ".jar")


ProguardSettings.settings

javaOptions in (Proguard, ProguardKeys.proguard) := Seq("-Xmx4G")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("feh.tec.cvis.HarrisApp")

ProguardKeys.libraries in Proguard := (ProguardKeys.libraries in Proguard).value.filter(_.getName != "opencv-300.jar")

ProguardKeys.proguardVersion in Proguard := "5.2.1"

ProguardKeys.options in Proguard ++= Seq( "-dontnote"
                                        , "-dontwarn"
                                        , "-ignorewarnings"
                                        , "-dontobfuscate"
                                        , "-optimizations !class/merging/*" // http://sourceforge.net/p/proguard/bugs/487/
                                        //
//                                        , "-keep class breeze.stats.** { *** n(); }" // wild card seems to cause error
                                        , """-keep class breeze.stats.DescriptiveStatsTrait$meanAndVariance$$anon$7$$anon$22 {
                                            |    public long   n();
                                            |    public double mu();
                                            |    public double s();
                                            |}""".stripMargin
                                        ,"""-keep class breeze.stats.DescriptiveStatsTrait$accumulateAndCount$$anon$1$$anon$20 {
                                            |    public double sum();
                                            |    public int    n();
                                            |}""".stripMargin
                                        //
                                        // from http://proguard.sourceforge.net/manual/examples.html
                                        , "-keepclassmembers class * { ** MODULE$; }"
//
                                        , """-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinPool {
                                            |    long eventCount;
                                            |    long stealCount;
                                            |    int  workerCounts;
                                            |    int  runControl;
                                            |    int  indexSeed;
                                            |    scala.concurrent.forkjoin.ForkJoinPool$WaitQueueNode syncStack;
                                            |    scala.concurrent.forkjoin.ForkJoinPool$WaitQueueNode spareStack;
                                            |}""".stripMargin
                                        , """-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinWorkerThread {
                                            |    int base;
                                            |    int sp;
                                            |    int runState;
                                            |}""".stripMargin
                                        , """-keepclassmembernames class scala.concurrent.forkjoin.ForkJoinTask {
                                            |    int status;
                                            |}""".stripMargin
                                        , """-keepclassmembernames class scala.concurrent.forkjoin.LinkedTransferQueue {
                                            |    scala.concurrent.forkjoin.LinkedTransferQueue$PaddedAtomicReference head;
                                            |    scala.concurrent.forkjoin.LinkedTransferQueue$PaddedAtomicReference tail;
                                            |    scala.concurrent.forkjoin.LinkedTransferQueue$PaddedAtomicReference cleanMe;
                                            |}""".stripMargin
                                          )
