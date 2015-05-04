name := "harris-app"

version := "0.1"

CommonSettings.settings

libraryDependencies += "commons-io" % "commons-io" % "2.4" % "compile, runtime"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"

libraryDependencies += "com.typesafe.slick" %% "slick" % "3.0.0"



artifactName in packageBin := ((_, m, _) => name.value + "-" + version.value + ".jar")


ProguardSettings.settings

javaOptions in (Proguard, ProguardKeys.proguard) := Seq("-Xmx4G")

ProguardKeys.options in Proguard += ProguardOptions.keepMain("feh.tec.cvis.HarrisApp")

//ProguardKeys.proguardVersion in Proguard := "5.2.1"

ProguardKeys.options in Proguard ++= Seq( "-dontnote"
                                        , "-dontwarn"
                                        , "-ignorewarnings"
                                        , "-dontobfuscate"
//                                        , "-keep,allowshrinking,allowobfuscation interface scala.Specializable" // http://sourceforge.net/p/proguard/bugs/487/
                                        , "-optimizations !class/merging/*" // http://sourceforge.net/p/proguard/bugs/487/
//                                        , ""
//                                        , ""
                                          )
