cmd /c "D:\tools\nexus\nexus-2.7.2-03\bin\nexus.bat stop"

cmd /c "mvn package"

xcopy /v /f /y target\nexus-npm-repository-plugin-0.0.1-SNAPSHOT.jar D:\tools\nexus\sonatype-work\nexus\plugin-repository\nexus-npm-repository-plugin-0.0.1-SNAPSHOT

del D:\tools\nexus\sonatype-work\nexus\logs\nexus.log

cmd /c "D:\tools\nexus\nexus-2.7.2-03\bin\nexus.bat start"