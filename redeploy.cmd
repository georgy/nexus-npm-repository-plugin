cmd /c "D:\tools\nexus\nexus-2.7.2-03\bin\nexus.bat stop"

cmd /c "mvn package"

del /q D:\tools\nexus\sonatype-work\nexus\plugin-repository\nexus-npm-repository-plugin-0.0.2-SNAPSHOT

xcopy /v /f /y target\nexus-npm-repository-plugin-0.0.2-SNAPSHOT-bundle.zip D:\tools\nexus\sonatype-work\nexus\plugin-repository\nexus-npm-repository-plugin-0.0.2-SNAPSHOT-bundle.zip

pushd D:\tools\nexus\sonatype-work\nexus\plugin-repository\

jar xf D:\tools\nexus\sonatype-work\nexus\plugin-repository\nexus-npm-repository-plugin-0.0.2-SNAPSHOT-bundle.zip

popd

del D:\tools\nexus\sonatype-work\nexus\logs\nexus.log

cmd /c "D:\tools\nexus\nexus-2.7.2-03\bin\nexus.bat start"