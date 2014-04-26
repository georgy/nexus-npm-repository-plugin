nexus-npm-repository-plugin
===========================

Sonatype Nexus OSS plugin for npm (https://www.npmjs.org) repository support

0.0.1 aka MVP aka dumb proxy:
 + Proxy requests to one registry
 + Parses JSON and replaces all tarball URLs with URLs pointing to itself (i.e. tarballs will be delivered via same proxy repo)

Usage:
 * Install Nexus OSS (or Pro), add plugin to sonatype-work/nexus/plugin-repository (better to unpack the zip yourself)
 * Restart Nexus
 * Login as admin
 * Check Plugin console. NPM plugin should be activated:
 ![plugin console](https://github.com/georgy/nexus-npm-repository-plugin/raw/master/src/site/plugin-console.png)(#)
 * Go to list of repositories and add new proxy repository
 * Set remote location to [https://registry.npmjs.org](https://registry.npmjs.org)
 * Set Provider to "Npm plugin"
 ![proxy config](https://github.com/georgy/nexus-npm-repository-plugin/raw/master/src/site/proxy-config.png)(#)
 * Configure your npm:

        $ npm config set registry http://localhost:8081/nexus/content/npm/registry.npmjs.org/

 Note: URL will depend on host/port of your nexus instance and ID of your proxy repo

