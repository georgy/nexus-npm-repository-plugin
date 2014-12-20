<!--

    Copyright (c) 2007-2014 Sonatype, Inc. and Georgy Bolyuba. All rights reserved.

    This program is licensed to you under the Apache License Version 2.0,
    and you may not use this file except in compliance with the Apache License Version 2.0.
    You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.

    Unless required by applicable law or agreed to in writing,
    software distributed under the Apache License Version 2.0 is distributed on an
    "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.

-->
# Nexus NPM Plugin

The plugin that adds NPM (https://www.npmjs.org) repository type support for Nexus 2.10+

The plugin is based on original work from this repository
https://github.com/georgy/nexus-npm-repository-plugin

## Branches

These are the relevant branches

* "nexus-2.10.x" local production, receives merges from sonatype-master (CI built, bundled with NX) https://github.com/sonatype/nexus-npm-repository-plugin/tree/nexus-2.10.x

Other branches:

* "master" synced to upstream https://github.com/sonatype/nexus-npm-repository-plugin/tree/master

## Workflow

Branch off "nexus-2.10.x" for new work, and create PRs against same.

Do not touch/use branches "master" and other branches.

## Features
Current features covers complete "roundtrip" for development, by caching, hosting and publishing NPM packages.
Current features include:
* Hosted (aka "private registry") repository support
* Proxy repository support (proxies and caches remote registries)
* Group repository support (aggregates multiple NPM repositories and publishes via single URL)

## Recommended Nexus setup
Simplest and recommended setup covers the required cycle of consuming and publishing NPM packages.

Steps:
 * Install NPM plugin to OSS or Pro Nexus (this step will be not needed after Nexus 2.10 release)
 * Check Plugin console. NPM plugin should be activated:
 ![plugin console](https://github.com/georgy/nexus-npm-repository-plugin/raw/master/site/plugin-console.png)
 * Create a Hosted repository (set provider to "NPM") to host packages you publish (below will call it "npmhosted")
 * Create a Proxy repository to proxy NPM registry (set provider to "NPM" and remote location to https://registry.npmjs.org). This step can be repeated as many times as many registries you want to use.
 ![proxy config](https://github.com/georgy/nexus-npm-repository-plugin/raw/master/site/proxy-config.png)
 * Create a Group repository to aggregate all the repositories you created above (let's call it "npmgroup").

## Recommended NPM CLI setup
Now you need to point NPM at the URLs available from your Nexus. If the repositories created in
Nexus setup above are called "npmhosted", "npmproxy" and "npmgroup" respectively, your setup would
be following:

In `.npmrc` make sure following lines are present (fix the URL host part to point to real location in case you are not running
Nexus on localhost):

```
config = 0
registry = http://localhost:8081/nexus/content/groups/npmgroup/
init.author.name = My Name
init.author.email = my@email.com
init.author.url = http://my.blog
email=my@email.com
_auth=ZGVwbG95bWVudDpkZXBsb3ltZW50MTIz
```

The `_auth` part is important, it should contain *valid Nexus username and password* Base64 encoded to authenticate
with. The value above contains the default "deployment" user that comes out of the box with new
deployments of Nexus, so for testing purposes, you can copy just that.

## Recommended package setup
To make your package land properly (erm, "be published") of your Nexus instance, you need to override
the `publishConfig`. To do that, add the following to `package.json`:

```
  "publishConfig" : {
    "registry" : "http://localhost:8081/nexus/content/repositories/npmhosted/"
  },
```

## Hack away!
Having this setup, you can now fetch/install packages and also publish the package you want
to your Nexus instance.

Have fun,  
NPM Plugin Team
