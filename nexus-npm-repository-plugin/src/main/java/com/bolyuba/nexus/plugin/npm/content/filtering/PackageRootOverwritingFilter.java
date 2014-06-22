package com.bolyuba.nexus.plugin.npm.content.filtering;

/**
 * @author Georgy Bolyuba (georgy@bolyuba.com)
 */
public class PackageRootOverwritingFilter extends DecoratingJsonFilter {

    public PackageRootOverwritingFilter() {
        // last filter in the chain
        final DefaultJsonFilter defaultJsonFilter = new DefaultJsonFilter();

        // remove _id and _rev
        this.delegate = new NameRegexJsonFilter(defaultJsonFilter, "^_id|_rev$");
    }
}
