package com.bolyuba.nexus.plugin.npm.metadata.internal.orient;

import javax.inject.Inject;
import javax.inject.Named;
import javax.inject.Singleton;

import org.sonatype.nexus.events.EventSubscriber;
import org.sonatype.nexus.proxy.events.NexusInitializedEvent;
import org.sonatype.nexus.proxy.events.NexusStoppedEvent;
import org.sonatype.sisu.goodies.common.ComponentSupport;

import com.google.common.eventbus.Subscribe;
import com.orientechnologies.orient.core.Orient;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * {@link EventSubscriber} implementation that drives OrientMetadataStore lifecycle.
 */
@Singleton
@Named
public class OrientMetadataStoreLifecycle
    extends ComponentSupport
    implements EventSubscriber
{
  private final OrientMetadataStore orientMetadataStore;

  @Inject
  public OrientMetadataStoreLifecycle(
      final OrientMetadataStore orientMetadataStore)
  {
    this.orientMetadataStore = checkNotNull(orientMetadataStore);
    Orient.instance().removeShutdownHook();
  }


  @Subscribe
  public void on(final NexusInitializedEvent e) throws Exception {
    orientMetadataStore.start();
  }

  @Subscribe
  public void on(final NexusStoppedEvent e) throws Exception {
    orientMetadataStore.stop();
    Orient.instance().shutdown();
  }
}
