export interface InternalLocation {
  readonly directory: string;
  readonly workspaceID?: string;
}

export interface PublicLocation {
  readonly directory: string;
}

function samePublicLocation(
  left: PublicLocation,
  right: PublicLocation,
): boolean {
  return left.directory === right.directory;
}

export function eventBelongsToLocation(
  pluginLocation: InternalLocation,
  eventLocation: PublicLocation | undefined,
  sessionLocation: PublicLocation,
): boolean {
  return (
    samePublicLocation(sessionLocation, pluginLocation) &&
    (eventLocation === undefined ||
      samePublicLocation(eventLocation, pluginLocation))
  );
}
