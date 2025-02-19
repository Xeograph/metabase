import { t } from "ttag";
import { Collection } from "metabase-types/api";

export type Item = {
  id: number;
  model: string;
  name: string;
  description: string | null;
  copy?: boolean;
  collection_position?: number | null;
  collection_preview?: boolean | null;
  has_required_parameters?: boolean | null;
  getIcon: () => { name: string };
  getUrl: () => string;
  setArchived: (isArchived: boolean) => void;
  setPinned: (isPinned: boolean) => void;
  setCollection?: (collection: Collection) => void;
  setCollectionPreview?: (isEnabled: boolean) => void;
};

export function nonPersonalOrArchivedCollection(
  collection: Collection,
): boolean {
  // @TODO - should this be an API thing?
  return !isPersonalCollection(collection) && !collection.archived;
}

export function isPersonalCollection(collection: Collection): boolean {
  return typeof collection.personal_owner_id === "number";
}

// Replace the name for the current user's collection
// @Question - should we just update the API to do this?
function preparePersonalCollection(c: Collection): Collection {
  return {
    ...c,
    name: t`Your personal collection`,
    originalName: c.name,
  };
}

// get the top level collection that matches the current user ID
export function currentUserPersonalCollections(
  collectionList: Collection[],
  userID: number,
): Collection[] {
  return collectionList
    .filter(l => l.personal_owner_id === userID)
    .map(preparePersonalCollection);
}

function getNonRootParentId(collection: Collection) {
  if (Array.isArray(collection.effective_ancestors)) {
    // eslint-disable-next-line no-unused-vars
    const [root, nonRootParent] = collection.effective_ancestors;
    return nonRootParent ? nonRootParent.id : undefined;
  }
  // location is a string like "/1/4" where numbers are parent collection IDs
  const nonRootParentId = collection.location?.split("/")?.[0];
  return canonicalCollectionId(nonRootParentId);
}

export function isPersonalCollectionChild(
  collection: Collection,
  collectionList: Collection[],
): boolean {
  const nonRootParentId = getNonRootParentId(collection);
  if (!nonRootParentId) {
    return false;
  }
  const parentCollection = collectionList.find(c => c.id === nonRootParentId);
  return Boolean(parentCollection && !!parentCollection.personal_owner_id);
}

export function isRootCollection(collection: Collection): boolean {
  return collection.id === "root";
}

export function isItemPinned(item: Item) {
  return item.collection_position != null;
}

export function isPreviewShown(item: Item) {
  return isPreviewEnabled(item) && hasRequiredParameters(item);
}

export function isPreviewEnabled(item: Item) {
  return item.collection_preview ?? true;
}

export function hasRequiredParameters(item: Item) {
  return item.has_required_parameters ?? true;
}

// API requires items in "root" collection be persisted with a "null" collection ID
// Also ensure it's parsed as a number
export function canonicalCollectionId(
  collectionId: string | number | null | undefined,
): number | null {
  if (collectionId === "root" || collectionId == null) {
    return null;
  } else if (typeof collectionId === "number") {
    return collectionId;
  } else {
    return parseInt(collectionId, 10);
  }
}
