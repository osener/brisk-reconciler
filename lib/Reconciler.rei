let updateInstanceForest:
  (
    ~updateContext: Update.context('parentNode, 'node),
    ~oldInstanceForest: CoreTypes.instanceForest('node),
    ~nextElement: Element.t('node),
    unit
  ) =>
  Instance.renderedElement('parentNode, 'node);

let flushPendingUpdates:
  (
    ~hostTreeState: Update.hostTreeState('parentNode, 'node),
    CoreTypes.opaqueInstance('node),
  ) =>
  Instance.opaqueInstanceUpdate('parentNode, 'node);
