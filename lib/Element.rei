type t('node) = CoreTypes.element('node);

let toRenderedElement:
  (
    ~mapper: (
          ~hostTreeState: Update.hostTreeState('a, 'b),
          ~component: CoreTypes.opaqueComponent('c)
        ) =>
        Update.t('a, 'b, CoreTypes.opaqueInstance('c)),
    ~init: Update.hostTreeState('a, 'b),
    t('c)
  ) =>
  Update.t('a, 'b, CoreTypes.instanceForest('c));