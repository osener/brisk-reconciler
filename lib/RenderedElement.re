open CoreTypes;

type t('node, 'childNode) = Instance.renderedElement('node, 'childNode);
type root('node, 'childNode) = {
  node: 'node,
  insertNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  deleteNode: (~parent: 'node, ~child: 'childNode, ~position: int) => 'node,
  moveNode:
    (~parent: 'node, ~child: 'childNode, ~from: int, ~to_: int) => 'node,
};

let render = (root, children) => {
  let hostTreeState = {
    Update.nodeElement: {
      make: () => root.node,
      configureInstance: (~isFirstRender as _, i) => i,
      children,
      insertNode: root.insertNode,
      deleteNode: root.deleteNode,
      moveNode: root.moveNode,
    },
    nearestHostNode: lazy(Node(root.node)),
    absoluteSubtreeIndex: 0,
  };
  Instance.ofElement(~hostTreeState, children);
};
let update =
    (~renderedElement as {Update.payload, hostTreeUpdate}, nextElement) =>
  Reconciler.updateInstanceForest(
    ~updateContext=
      Update.{
        hostTreeState: hostTreeUpdate,
        shouldExecutePendingUpdates: false,
      },
    ~oldInstanceForest=payload,
    ~nextElement,
    (),
  );

let rec map = (f, renderedElement, hostTreeState) =>
  switch (renderedElement) {
  | IFlat(e) =>
    f(~hostTreeState, e)
    |> Update.map(opaqueInstance => {
         let unchanged = e === opaqueInstance;
         unchanged ? renderedElement : IFlat(opaqueInstance);
       })
  | INested(l, _) =>
    let update =
      List.fold_left(
        (acc, renderedElement) => {
          let update = map(f, renderedElement, acc.Update.hostTreeUpdate);
          {
            ...update |> Update.map(next => [next, ...acc.Update.payload]),
            enqueuedEffects:
              EffectSequence.chain(
                acc.enqueuedEffects,
                update.enqueuedEffects,
              ),
          };
        },
        {
          Update.payload: [],
          hostTreeUpdate: hostTreeState,
          enqueuedEffects: EffectSequence.noop,
          childNodes: Seq.empty,
        },
        List.rev(l),
      );
    update
    |> Update.map(payload => {
         let unchanged = List.for_all2((===), l, payload);
         unchanged
           ? renderedElement
           : INested(payload, update.hostTreeUpdate.absoluteSubtreeIndex);
       });

  | IDiffableSequence(instances, _l) =>
    instances.toSeq()
    |> Seq.fold_left(
         (acc, _instance) => {acc},
         {
           Update.payload: instances,
           hostTreeUpdate: hostTreeState,
           enqueuedEffects: EffectSequence.noop,
           childNodes: Seq.empty,
         },
       )
    |> Update.map(instances => IDiffableSequence(instances, 0))
  };

/**
      * Flush the pending updates in an instance tree.
      */
let flushPendingUpdates =
    ({Update.payload: instanceForest, hostTreeUpdate, enqueuedEffects}) => {
  let update =
    map(Reconciler.flushPendingUpdates, instanceForest, hostTreeUpdate);
  {
    ...update,
    enqueuedEffects:
      EffectSequence.chain(update.enqueuedEffects, enqueuedEffects),
  };
};

let executeHostViewUpdates =
    ({Update.hostTreeUpdate: {nearestHostNode}}: t(_, _)) => {
  let Node(hostView) | UpdatedNode(_, hostView) =
    Lazy.force(nearestHostNode);
  hostView;
};

let executePendingEffects = ({enqueuedEffects} as renderedElement: t(_, _)) => {
  enqueuedEffects();
  {...renderedElement, enqueuedEffects: EffectSequence.noop};
};
