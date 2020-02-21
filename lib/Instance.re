open CoreTypes;

type opaque('node) = CoreTypes.opaqueInstance('node);

type opaqueInstanceUpdate('node, 'childNode) =
  Update.t('node, 'childNode, opaque('childNode));

type renderedElement('node, 'childNode) =
  Update.t('node, 'childNode, instanceForest('childNode));

let outputTreeNodes: type node. opaqueInstance(node) => lazyHostNodeSeq(node) =
  (Instance(instance)) => {
    switch (instance.component.childrenType) {
    | React => instance.wrappedHostNode
    | Host => Seq.((() => Cons(instance.wrappedHostNode, () => Nil)))
    };
  };


let rec ofComponent:
  type parentNode hooks node children childNode wrappedHostNode.
    (
      ~hostTreeState: Update.hostTreeState(parentNode, node),
      opaqueComponent(node),
      component((hooks, (node, children, childNode, wrappedHostNode)))
    ) =>
    opaqueInstanceUpdate(parentNode, node) =
  (~hostTreeState, opaqueComponent, component) => {
    let (children_, hooks) =
      component.render(
        Hooks.ofState(None, ~onStateDidChange=GlobalState.callStaleHandlers),
      );
    let hooks = Hooks.toState(hooks);
    let addMountEffects = u =>
      u
      |> Update.mapEffects(e =>
           EffectSequence.chain(
             Hooks.pendingEffects(
               ~lifecycle=Hooks.Effect.Mount,
               Some(hooks),
             ),
             e,
           )
         );
    switch (component.childrenType) {
    | React =>
      let update = ofElement(~hostTreeState, children_);
      update
      |> addMountEffects
      |> Update.map(childInstances =>
           Instance({
             hooks,
             opaqueComponent,
             component,
             children_,
             childInstances,
             wrappedHostNode: update.childNodes,
           })
         );
    | Host =>
      let update =
        ofElement(
          ~hostTreeState={
            nearestHostNode:
              lazy(
                Node(
                  children_.make()
                  |> children_.configureInstance(~isFirstRender=true),
                )
              ),
            nodeElement: children_,
            absoluteSubtreeIndex: 0,
          },
          children_.children,
        )
        |> addMountEffects;

      let node = update.hostTreeUpdate.nearestHostNode;
      let update =
        update
        |> Update.map(childInstances =>
             Instance({
               hooks,
               opaqueComponent,
               component,
               children_,
               childInstances,
               wrappedHostNode: node,
             })
           );

      let hostTreeUpdate = {
        ...hostTreeState,
        nearestHostNode:
          lazy(
            SubtreeChange.insertNodes(
              ~nodeElement=hostTreeState.nodeElement,
              ~parent=Lazy.force(hostTreeState.nearestHostNode),
              ~children=
                [update.hostTreeUpdate.nearestHostNode] |> List.to_seq,
              ~position=hostTreeState.absoluteSubtreeIndex,
            )
          ),
        absoluteSubtreeIndex: hostTreeState.absoluteSubtreeIndex + 1,
      };
      {
        hostTreeUpdate,
        enqueuedEffects: update.enqueuedEffects,
        payload: update.payload,
        childNodes: Seq.((() => Cons(node, () => Nil))),
      };
    };
  }

and ofOpaqueComponent:
  type parentNode node.
    (
      ~hostTreeState: Update.hostTreeState(parentNode, node),
      ~component: opaqueComponent(node)
    ) =>
    opaqueInstanceUpdate(parentNode, node) =
  (
    ~hostTreeState,
    ~component as OpaqueComponent(component) as opaqueComponent,
  ) =>
    ofComponent(~hostTreeState, opaqueComponent, component)

and ofElement:
  type parentNode node.
    (
      ~hostTreeState: Update.hostTreeState(parentNode, node),
      element(node)
    ) =>
    renderedElement(parentNode, node) =
  (~hostTreeState, syntheticElement) =>
    Element.toRenderedElement(
      ~mapper=ofOpaqueComponent,
      ~init=hostTreeState,
      syntheticElement,
    );

module Forest = {
  type t('node) = CoreTypes.instanceForest('node);

  let rec fold:
    type any. (~f: ('acc, opaque(any)) => 'acc, 'a, t(any)) => 'acc =
    (~f, acc, instanceForest) => {
      switch (instanceForest) {
      | IFlat(opaqueInstance) => f(acc, opaqueInstance)
      | INested(l, _) =>
        List.fold_left(
          (acc, instanceForest) => fold(~f, acc, instanceForest),
          acc,
          l,
        )
      | IDiffableSequence(l, _) =>
        Seq.fold_left(
          (acc, instanceForest) => fold(~f, acc, instanceForest),
          acc,
          l.toSeq(),
        )
      };
    };

  let pendingEffects = (~lifecycle, instanceForest) => {
    let f = (acc, CoreTypes.Instance({hooks})) =>
      EffectSequence.chain(
        Hooks.pendingEffects(~lifecycle, Some(hooks)),
        acc,
      );
    let rec fold: type any. (EffectSequence.t, t(any)) => EffectSequence.t =
      (acc, instanceForest) => {
        switch (instanceForest) {
        | IFlat(Instance({childInstances}) as opaqueInstance) =>
          f(fold(acc, childInstances), opaqueInstance)
        | INested(l, _) =>
          List.fold_left(
            (acc, instanceForest) => fold(acc, instanceForest),
            acc,
            l,
          )
        | IDiffableSequence(l, _) =>
          Seq.fold_left(
            (acc, instanceForest) => fold(acc, instanceForest),
            acc,
            l.toSeq(),
          )
        };
      };
    fold(EffectSequence.noop, instanceForest);
  };

  let outputTreeNodes = forest => {
    fold(
      ~f=
        (l, opaqueInstance) =>
          [outputTreeNodes(opaqueInstance) |> List.of_seq, ...l],
      [],
      forest,
    )
    |> List.rev
    |> List.flatten
    |> List.to_seq;
  };
};

let pendingEffects =
    (~lifecycle, ~nextEffects, ~instance as {childInstances, hooks}) => {
  EffectSequence.chain(
    Forest.pendingEffects(~lifecycle, childInstances)
    |> EffectSequence.chain(Hooks.pendingEffects(~lifecycle, Some(hooks))),
    nextEffects,
  );
};
