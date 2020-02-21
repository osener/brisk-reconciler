open CoreTypes;

type subtreeUpdate('node) =
  | MatchingSubtree(list(subtreeUpdate('node)))
  | Update(opaqueInstance('node), opaqueComponent('node))
  | ReRender(instanceForest('node), element('node))
  | UpdateSequence(
      dynamicElement('node, instanceForest('node)),
      dynamicElement('node, element('node)),
    )
  | DeleteMovableInstanceForest(instanceForest('node), element('node))
  | InsertMovableInstanceForest(instanceForest('node), element('node));

let rec prepareUpdate = (~oldInstanceForest, ~nextElement) => {
  switch (oldInstanceForest, nextElement) {
  | (IFlat(instance), Leaf(component)) => Update(instance, component)
  | (INested(instances, _), StaticList(elements))
      when List.length(instances) == List.length(elements) =>
    MatchingSubtree(
      List.map2(
        (inst, elem) =>
          prepareUpdate(~oldInstanceForest=inst, ~nextElement=elem),
        instances,
        elements,
      ),
    )
  | (IDiffableSequence(instances, _), DiffableSequence(elements)) =>
    UpdateSequence(instances, elements)
  | (
      IMovable(instanceForest, instanceRef),
      Movable(nextElement, elementRef),
    )
      when instanceRef === elementRef =>
    prepareUpdate(~oldInstanceForest, ~nextElement)
  | (
      IMovable(instanceForest, instanceRef),
      Movable(nextElement, elementRef),
    ) =>
    InsertedAndDeletedLOL(instanceForest, nextElement)
  | (IMovable(instanceForest, _), _) =>
    DeleteMovableInstanceForest(instanceForest, nextElement)
  | (instanceForest, Movable(nextElement, elementRef)) =>
    InsertMovableInstanceForest(instanceForest, nextElement)

  | _ => ReRender(oldInstanceForest, nextElement)
  };
};


/**
  * Initial render of an Element. Recurses to produce the entire tree of
  * instances.
  */
let rec renderElement:
  type parentNode node.
    (
      ~updateContext: Update.context(parentNode, node),
      opaqueComponent(node)
    ) =>
    Instance.opaqueInstanceUpdate(parentNode, node) =
  (~updateContext, opaqueComponent) =>
    Instance.ofOpaqueComponent(
      ~hostTreeState=updateContext.hostTreeState,
      ~component=opaqueComponent,
    )

and renderReactElement:
  type parentNode node.
    (~updateContext: Update.context(parentNode, node), element(node)) =>
    Instance.renderedElement(parentNode, node) =
  (~updateContext, element) =>
    Element.toRenderedElement(
      ~mapper=
        (~hostTreeState, ~component) => {
          renderElement(
            ~updateContext={...updateContext, hostTreeState},
            component,
          )
        },
      ~init=updateContext.hostTreeState,
      element,
    )

and updateOpaqueInstance:
  type node parentNode.
    (
      ~updateContext: Update.context(parentNode, node),
      opaqueInstance(node),
      opaqueComponent(node)
    ) =>
    Instance.opaqueInstanceUpdate(parentNode, node) =
  (
    ~updateContext,
    Instance(instance) as originalOpaqueInstance,
    OpaqueComponent(nextComponent) as nextOpaqueComponent,
  ) => {
    let nextState =
      updateContext.shouldExecutePendingUpdates
        ? Hooks.flushPendingStateUpdates(instance.hooks) : instance.hooks;
    let stateChanged = nextState !== instance.hooks;

    let bailOut =
      !stateChanged && instance.opaqueComponent === nextOpaqueComponent;

    if (bailOut && !updateContext.shouldExecutePendingUpdates) {
      {
        hostTreeUpdate: updateContext.hostTreeState,
        payload: originalOpaqueInstance,
        enqueuedEffects: EffectSequence.noop,
        childNodes: Instance.outputTreeNodes(originalOpaqueInstance),
      };
    } else {
      let {component} = instance;
      switch (
        nextComponent.eq(
          {...instance, hooks: nextState},
          component.id,
          nextComponent.id,
        )
      ) {
      /*
       * Case A: The nextComponent is the same as component.
       */
      | Some(handedInstance) =>
        let {Update.hostTreeUpdate, payload: newOpaqueInstance} as ret =
          updateInstance(
            ~originalOpaqueInstance,
            ~updateContext,
            ~nextComponent,
            ~nextOpaqueComponent,
            ~stateChanged,
            handedInstance,
          );
        newOpaqueInstance === originalOpaqueInstance
          ? ret
          : {
            ...ret,
            hostTreeUpdate: {
              ...hostTreeUpdate,
              nearestHostNode:
                SubtreeChange.updateNodes(
                  ~nodeElement=hostTreeUpdate.nodeElement,
                  ~parent=hostTreeUpdate.nearestHostNode,
                  ~children=Instance.outputTreeNodes(newOpaqueInstance),
                  ~position=hostTreeUpdate.absoluteSubtreeIndex,
                ),
            },
          };
      | None =>
        /**
          * ** Switching component type **
          */

        let hostTreeState = {
          ...updateContext.hostTreeState,
          nearestHostNode:
            SubtreeChange.deleteNodes(
              ~nodeElement=updateContext.hostTreeState.nodeElement,
              ~parent=updateContext.hostTreeState.nearestHostNode,
              ~children=Instance.outputTreeNodes(originalOpaqueInstance),
              ~position=updateContext.hostTreeState.absoluteSubtreeIndex,
            ),
        };

        Instance.ofOpaqueComponent(
          ~hostTreeState,
          ~component=nextOpaqueComponent,
        )
        |> Update.mapEffects(mountEffects =>
             Instance.pendingEffects(
               ~lifecycle=Hooks.Effect.Unmount,
               ~nextEffects=mountEffects,
               ~instance,
             )
           );
      };
    };
  }

and updateInstance:
  type hooks node children childNode wrappedHostNode parentNode.
    (
      ~originalOpaqueInstance: opaqueInstance(node),
      ~updateContext: Update.context(parentNode, node),
      ~nextComponent: component(
                        (
                          hooks,
                          (node, children, childNode, wrappedHostNode),
                        ),
                      ),
      ~nextOpaqueComponent: opaqueComponent(node),
      ~stateChanged: bool,
      instance((hooks, (node, children, childNode, wrappedHostNode)))
    ) =>
    Instance.opaqueInstanceUpdate(parentNode, node) =
  (
    ~originalOpaqueInstance,
    ~updateContext,
    ~nextComponent,
    ~nextOpaqueComponent,
    ~stateChanged,
    instance,
  ) => {
    let updatedInstanceWithNewElement = {
      ...instance,
      component: nextComponent,
      opaqueComponent: nextOpaqueComponent,
    };

    let shouldRerender =
      stateChanged || nextOpaqueComponent !== instance.opaqueComponent;

    let (nextSubElements, initialHooks) =
      if (shouldRerender) {
        let (nextElement, initialHooks) =
          nextComponent.render(
            Hooks.ofState(
              Some(updatedInstanceWithNewElement.hooks),
              ~onStateDidChange=GlobalState.callStaleHandlers,
            ),
          );
        (nextElement, Hooks.toState(initialHooks));
      } else {
        (instance.children_, instance.hooks);
      };

    let updatedInstanceWithNewState = {
      ...updatedInstanceWithNewElement,
      hooks: initialHooks,
    };

    let {childInstances} = updatedInstanceWithNewState;
    let (
      nearestHostNode: lazyHostNode(parentNode),
      updatedInstanceWithNewSubtree,
      enqueuedEffects,
      nodeElement: hostNodeElement(parentNode, node),
    ) =
      switch (nextComponent.childrenType) {
      | React =>
        let {Update.payload: nextInstanceSubForest, enqueuedEffects} =
          updateInstanceForest(
            ~updateContext,
            ~oldInstanceForest=childInstances,
            ~nextElement=nextSubElements,
            (),
          );
        nextInstanceSubForest !== childInstances
          ? (
            updateContext.hostTreeState.nearestHostNode,
            {
              ...updatedInstanceWithNewState,
              children_: nextSubElements,
              childInstances: nextInstanceSubForest,
            }:
              instance(
                (hooks, (node, children, childNode, wrappedHostNode)),
              ),
            enqueuedEffects,
            updateContext.hostTreeState.nodeElement,
          )
          : (
            updateContext.hostTreeState.nearestHostNode,
            updatedInstanceWithNewState,
            enqueuedEffects,
            updateContext.hostTreeState.nodeElement,
          );
      | Host =>
        let instanceWithNewHostView:
          instance((hooks, (node, children, childNode, wrappedHostNode))) =
          shouldRerender
            ? {
              ...updatedInstanceWithNewState,
              wrappedHostNode:
                lazy({
                  let instance =
                    Lazy.force(updatedInstanceWithNewState.wrappedHostNode);
                  let Node(beforeUpdate) | UpdatedNode(_, beforeUpdate) = instance;
                  let afterUpdate =
                    nextSubElements.configureInstance(
                      ~isFirstRender=false,
                      beforeUpdate,
                    );
                  afterUpdate === beforeUpdate
                    ? instance : UpdatedNode(beforeUpdate, afterUpdate);
                }),
            }
            : updatedInstanceWithNewState;

        let {
          Update.hostTreeUpdate,
          payload: nextInstanceSubForest,
          enqueuedEffects,
        } = {
          updateInstanceForest(
            ~updateContext={
              Update.shouldExecutePendingUpdates:
                updateContext.shouldExecutePendingUpdates,
              hostTreeState: {
                absoluteSubtreeIndex: 0,
                nearestHostNode: (
                  instanceWithNewHostView.wrappedHostNode: lazyHostNode(node)
                ),
                nodeElement: (
                  instanceWithNewHostView.children_:
                    hostNodeElement(node, childNode)
                ),
              },
            },
            ~oldInstanceForest=childInstances,
            ~nextElement=nextSubElements.children,
            (),
          );
        };
        if (nextInstanceSubForest !== instanceWithNewHostView.childInstances) {
          (
            updateContext.hostTreeState.nearestHostNode,
            {
              ...instanceWithNewHostView,
              childInstances: nextInstanceSubForest,
              children_: nextSubElements,
              wrappedHostNode: hostTreeUpdate.nearestHostNode,
            }:
              instance(
                (hooks, (node, children, childNode, wrappedHostNode)),
              ),
            enqueuedEffects,
            updateContext.hostTreeState.nodeElement,
          );
        } else {
          (
            updateContext.hostTreeState.nearestHostNode,
            instanceWithNewHostView,
            enqueuedEffects,
            updateContext.hostTreeState.nodeElement,
          );
        };
      };
    if (updatedInstanceWithNewSubtree === updatedInstanceWithNewElement
        && !stateChanged) {
      {
        hostTreeUpdate: {
          nodeElement,
          nearestHostNode,
          absoluteSubtreeIndex: 0,
        },
        payload: originalOpaqueInstance,
        enqueuedEffects,
        childNodes: Seq.empty,
      };
    } else {
      {
        hostTreeUpdate: {
          nodeElement,
          nearestHostNode,
          absoluteSubtreeIndex: 0,
        },
        payload: Instance(updatedInstanceWithNewSubtree),
        enqueuedEffects:
          EffectSequence.chain(
            Hooks.pendingEffects(
              ~lifecycle=Hooks.Effect.Update,
              Some(updatedInstanceWithNewSubtree.hooks),
            ),
            enqueuedEffects,
          ),
        childNodes: Seq.empty,
      };
    };
  }

and applyUpdate:
  type node childNode.
    (
      ~updateContext: Update.context(node, childNode),
      subtreeUpdate(childNode)
    ) =>
    Update.t(node, childNode, instanceForest(childNode)) =
  (~updateContext, update) => {
    switch (update) {
    | MatchingSubtree(updates) =>
      let update =
        List.fold_left(
          (updateAcc: Update.t(_, _, _), subtreeUpdate) => {
            let update =
              applyUpdate(
                ~updateContext={
                  Update.hostTreeState: updateAcc.Update.hostTreeUpdate,
                  shouldExecutePendingUpdates:
                    updateContext.shouldExecutePendingUpdates,
                },
                subtreeUpdate,
              );
            // TODO: Don't forget about effects...
            update
            |> Update.map(updatedInstanceForest =>
                 [updatedInstanceForest, ...updateAcc.payload]
               );
          },
          {
            Update.payload: [],
            hostTreeUpdate: updateContext.hostTreeState,
            enqueuedEffects: EffectSequence.noop,
            childNodes: Seq.empty,
          },
          List.rev(updates),
        );
      update
      |> Update.map(instances =>
           INested(
             instances,
             update.Update.hostTreeUpdate.absoluteSubtreeIndex
             - updateContext.hostTreeState.absoluteSubtreeIndex,
           )
         );
    | Update(instance, newComponent) =>
      updateOpaqueInstance(~updateContext, instance, newComponent)
      |> Update.map(instance => IFlat(instance))
    | UpdateSequence(instances, elements) =>
      instances.diff(elements)
      |> Seq.fold_left(
           (acc, change) =>
             switch (change) {
             | Updated(oldInstanceForest, nextElement) =>
               applyUpdate(
                 ~updateContext={
                   Update.hostTreeState: acc.Update.hostTreeUpdate,
                   shouldExecutePendingUpdates:
                     updateContext.shouldExecutePendingUpdates,
                 },
                 prepareUpdate(~oldInstanceForest, ~nextElement),
               )
               |> Update.map(instanceForest =>
                    acc.payload.insert(instanceForest)
                  )
             | Added(element) =>
               renderReactElement(~updateContext, element)
               |> Update.map(instanceForest =>
                    acc.payload.insert(instanceForest)
                  )
             | Removed(_instance) => acc
             },
           {
             Update.payload: instances,
             hostTreeUpdate: updateContext.hostTreeState,
             enqueuedEffects: EffectSequence.noop,
             childNodes: Seq.empty,
           },
         )
      |> Update.map(instances => IDiffableSequence(instances, 0))
    | ReRender(oldInstanceForest, element) =>
      let updateTreeState = updateContext.hostTreeState;
      let hostTreeState = {
        Update.nodeElement: updateTreeState.nodeElement,
        nearestHostNode:
          SubtreeChange.deleteNodes(
            ~nodeElement=updateTreeState.nodeElement,
            ~parent=updateTreeState.nearestHostNode,
            ~children=Instance.Forest.outputTreeNodes(oldInstanceForest),
            ~position=updateTreeState.absoluteSubtreeIndex,
          ),
        absoluteSubtreeIndex: updateTreeState.absoluteSubtreeIndex,
      };
      renderReactElement(
        ~updateContext={...updateContext, hostTreeState},
        element,
      );
    | DeleteMovableInstanceForest(instanceForest, nextElement) =>
      // DI: Current index of a deleted element in the udpate loop
      // II: Current index of an inserted element in the udpate loop
      // There are 5 possible cases:
      // MOVE_BEHIND: DI > II
      // MOVE_AHEAD: DI < II
      // DELETED
      // **future** CLONE_DELETE: An occurence has been removed (clones)
      // **future** CLONE_INSERT: An occurence has been inserted (clones)
      if (Hashtbl.mem(instances, instanceForest)) {
        // MOVE_BEHIND
        //   It means that we should already have it in our hashtbl
        let updateTreeState = updateContext.hostTreeState;
        // prepareUpdate for oldInstanceForest/newInstanceForest
        let hostTreeState = {
          Update.nodeElement: updateTreeState.nodeElement,
          nearestHostNode:
            SubtreeChange.reorder(
              ~nodeElement=updateTreeState.nodeElement,
              ~parent=updateTreeState.nearestHostNode,
              ~instances=Instance.Forest.outputTreeNodes(oldInstanceForest),
              ~indexShift=updateTreeState.absoluteSubtreeIndex,
              ~from=0,
              ~to_=1,
            ),
          // Increase absoluteSubtreeIndex by newTreeSize - oldTreeSize
          absoluteSubtreeIndex: updateTreeState.absoluteSubtreeIndex,
        };
        renderReactElement(
          ~updateContext={...updateContext, hostTreeState},
          nextElement,
        );
      } else {
        // MOVE_AHEAD or DELTED
        //   We should add it to the hashtbl
        Hashtbl.add(
          movables,
          instanceForest,
          someKey,
        );
      }
    | InsertMovableInstanceForest(instanceForest, nextElement) =>
      {};
      // There are three (four) possible cases:
      // 4. FUTURE: An occurence has been removed or added (clones)
      if (Hashtbl.has(instances, instanceForest)) {
        // MOVE_AHEAD
        // Increase absoluteSubtreeIndex by newTreeSize - oldTreeSize
        SubtreeChange.reorder();
      } else {
        {
          // MOVE_BEHIND or DELETED
        };
      };
    };
  }

and updateInstanceForest:
  type parentNode node.
    (
      ~updateContext: Update.context(parentNode, node),
      ~oldInstanceForest: instanceForest(node),
      ~nextElement: element(node),
      unit
    ) =>
    Instance.renderedElement(parentNode, node) =
  (~updateContext, ~oldInstanceForest, ~nextElement, ()) => {
    applyUpdate(
      ~updateContext,
      prepareUpdate(~oldInstanceForest, ~nextElement),
    );
  };

/**
  * Execute the pending updates at the top level of an instance tree.
  * If no state change is performed, the argument is returned unchanged.
  */
let flushPendingUpdates = (~hostTreeState, opaqueInstance) => {
  let Instance({opaqueComponent}) = opaqueInstance;
  updateOpaqueInstance(
    ~updateContext=Update.{shouldExecutePendingUpdates: true, hostTreeState},
    opaqueInstance,
    opaqueComponent,
  );
};
