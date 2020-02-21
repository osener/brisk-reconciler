type t('node) = CoreTypes.element('node);

let rec toRenderedElement = (~mapper, ~init, renderedElement) => {
  let foldSequence = (seq, container, add, wrapResult) => {
    let update =
      Seq.fold_left(
        (update, element) => {
          toRenderedElement(
            ~mapper,
            ~init=update.Update.hostTreeUpdate,
            element,
          )
          |> Update.map(payload => add(update.Update.payload, payload))
          |> Update.mapEffects(nextEffects =>
               EffectSequence.chain(
                 update.Update.enqueuedEffects,
                 nextEffects,
               )
             )
        },
        {
          Update.payload: container,
          enqueuedEffects: EffectSequence.noop,
          hostTreeUpdate: init,
          childNodes: Seq.empty,
        },
        seq,
      );
    update
    |> Update.map(payload =>
         wrapResult(
           payload,
           update.hostTreeUpdate.absoluteSubtreeIndex
           - init.absoluteSubtreeIndex,
         )
       );
  };
  CoreTypes.(
    switch (renderedElement) {
    | Leaf(c) =>
      mapper(~hostTreeState=init, ~component=c)
      |> Update.map(instance => IFlat(instance))
    | DiffableSequence(seq) =>
      foldSequence(
        seq.toSeq(),
        seq.empty(),
        (instances, instance) => instances.insert(instance),
        (seq, length) => IDiffableSequence(seq, length),
      )
    | StaticList(l) =>
      foldSequence(
        l |> List.to_seq,
        [],
        (rest, h) => [h, ...rest],
        (list, length) => INested(List.rev(list), length),
      )

    | Movable(element, ref) =>
      let instanceForest = toRenderedElement(~mapper, ~init, element);
      ref := Some(instanceForest.payload);
      instanceForest;
    }
  );
};
