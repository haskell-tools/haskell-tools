


pullDefinition' :: SrcSpan -> Ann Module STWithId -> RefactoredModule n
pullDefinition' sp = pullDefinition (nodesContaining sp) (nodesContaining sp) (valBindsInList & filtered (isInside sp))

pullDefinition :: Simple Traversal (Ann Module (STWithName n)) (AnnList d (STWithName n))
                    -> Simple Traversal (AnnList Module (STWithName n)) (AnnList LocalBind (STWithName n))
                    -> Simple Traversal (AnnList LocalBind (STWithName n)) (AnnList ValueBind (STWithName n))
                    -> Ann Module STWithId -> RefactoredModule n
pullDefinition topLevel localBinds selected mod
  = let def = mod ^? firstRef localBinds & selected
        removeOld = firstRef localBinds & annListElements .- delete def
        addNew = firstRef
     in 


