injIO
timer putStrLn getCurrentTime = liftIO $ do
  prevTime <- newIORef Nothing
  return $ liftIO $ do
    pTime <- readIORef prevTime
    time <- getCurrentTime
    writeIORef prevTime $ Just time
    case pTime of
      Nothing -> putStrLn $ show time
      Just a  -> putStrLn $ show time ++ ", diff: " ++ (show $ diffUTCTime time a)

main'1 = $(assemble timer)
main'2 = do t<-timer; t
main'3 = timer >>= id

inj
doAction timer = do
	timer
  act1
  timer

main'4 = $(assemble doAction)
main'5 = timer >>= \timer -> doAction timer

injIO
flyingLogicLocation = do
  foundExecutables <- possibleFlyingLogicPaths
    $> map (\x->do b <- doesFileExist x; return (b, x))
    $> sequence
    $> fmap (filter fst .> map snd)

inj
invokeFlyingLogicGuessedWithDoc flyingLogicLocation = \doc -> do
  invokeFlyingLogicWithDoc flyingLogicLocation doc


main'6 = $(assemble invokeFlyingLogicGuessedWithDoc)
main'7 = timer >>= \timer -> doAction timer

main'8 = aIO >>= \a -> bIO a >>= \b -> c b a 