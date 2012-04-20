function runAllTests()
    theTests=[];
    
    testsanity.agent=test_1_agent();
    testsanity.environment=test_1_environment();
    testsanity.experiment=@test_sanity_experiment;

    test1.agent=test_1_agent();
    test1.environment=test_1_environment();
    test1.experiment=@test_1_experiment;

    testempty.agent=test_empty_agent();
    testempty.environment=test_empty_environment();
    testempty.experiment=@test_empty_experiment;

    testmessage.agent=test_message_agent();
    testmessage.environment=test_message_environment();
    testmessage.experiment=@test_message_experiment;

    testepisode.agent=test_1_agent();
    testepisode.environment=test_1_environment();
    testepisode.experiment=@test_rl_episode_experiment;

    theTests=[testsanity, test1, testempty, testmessage,testepisode];
    runTheTests(theTests);
end

function runTheTests(theTests)
errorCount=0;
for thisTest=theTests
    errorCount=errorCount+runOneTest(thisTest);
end
fprintf(1,'Total errors: %d\n',errorCount);

end

function errors=runOneTest(thisTest)
!/usr/local/bin/rl_glue &
    errors=runRLGlueMultiExperiment(thisTest);
!/usr/bin/killall rl_glue
end