$(function() {
    var serverAddress = "ws://" + window.location.hostname  + ":8206/";

    var ws;

    function openSocketConnection() {
        ws = new WebSocket(serverAddress);

        ws.onopen = function() {
            // send the Connect signal to connect to the service
            var modules = [];
            for (var i = 0; i < tabs.length; i++) {
                modules.push([tabs[i], tabContents[tabs[i]]]);
            }
            toServer({ tag: 'InitialProject', initialModules: modules });
            // setup a regular signal to keep the connection alive
            setInterval(function() { toServer({ tag: 'KeepAlive', contents: [] }); }, 1000);
        }

        ws.onmessage = function(evt) { 
            var data = evt.data;
            responseHandler($.parseJSON(data));
        };

        ws.onclose = ws.onerror = function(evt){
            showError("The connection with the refactoring service cannot be established. Please try again later.")
            setTimeout(function() { openSocketConnection(); }, 2000);
        }
    }

    openSocketConnection();

    toServer = function(data) {
        ws.send(JSON.stringify(data));
    }

    // perform refactorings on button clicks
    $('.refactorbutton').click(function(event) {
        var refactor = $(event.target).attr("data-refactor");
        if (refactor == "RenameDefinition") {
            getDetails = renameDetails;
        } else if (refactor == "ExtractBinding") {
            getDetails = extractBindingDetails;
        } else {
            getDetails = function(cont) { cont([]); };
        }
        performRefactor(refactor, getDetails)(null);
    });

    function responseHandler(resp) {
        $('body').removeClass("loading");
        if(resp.tag == 'ErrorMessage') {
            showError(resp.errorMsg);
        } else if (resp.tag == 'CompilationProblem') {
            $('#compile-errors').text(resp.errorMsg);
            $('#compile-errors').addClass('errors-visible');
        } else if (resp.tag == 'RefactorChanges') {
            var currentModuleRemoved = false;
            var currentModuleChanged = false;
            for (var i = 0; i < resp.moduleChanges.length; i++) {
                var moduleName = resp.moduleChanges[i][0];
                var moduleContent = resp.moduleChanges[i][1];
                tabContents[moduleName] = moduleContent;
                var contInd = tabs.indexOf(moduleName);
                if (moduleContent == null && contInd != -1) {
                    tabs.splice(contInd, 1);
                    if (moduleName == currentTab) {
                        currentModuleRemoved = true;
                    }
                } else if (contInd == -1) {
                    tabs.push(moduleName);
                    createTab(moduleName, false);
                }
                if (moduleName == currentTab) {
                    currentModuleChanged = true;
                }
            }
            if (currentModuleRemoved) {
                selectTab(tabs[tabs.length - 1]);
            } else if (currentModuleChanged) {
                // keep the selection intact (of course it can be obsolate)
                var oldSelection = editor.selection.getRange();
                editor.setValue(tabContents[currentTab]);
                disableASTSelect = true;
                editor.selection.setSelectionRange({ start : oldSelection.start, end : oldSelection.start });
                disableASTSelect = false;
            }
        } else if (resp.tag == 'ASTViewContent') {
            switchASTView(true);
            $('#ast').jstree({ 'core' : { "themes" : { "stripes" : true, "icons" : false }
                                        , 'data' : $.parseJSON(resp.astContent) }});
        }
    }

    function createMessage(refactor, details) {
        var range = editor.selection.getRange();
        var formattedRange = fromAceRange(range);
        return { tag: "PerformRefactoring"
               , refactoring: refactor
               , moduleName: currentTab
               , editorSelection: formattedRange
               , details: details
               };
    }

    function createGeneralCommandNoDetails(refactor, name, key) {
        return createGeneralCommand(refactor, name, key, function(cont) { cont([]); })
    }

    function createCommandNoDetails(refactor, name, bindings) {
        return createCommand(refactor, name, bindings, function(cont) { cont([]); })
    }

    function createGeneralCommand(refactor, name, key, getDetails) {
        createCommand(refactor, name + ' 1', {win: 'Ctrl-Alt-' + key,  mac: 'Command-Alt-' + key}, getDetails);
        createCommand(refactor, name + ' 2', {win: 'Ctrl-Shift-' + key,  mac: 'Command-Shift-' + key}, getDetails);
        createCommand(refactor, name + ' 3', {win: 'Shift-Alt-' + key,  mac: 'Shift-Alt-' + key}, getDetails);
        createCommand(refactor, name + ' 4', {win: 'Alt-' + key,  mac: 'Alt-' + key}, getDetails);
        createCommand(refactor, name + ' 5', {win: 'Ctrl-' + key,  mac: 'Command-' + key}, getDetails);
    }

    function createCommand(refactor, name, bindings, getDetails) {
        editor.commands.addCommand({
            name: name,
            bindKey: bindings,
            exec: performRefactor(refactor, getDetails),
            readOnly: false // false if this command should not apply in readOnly mode
        });
    }

    function performRefactor(refactor, getDetails) {
        return function(editor) {
            synchronizeChanges();
            getDetails( function(details) {
                var mess = createMessage(refactor, details);
                $('body').addClass("loading");
                toServer(mess);
            });
        };
    }

    createGeneralCommandNoDetails("GenerateSignature", 'Generate Type Signature', 'S');
    createGeneralCommandNoDetails("OrganizeImports", 'Organize Imports', 'O');
    createGeneralCommandNoDetails("GenerateExports", 'Generate Exports', 'E');
    createGeneralCommand("RenameDefinition", 'Rename Definition', 'R', renameDetails);
    createGeneralCommand("ExtractBinding", 'Extract Binding', 'B', extractBindingDetails);
    createGeneralCommandNoDetails("InlineBinding", 'Inline Binding', 'I');
    createCommandNoDetails("TestErrorLogging", 'Test Error Logging', {win: 'Shift-Ctrl-Alt-L'});
    createGeneralCommandNoDetails("UpdateAST", 'Update AST View', 'U');


    $('.dialog form').on('submit', function(event) {
        event.preventDefault();
        $(this).closest('.ui-dialog').find('button.ui-button-text-only').click();
    });


    function renameDetails(cont) {
        var dialog = $( "#rename-refactor-dialog" );
        dialog.dialog({
            modal: true,
            buttons: {
                "Rename definition": function() {
                    var newname = $('#rename-refactor-dialog .newname').val();
                    if (newname.indexOf(" ") > -1) {
                        $('#rename-refactor-dialog .newname').addClass("ui-state-error");
                    } else {
                        cont([newname]);
                        dialog.dialog( "close" );
                    }
                }
            }
        });
    }

    function extractBindingDetails(cont) {
        var dialog = $( "#extract-refactor-dialog" );
        dialog.dialog({
            modal: true,
            buttons: {
                "Extract binding": function() {
                    var newname = $('#extract-refactor-dialog .newname').val();
                    if (newname.indexOf(" ") > -1) {
                        $('#extract-refactor-dialog .newname').addClass("ui-state-error");
                    } else {
                        cont([newname]);
                        dialog.dialog( "close" );
                    }
                }
            }
        });
    }

    function showError(msg) {
        $('#error-message').text(msg);
        $('#error-dialog').dialog({ dialogClass: "opened-dialog", draggable: false });
    }

});