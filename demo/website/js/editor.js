$(function () { 
    
    // create the editor in a global scope
    editor = ace.edit("editor");

    // configure the editor
    editor.$blockScrolling = Infinity; // stop automatic scrolling on selection change
    editor.setTheme("ace/theme/kr_theme"); // setting up theme
    editor.session.setMode("ace/mode/haskell"); // haskell syntax highlight
    editor.getSession().setUseWrapMode(true); // wrap lines

    // variables for tab handling
    currentTab = "Demo";
    tabContents = {};
    tabs = [];

    manualEditing = false;

    function setEditorContents(cont) {
        manualEditing = true;
        editor.setValue(cont, -1);
        manualEditing = false;
    }

    // load the initial example at the start
    $.get('res/Demo.hs', function(data) {
        setEditorContents(data);
        tabContents["Demo"] = data;
        tabs = ["Demo"];
    });

    // initialize tabs
    $('#tabs').tabs({
        // setup tab change hook
        activate: function(event, ui) {;
            var oldTab = ui.oldTab.text();
            tabContents[oldTab] = editor.getValue();
            synchronizeChanges(oldTab, tabContents[oldTab]);
            var newTab = ui.newTab.text();
            setEditorContents(tabContents[newTab] || "")
            event.stopPropagation();
            currentTab = newTab;
        }
    });

    // close a tab
    $(document).on( "click", ".ui-closable-tab", {}, function() {
        var tabContainerDiv=$(this).closest(".ui-tabs").attr("id");
        var tabCount=$("#"+tabContainerDiv).find(".ui-closable-tab").length;
        if (tabCount>1) {
            var moduleName = $(this).closest("li").text();
            tabs.splice(tabs.indexOf(moduleName), 1);
            var panelId = $( this ).closest( "li" ).remove().attr( "aria-controls" );
            $( "#" + panelId ).remove();
            $("#"+tabContainerDiv).tabs("refresh");
            toServer({ tag : 'ModuleDeleted', moduleName: moduleName });
        }
    });

    // load a feature or refactor demo into the editor 
    $('.featurebutton, #demoanimation').click(function(event) {
        var demoname = $(event.target).attr("data-demoname");
        $.get('res/' + demoname.replace('.','/') + '.hs', function(data) {
            $('body').animate({ scrollTop: $('a[name=demo]').offset().top }, 1000);
            if (!(demoname in tabContents)) {
                tabContents[demoname] = data;
                createTab(demoname, true);
            } else {
                tabContents[demoname] = data;
                selectTab(demoname);
            }
            toServer({ tag : 'ModuleChanged', moduleName: demoname, newContent: data });
        })
    });

    // prepare the module name dialog
    var moduleNameDialog = $( "#new-tab-dialog" );
    moduleNameDialog.dialog({
        autoOpen: false,
        modal: true,
        buttons: {
            "Create": function() {
                var moduleName = $('#modulenamefld').val();
                tabContents[moduleName] = "module " + moduleName + " where\n\n";
                createTab(moduleName, true);
                toServer({ tag : 'ModuleChanged', moduleName: moduleName, newContent: tabContents[moduleName] });
                moduleNameDialog.dialog( "close" );
            },
            "Cancel": function() {
                moduleNameDialog.dialog( "close" );
            }
        }
    }); 

    // create a new tab on click
    $('#editortabs').click(function(event) {
        if (event.target == $('#editortabs')[0]) {
            moduleNameDialog.dialog('open');
        }
    })  

    // create a new tab for the given module
    createTab = function(name, focus) {
        $('#editortabs').append( $("<li><a href='#tab-" + name + "'>" + name + "</a><span class='ui-icon ui-icon-circle-close ui-closable-tab'></span></li>") );
        $('#tabpanels').append( $("<div id='tab-" + name + "'></div>") );
        $('#tabs').tabs('refresh');
        tabs.push(name);
        if (focus) selectTab(name);
    }

    // bring the given module forward
    selectTab = function(name) {
        $('#tabs').tabs( "option", "active", tabs.indexOf(name) );
    }

    // True, if the server is up-to-date with the current editor content
    var changesSynched = true;
    var changeSynch;

    // when the editor content changes synchronize it on the server
    editor.getSession().on('change', function(data) {
        clearTimeout(changeSynch);
        if (!manualEditing) {
            changesSynched = false;
            changeSynch = setTimeout(function() { synchronizeChanges(currentTab, editor.getValue()); }, 2000);
        }
    });

    synchronizeChanges = function(module = currentTab, content = tabContents[module]) {
        if (!changesSynched) {
            $('#compile-errors').removeClass('errors-visible');
            toServer({ tag : 'ModuleChanged', moduleName: module, newContent: content });
        }
        changesSynched = true;
    }

});