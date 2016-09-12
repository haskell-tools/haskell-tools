$(function() {

    astView = false;
    disableEditorSelect = false;
    disableASTSelect = false;

    function selectASTElemForRange() {
        var editorRange = editor.selection.getRange();
        var toSelect = $('#ast .jstree-node .jstree-anchor').filter(function(i,e) { 
            return dataRangeInside(toAceRange($(e).attr("data-range")), editorRange);
        }).last();
        if (toSelect.size() > 0) {
            disableEditorSelect = true;
            if (!disableASTSelect) {
                $('#ast').jstree().deselect_all();
                $('#ast').jstree().select_node(toSelect.attr('id'));
            }
            disableEditorSelect = false;
            $('#ast').stop(true).animate(
                { scrollTop: toSelect.offset().top - $('#ast').offset().top + $('#ast').scrollTop() - $('#ast').height()/2
                , scrollLeft: toSelect.offset().left - $('#ast').offset().left + $('#ast').scrollLeft() + toSelect.width() - $('#ast').width()
                }, 500);
        }
    }

    function toAceRange(textRange) {
        var rng = textRange.split(/:|\-/);
        return { start : { row : rng[0] - 1, column : rng[1] - 1 }, end : { row : rng[2] - 1, column : rng[3] - 1 } };
    }

    fromAceRange = function(rng) {
        return (rng.start.row + 1) + ":" + (rng.start.column + 1) + "-" + (rng.end.row + 1) + ":" + (rng.end.column + 1);
    }

    function dataRangeInside(bigger, smaller) {
        var startInside = bigger.start.row < smaller.start.row || (bigger.start.row == smaller.start.row && bigger.start.column <= smaller.start.column);
        var endInside = smaller.end.row < bigger.end.row || (smaller.end.row == bigger.end.row && smaller.end.column <= bigger.end.column)
        return startInside && endInside;
    }

    // when the editor content is changed exit from AST mode
    editor.getSession().on('change', function(data) {
        switchASTView(false);
    });
    
    // hookup selection change listener (for AST view)
    editor.getSession().selection.on('changeSelection', function(e) {
        if (astView && !disableEditorSelect) {
            selectASTElemForRange();
        }
    });

    // turns the AST view on and off
    switchASTView = function(on) {
        astView = on;
        if (on) {
            $('.editorslide .slidecont').css('width', '100%');
            $('#ast').css('right', '75%');
            $('#rightpane').css('left', '25%');
            $('#infos').css('top', '85%');
            $('#editor').css('bottom', '15%');
            $('#ast').jstree();
            $('#ast').jstree().destroy();

            $('#ast').on("ready.jstree", function (e, data) {
                selectASTElemForRange();
            });

            $('#ast').on("changed.jstree", function (e, data) {
                if (data.action == "deselect_all") {
                    return;
                }
                disableASTSelect = true;
                if (!disableEditorSelect) {
                    var range = toAceRange(data.node.a_attr["data-range"]);
                    editor.selection.setSelectionRange(range);
                    editor.scrollToLine(range.start.row, true, true, function() {});
                }
                disableASTSelect = false;
                var memberdata = data.node.a_attr["data-elems"];
                $('#memberinfos').empty();
                $('#memberinfos').append(memberdata);
                var semadata = data.node.a_attr["data-sema"];
                $('#semainfos').empty();
                $('#semainfos').append(semadata);
            });
        } else {
            $('.editorslide .slidecont').css('width', '');
            $('#ast').css('right', '');
            $('#rightpane').css('left', '');
            $('#infos').css('top', '');
            $('#editor').css('bottom', '');
            $('#ast').jstree();
            $('#ast').jstree().destroy();
        }
    }
});