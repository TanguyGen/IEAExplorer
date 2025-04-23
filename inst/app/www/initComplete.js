window.initTooltipJS = function(settings, json) {
  var table = this.api();

  var tooltipDiv = $('<div></div>').attr('id', 'tooltip').css({
    position: 'absolute',
    padding: '8px',
    background: 'white',
    border: '1px solid #dddddd',
    visibility: 'hidden'
  }).appendTo('body');

  table.on('mouseover', 'tr', function() {
    var rowData = table.row(this).data();
    if (rowData) {
      $('#tooltip').html('Description: ' + rowData[4] + '<br>Source: ' + rowData[5]);
      tooltipDiv.css('visibility', 'visible').fadeIn(100);
    }
  });

  table.on('mouseout', 'tr', function() {
    tooltipDiv.css('visibility', 'hidden').fadeOut(100);
  });

  table.on('mousemove', function(e) {
    $('#tooltip').css({ top: e.pageY + 10, left: e.pageX + 10 });
  });

  $('<button id="toggle-select-all" class="btn" style="background-color:#1b98e0; color: white; margin: 10px;">Select All/Unselect All</button>')
    .prependTo($(table.table().container()).find('.dataTables_filter'));

  var allSelected = false;
  $('#toggle-select-all').click(function() {
    allSelected = !allSelected;
    var filteredRows = table.rows({ filter: 'applied' }).indexes();
    if (allSelected) {
      filteredRows.each(function(value) {
        table.row(value).select();
      });
    } else {
      filteredRows.each(function(value) {
        table.row(value).deselect();
      });
    }

    var selectedNames = filteredRows.toArray().map(function(index){
      return table.row(index).data()[0];
    });
    Shiny.setInputValue('Variables_rows_selected_names', selectedNames);
  });
};