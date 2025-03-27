$(document).ready(function () {
  const table = this.api();

  // Create a tooltip div
  const tooltipDiv = $('<div></div>')
    .attr('id', 'tooltip')
    .css({
      position: 'absolute',
      padding: '8px',
      background: 'white',
      border: '1px solid #dddddd',
      visibility: 'hidden'
    })
    .appendTo('body');

  // Show tooltip on row hover
  table.on('mouseover', 'tr', function () {
    const rowData = table.row(this).data();
    if (rowData) {
      $('#tooltip')
        .html(`Description: ${rowData[4]}<br>Source: ${rowData[5]}`);
      tooltipDiv.css('visibility', 'visible').fadeIn(100);
    }
  });

  // Hide tooltip when mouse leaves row
  table.on('mouseout', 'tr', function () {
    tooltipDiv.css('visibility', 'hidden').fadeOut(100);
  });

  // Move tooltip with mouse
  table.on('mousemove', function (e) {
    $('#tooltip')
      .css('top', e.pageY + 10)
      .css('left', e.pageX + 10);
  });

  // Add a "Select All / Unselect All" button above the filter
  const $button = $('<button id="toggle-select-all" class="btn">')
    .text('Select All/Unselect All')
    .css({
      backgroundColor: '#1b98e0',
      color: 'white',
      margin: '10px'
    });

  $(table.table().container())
    .find('.dataTables_filter')
    .prepend($button);

  let allSelected = false;

  // Handle button click
  $('#toggle-select-all').on('click', function () {
    allSelected = !allSelected;

    const filteredRows = table.rows({ filter: 'applied' }).indexes();

    filteredRows.each(function (index) {
      if (allSelected) {
        table.row(index).select();
      } else {
        table.row(index).deselect();
      }
    });

    // Get names of selected rows
    const selectedNames = filteredRows.toArray().map(function (index) {
      const name = table.row(index).data()[0];
      console.log('Row Name:', name);
      return name;
    });

    console.log('Selected Names:', selectedNames);
    Shiny.setInputValue('Variables_rows_selected_names', selectedNames);
  });
});
