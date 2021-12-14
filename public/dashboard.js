/* globals Chart:false, feather:false */

  function previewFile() {
    const content = document.getElementById('PvWindCsvData');
    const [file] = document.querySelector('input[type=file]').files;
    const reader = new FileReader();
  
    reader.addEventListener("load", () => {
      content.value = reader.result;
    }, false);
  
    if (file) {
      reader.readAsText(file);
    }
  }

  function download_table_as_csv(table_id, separator = ';') {
    // Select rows from table_id
    var rows = document.querySelectorAll('table#' + table_id + ' tr');
    // Construct csv
    var csv = [];
    for (var i = 0; i < rows.length; i++) {
        var row = [], cols = rows[i].querySelectorAll('td, th');
        for (var j = 0; j < cols.length; j++) {
            // Clean innertext to remove multiple spaces and jumpline (break csv)
            var data = cols[j].innerText.replace(/(\r\n|\n|\r)/gm, '').replace(/(\s\s)/gm, ' ')
            
            // check abbr in th
            if (cols[j].tagName === "TH"  && cols[j].childNodes[0].tagName === "ABBR") {
              data = cols[j].childNodes[0].getAttribute("title");
            }
            
            // Push escaped string
            row.push(data);

            // Check colspan in header
            if (cols[j].tagName === "TH" && cols[j].hasAttribute("colspan")) {
              var cs = parseInt(cols[j].getAttribute("colspan"));
              for(var r = 1; r < cs; r++) {
                row.push('#');
              }
            }
            
        }
        csv.push(row.join(separator));
    }
    var csv_string = csv.join('\n');
    // Download it
    var filename = 'export_' + table_id + '.csv';
    var link = document.createElement('a');
    link.style.display = 'none';
    link.setAttribute('target', '_blank');
    link.setAttribute('href', 'data:text/csv;charset=utf-8,' + encodeURIComponent(csv_string));
    link.setAttribute('download', filename);
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
}
  