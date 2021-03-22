Plotly.d3.csv("https://raw.githubusercontent.com/cieb-unibas/female_inventors/main/Report/graph_gender_techgroup/female_inventors_graduates_techgroup_USPTO.csv", function(err, rows){
function unpack(rows, key) {
        return rows.map(function(row) { return row[key]; });
    }

var     allTech = unpack(rows, 'tech_group'),
        femGrad = unpack(rows, 'female_share_graduates'),
        femInv = unpack(rows, 'female_share_inventors'),
        invCtry = unpack(rows, 'inv_ctry'),
        allcountryName = unpack(rows, 'country'),
        listofCountries = [],
        currentCountry = [],
        currentFemInv = [],
        currentFemGrad = [],
        currentInv = [],
        currentcountryName = [];


    for (var i = 0; i < allTech.length; i++ ){
        if (listofCountries.indexOf(allTech[i]) === -1 ){
            listofCountries.push(allTech[i]);
        }
    }

    function getCountryData(chosenCountry) {
        currentFemInv = [];
        currentFemGrad = [];
        currentInv = [];
        currentcountryName = [];
        for (var i = 0 ; i < allTech.length ; i++){
            if ( allTech[i] === chosenCountry ) {
                currentFemInv.push(femInv[i]);
                currentFemGrad.push(femGrad[i]);
                currentInv.push(invCtry[i]);
                currentcountryName.push(allcountryName[i]);
                }
        }
    };

    // Default Country Data
    setBubblePlot('Overall');

    function setBubblePlot(chosenCountry) {
        getCountryData(chosenCountry);

        var trace1 = {
            x: currentFemGrad,
            y: currentFemInv,
            type: 'scatter',
            mode: 'markers+text',
            hovertemplate:  '%{y}' + '<extra></extra>',
            marker: { color:  ['#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         'red',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f']},
            text: currentInv,
            textposition: 'bottom center'
        };
        
        var trace2 = {
            x: currentFemGrad,
            y: currentFemInv,
            mode: 'markers',
            hovertemplate: '<i>Country</i>: %{text}' + '<extra></extra>',
            marker: { color:  ['#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         'red',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', 
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f',
                         '#2ca25f', '#2ca25f', '#2ca25f', '#2ca25f']},
                         text: currentcountryName,
            textposition: 'bottom center',
            type: 'scatter'
        };

        var data = [trace1, trace2];

        var layout = {
          showlegend: false,
          scrollZoom: false,
    xaxis: {fixedrange: true,
            zeroline: false,
            tickformat: ',.1%', 
            title: {text: '<b>Female Graduate Share in STEM Fields</b>'}},
    yaxis: {fixedrange: true,
            zeroline: false,
            tickformat: ',.1%',
            title: {text: '<b>Female Inventor Share</b>'}},
    shapes: [
{
      type: 'line',
      x0: math.median([currentFemGrad]),
      x1: math.median([currentFemGrad]),
      y0: math.min([currentFemInv]),
      y1: math.max([currentFemInv]),
      line: {dash: 'dot'}
    },
    {
      type: 'line',
      x0: math.min([currentFemGrad]),
      y0: math.median([currentFemInv]),
      x1: math.max([currentFemGrad]),
      y1: math.median([currentFemInv]),
      line: {dash: 'dot'}
    }
    ]
};

$("select").selectpicker();

Plotly.newPlot('myDiv', data, layout, {displayModeBar: false});
    };

    var countrySelector = document.querySelector('.selectpicker');

    function assignOptions(textArray, selector) {
        for (var i = 0; i < textArray.length-1;  i++) {
            var currentOption = document.createElement('option');
            currentOption.text = textArray[i];
            selector.appendChild(currentOption);
        }
    }

    assignOptions(listofCountries, countrySelector);

    function updateCountry(){
        setBubblePlot(countrySelector.value);
    }

    countrySelector.addEventListener('change', updateCountry, false);
});   