function addKmlLayer(kml_file, google_map){
    kmlLayer = new google.maps.KmlLayer({
        url: 'https://raw.githubusercontent.com/bertcarnell/USS-Saufely-DD-465/main/src/kml/' + kml_file,
        suppressInfoWindows: true,
        map: google_map
    });
    
    kmlLayer.addListener('click', function(kmlEvent) {
        var text = kmlEvent.featureData.description;
        showInContentWindow(text);
    });

    function showInContentWindow(text) {
        var sidediv = document.getElementById('test_map_text');
        sidediv.innerHTML = text;
    };
};

function createStandardMap(){
    var smap = new google.maps.Map(document.getElementById('test_map_div'), {
        zoom: 4,
        zoomControl: true,
        mapTypeControl: true,
        mapTypeControlOptions: {
            style: google.maps.MapTypeControlStyle.DROPDOWN_MENU
        },
        scaleControl: true,
        streetViewControl: false,
        rotateControl: false,
        fullscreenControl: true
    });
    return smap;
};

// Initialize and add the map
function initMap() {
    map = createStandardMap();
    addKmlLayer('test1942.kml', map);
};

var map;
var kmlLayer;

window.initMap = initMap;

const select_div = document.getElementById('botton_group');

select_div.addEventListener('click', ({ target }) => {
  if (target.getAttribute('name') === 'timeRadio') {
    var div_id = target.getAttribute('id');
    if (div_id === 'radio1942'){
        kmlLayer.setMap(null);
        addKmlLayer('test1942.kml', map);
    } else if (div_id === 'radio1943'){
        kmlLayer.setMap(null);
        addKmlLayer('test1943.kml', map);
    } else if (div_id === 'radio1944'){
        kmlLayer.setMap(null);
        addKmlLayer('test1942icon.kml', map);
    } else {
        kmlLayer.setMap(null);
        addKmlLayer('test1945.kml', map);
    }
  }
});
