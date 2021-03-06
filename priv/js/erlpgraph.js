var Drawing = Drawing || {};

Drawing.ProcessGraph = function(options) {
  var options = options || {};
  
  this.layout = options.layout || "2d";
  this.layout_options = options.graphLayout || {};
  this.show_stats = options.showStats || false;
  this.show_info = options.showInfo || false;
  this.show_labels = options.showLabels || false;
  this.selection = options.selection || false;
  this.limit = options.limit || 10;
  this.nodes_count = options.numNodes || 20;
  this.edges_count = options.numEdges || 10;

  var camera, scene, renderer, interaction, geometry, object_selection;
  var stats;
  var info_text = {};
  var graph = new Graph({limit: options.limit});
  
  var geometries = [];
  
  var that=this;

  this.init = function() {
    // Three.js initialization
    renderer = new THREE.WebGLRenderer({antialias: true});
    renderer.setSize( window.innerWidth, window.innerHeight );
    
    camera = new THREE.TrackballCamera({
      fov: 40, 
      aspect: window.innerWidth / window.innerHeight,
      near: 1,
      far: 1000000,

      rotateSpeed: 0.5,
      zoomSpeed: 5.2,
      panSpeed: 1,

      noZoom: false,
      noPan: false,

      staticMoving: false,
      dynamicDampingFactor: 0.3,
      
      domElement: renderer.domElement,

      keys: [ 65, 83, 68 ]
    });
    camera.position.z = 5000;

    scene = new THREE.Scene();

    // Node geometry
    if(that.layout === "3d") {
      geometry = new THREE.SphereGeometry( 25, 25, 25 );
    } else {
      geometry = new THREE.SphereGeometry( 50, 50, 0 );
    }
    
    // Create node selection, if set
    if(that.selection) {
      object_selection = new THREE.ObjectSelection({
        domElement: renderer.domElement,
        selected: function(obj) {
          // display info
          if(obj != null) {
            info_text.select = "Object " + obj.id;
          } else {
            delete info_text.select;
          }
        },
        clicked: function(obj) {
        }
      });
    }

    document.body.appendChild( renderer.domElement );
  
    // Stats.js
    if(that.show_stats) {
      stats = new Stats();
      stats.domElement.style.position = 'absolute';
      stats.domElement.style.top = '0px';
      document.body.appendChild( stats.domElement );
    }

    // Create info box
    if(that.show_info) {
      var info = document.createElement("div");
      var id_attr = document.createAttribute("id");
      id_attr.nodeValue = "graph-info";
      info.setAttributeNode(id_attr);
      document.body.appendChild( info );
    }
  };
  
  /**
   *  Creates a graph with random nodes and edges.
   *  Number of nodes and edges can be set with
   *  numNodes and numEdges.
   */
  this.createGraph = function(vertices, edges) {

    console.log('vertices -- ');
    console.log(vertices);
    console.log('edges --');
    console.log(edges);

    // add all vertices
    var i = 0;
    for(var v in vertices) {
        if(vertices.hasOwnProperty(v)) {
            vertices[v]["node"] = new Node(v);
            graph.addNode(vertices[v].node);
            drawNode(vertices[v].node);
        }
    }

    // add all edges
    for(var v in vertices) {
        if(vertices.hasOwnProperty(v)
                && edges.hasOwnProperty(v)
                && vertices.hasOwnProperty(edges[v])) {
          var from_node = vertices[v].node;
          var to_node = vertices[edges[v]].node;
          if(graph.addEdge(from_node, to_node)) {
            drawEdge(from_node, to_node);
          }
        }
    }
    
/*
    var node = new Node(0);
    node.data.title = vertices[0].name;
    console.log('node --');
    console.log(node);
    graph.addNode(node);
    drawNode(node);

    var nodes = [];
    nodes.push(node);

    var steps = 1;
    while(nodes.length != 0 && steps < that.nodes_count) {
      var node = nodes.shift();

      var numEdges = randomFromTo(1, that.edges_count);
      for(var i=1; i <= numEdges; i++) {
        var target_node = new Node(i*steps);
        if(graph.addNode(target_node)) {
          target_node.data.title = vertices[i*steps].name;
          
          drawNode(target_node);
          nodes.push(target_node);
          if(graph.addEdge(node, target_node)) {
            drawEdge(node, target_node);
          }
        }
      }
      steps++;
    } 
//*/
    that.layout_options.width = that.layout_options.width || 2000;
    that.layout_options.height = that.layout_options.height || 2000;
    that.layout_options.iterations = that.layout_options.iterations || 100000;
    that.layout_options.layout = that.layout_options.layout || that.layout;
    graph.layout = new Layout.ForceDirected(graph, that.layout_options);
    graph.layout.init();
    info_text.nodes = "Nodes " + graph.nodes.length;
    info_text.edges = "Edges " + graph.edges.length;
  };


  /**
   *  Create a node object and add it to the scene.
   */
  function drawNode(node) {
    var draw_object = new THREE.Mesh( geometry, [ new THREE.MeshBasicMaterial( {  color: Math.random() * 0xffffff, opacity: 0.5 } ) ] );
    
    if(that.show_labels) {
      if(node.data.title != undefined) {
        var label_object = new THREE.Label(node.data.title);
      } else {
        var label_object = new THREE.Label(node.id);
      }
      node.data.label_object = label_object;
      scene.addObject( node.data.label_object );
    }

    var area = 5000;
    draw_object.position.x = Math.floor(Math.random() * (area + area + 1) - area);
    draw_object.position.y = Math.floor(Math.random() * (area + area + 1) - area);
    
    if(that.layout === "3d") {
      draw_object.position.z = Math.floor(Math.random() * (area + area + 1) - area);
    }

    draw_object.id = node.id;
    node.data.draw_object = draw_object;
    node.position = draw_object.position;
    scene.addObject( node.data.draw_object );
  }


  /**
   *  Create an edge object (line) and add it to the scene.
   */
  function drawEdge(source, target) {
      material = new THREE.LineBasicMaterial( { color: 0xff0000, opacity: 1, linewidth: 0.5 } );
      var tmp_geo = new THREE.Geometry();
    
      tmp_geo.vertices.push(new THREE.Vertex(source.data.draw_object.position));
      tmp_geo.vertices.push(new THREE.Vertex(target.data.draw_object.position));

      line = new THREE.Line( tmp_geo, material, THREE.LinePieces );
      line.scale.x = line.scale.y = line.scale.z = 1;
      line.originalScale = 1;
      
      geometries.push(tmp_geo);
      
      scene.addObject( line );
  }


  this.animate = function() {
    requestAnimationFrame( that.animate );
    render();
    if(that.show_info) {
      printInfo();
    }
  }


  function render() {
    // Generate layout if not finished
    if(!graph.layout.finished) {
      info_text.calc = "<span style='color: red'>Calculating layout...</span>";
      graph.layout.generate();
    } else {
      info_text.calc = "";
    }

    // Update position of lines (edges)
    for(var i=0; i<geometries.length; i++) {
      geometries[i].__dirtyVertices = true;
    }


    // Show labels if set
    // It creates the labels when this options is set during visualization
    if(that.show_labels) {
      var length = graph.nodes.length;
      for(var i=0; i<length; i++) {
        var node = graph.nodes[i];
        if(node.data.label_object != undefined) {
          node.data.label_object.position.x = node.data.draw_object.position.x;
          node.data.label_object.position.y = node.data.draw_object.position.y - 100;
          node.data.label_object.position.z = node.data.draw_object.position.z;
          node.data.label_object.lookAt(camera.position);
        } else {
          if(node.data.title != undefined) {
            var label_object = new THREE.Label(node.data.title, node.data.draw_object);
          } else {
            var label_object = new THREE.Label(node.id, node.data.draw_object);
          }
          node.data.label_object = label_object;
          scene.addObject( node.data.label_object );
        }
      }
    } else {
      var length = graph.nodes.length;
      for(var i=0; i<length; i++) {
        var node = graph.nodes[i];
        if(node.data.label_object != undefined) {
          scene.removeObject( node.data.label_object );
          node.data.label_object = undefined;
        }
      }
    }

    // render selection
    if(that.selection) {
      object_selection.render(scene, camera);
    }

    // update stats
    if(that.show_stats) {
      stats.update();
    }
    
    // render scene
    renderer.render( scene, camera );
  }

  /**
   *  Prints info from the attribute info_text.
   */
  function printInfo(text) {
    var str = '';
    for(var index in info_text) {
      if(str != '' && info_text[index] != '') {
        str += " - ";
      }
      str += info_text[index];
    }
    document.getElementById("graph-info").innerHTML = str;
  }

  // Generate random number
  function randomFromTo(from, to) {
    return Math.floor(Math.random() * (to - from + 1) + from);
  }
  
  // Stop layout calculation
  this.stop_calculating = function() {
    graph.layout.stop_calculating();
  }
}

function start_all()
{
    // fire off the request to /form.php
    request = $.ajax({
        url: "/graph/all",
        type: "post",
        data: {}
    });

    // callback handler that will be called on success
    request.done(function (response, textStatus, jqXHR){
        var PROCESSES = new Object();
        var LINKS = new Object();

        for(i = 0; i < response.vertices.length; ++i)
            PROCESSES[response.vertices[i].name] = response.vertices[i];

        for(i = 0; i < response.edges.length; ++i)
            LINKS[response.edges[i][0].name] = response.edges[i][1].name;

        /*
        new Drawing.SimpleGraph({layout: '3d', selection: true, numNodes: 50, graphLayout:{attraction: 5, repulsion: 0.5}, showStats: true, showInfo: true});
        debugger;
        /*/
        PGraph = new Drawing.ProcessGraph({layout: '3d', selection: true, showStats: true, showInfo: true});
        PGraph.init();
        PGraph.createGraph(PROCESSES, LINKS);
        PGraph.animate();
        //*/
    });

    // callback handler that will be called on failure
    request.fail(function (jqXHR, textStatus, errorThrown){
        // log the error to the console
        console.error(
            "The following error occured: "+
            textStatus, errorThrown
        );
    });

    // callback handler that will be called regardless
    // if the request failed or succeeded
    request.always(function () {
        console.log("send");
    });
}
