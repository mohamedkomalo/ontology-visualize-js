function VisGraph() {
	var self = this;

	var nodesMap = {};
	var edgesMap = {};

	var visgraph = {
		nodes : [],
		edges : []
	};

	self.nodesMap = nodesMap;
	self.getVisGraph = function() {
		return visgraph;
	};

	self.getNodeByName = function(nodeName, createIfNotExists, shape, shownText) {
		var node = nodesMap[nodeName];
		if (createIfNotExists && node == undefined) {
			node = self.addNode(nodeName, shape, shownText);
		}
		return node;
	};

	self.getEdgeByName = function(edgeName, createIfNotExists) {
		var edge = edgesMap[edgeName];
		if (createIfNotExists && edge == undefined) {
			edge = self.addEdge(null, null, edgeName);
		}
		return edge;
	};

	self.addNode = function(label, shape, shownText) {
		if(shownText == undefined)
			shownText = label;
		
		var newNode = {
			'id' : visgraph.nodes.length,
			'label' : shownText,
			'shape' : shape,
			'group' : ''
		};
		nodesMap[label] = newNode;
		visgraph.nodes.push(newNode);
		return newNode;
	};

	self.addEdge = function(srcIndex, destIndex, edge) {
		var newEdge = {
			'from' : srcIndex,
			'to' : destIndex,
			'label' : edge,
			'style' : 'arrow'
		};
		edgesMap[edge] = newEdge;
		visgraph.edges.push(newEdge);
		return newEdge;
	};
	return self;
};

ontologyVisualizer = (function() {
	var self = {};

	self.showIndividuals = false;

	self.showSchema = false;

	var uriPrefixesArr = [];

	var g;

	var getPrefixedURI = function(rdfValue) {
		// console.log(N3Util);

		if (N3.Util.isUri(rdfValue)) {
			for (var i = 0; i < uriPrefixesArr.length; i++) {
				var prefixInfo = uriPrefixesArr[i];
				if (rdfValue.indexOf(prefixInfo.uri) > -1) {
					// console.log(prefixInfo.prefix);
					return rdfValue.replace(prefixInfo.uri, prefixInfo.prefix + ":");
				}
			}
		}

		return rdfValue;
	};

	var isFromDefaultURIs = (function() {
		var SchemaUris = [ "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "http://www.w3.org/2002/07/owl#", "http://www.w3.org/2000/01/rdf-schema#", "http://www.w3.org/2001/XMLSchema#" ];
		return function(uri) {
			for (var i = 0; i < SchemaUris.length; i++) {
				if (uri.indexOf(SchemaUris[i]) > -1)
					return true;
			}
			return false;
		};
	})();

	var processTripple = function(triple) {
		if(!self.showIndividuals && !self.showSchema)
			return;

		var isSchemaTripple = isFromDefaultURIs(triple.predicate);
		var predicate = getPrefixedURI(triple.predicate);
		// console.log(predicate);

		if ((!isSchemaTripple && self.showSchema)  || (isSchemaTripple && !self.showSchema))
			return;

		var subject = getPrefixedURI(triple.subject);
		var object = triple.object;

		if (predicate === 'rdfs:domain') {
			var subNode;
			if (N3.Util.isUri(object)) {
				subNode = g.getNodeByName(getPrefixedURI(object), true, 'ellipse');
			}
			else{
				objNode = g.getNodeByName(object, true, 'box', "");
			}
			var edge = g.getEdgeByName(subject, true);
			edge.from = subNode.id;

		} else if (predicate === 'rdfs:range') {
			var objNode;
			var edge = g.getEdgeByName(subject, true);
			
			if (N3.Util.isUri(object)) {
				if(isFromDefaultURIs(object)){
					objNode = g.addNode(getPrefixedURI(object), 'box');
				}
				else{
					objNode = g.getNodeByName(getPrefixedURI(object), 'box');
				}

			} else if (N3.Util.isLiteral(object)) {
				objNode = g.getNodeByName(getPrefixedURI(object), true, 'ellipse');

			} else {
				objNode = g.getNodeByName(object, true, 'box');
			}

			edge.to = objNode.id;

		} else if (predicate === 'rdf:type' || predicate === 'owl:inverseOf') {
			return;

		} else{
			//console.log(subject, predicate, object);
			var subIdx;
			if(N3.Util.isBlank(subject)){
				subIdx = g.getNodeByName(subject, true, 'ellipse', "");	
			}
			else
				subIdx = g.getNodeByName(subject, true, 'ellipse');
			
			var objIdx;

			if (N3.Util.isUri(object)) {
				object = getPrefixedURI(object);

				if (object === 'rdf:nil')
					return;

				objIdx = g.getNodeByName(object, true, 'ellipse');

			} else if (N3.Util.isLiteral(object)) {
				objIdx = g.addNode(N3.Util.getLiteralValue(object), 'box');

			} else {
				objIdx = g.getNodeByName(object, true, 'box', "");
			}

			g.addEdge(subIdx.id, objIdx.id, predicate);
		}

	};

	self.visualize = function(containerElem, turtleString) {
		var parser = N3.Parser();
		g = new VisGraph();
		parser.parse(turtleString, function(error, triple, prefixes) {
			if (triple)
				processTripple(triple);

			else {
				// console.log(triples);
				// var visGraph =
				// triplesSchemaToGraph(triples).getVisGraph();

				var visGraph = g.getVisGraph();

				var options = {
					stabilize : false
				};
				var network = new vis.Network(containerElem, visGraph, options);
			}

		}, function(prefix, uri) {
			if(prefix === '')
				return;
			uriPrefixesArr.push({
				'prefix' : prefix,
				'uri' : uri
			});
		});
	};
	return self;
})();
