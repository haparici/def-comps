// defined by [object, size]
var referents = [["box", 10],["box", 12],["box", 14],["box", 30]];

var descriptions = [["bigger", "box"],["biggest", "box"], ["silence"]];

// no cost
var descriptionCosts = {
  "big":1,
  "bigger":1,
  "biggest":1,
  "silence":.1
};

var surfaceForm = {
  "big": "big",
  "bigger": "bigger",
  "biggest": "biggest",
  "silence": "silence"
}

var referentsPrior = function(context) {
  var extended = context.slice();
  extended.push("none"); // null referent -- to absorb probability mass in case nothing works
  
  var probabilities = map(function(n) {return n == "none" ? 20 : 100; }, extended);
  return categorical(probabilities, extended);
}

var contextPrior = function() {
  var refs = filter(function(ref) {flip(.5)}, referents);
  condition(refs.length > 1);
  return refs;
}

// now cost-based!
var descriptionPrior = function() {
  var costs = map(function(n) {return Math.exp(-1 * descriptionCosts[n[0]])}, descriptions);
  return categorical(costs, descriptions);
}

var isInContext = function(context, referent) {
  var f = find(function(n) { if (n == referent) {return n;}}, context);
  return f != null;
}

var getIndex = function(context, referent) {
  var f = mapIndexed(function(n, i) { return [i, n]}, context);
  var z = filter(function(n) {return n[0] == referent;}, f);
  return z[0][1];
}



// GRANULARITY CODE HERE


// ignore precisionP--this was used for the geometric distribution of probability that was scrapped (variable priors)
// currently this is just a flat prior.

var imprecisionCost = .5;

var granularitySelection = function(c) {
	// we explore possible granularities, starting with the max area differential (last referent size - first referent size) and 
	// we flip a coin of weight p each time to determine if we want to divide the granularity by 2 (more precision).
	// less probability mass given to the more precise granularities (which kind of makes sense...)
	// this thus also takes into account the "crisp judgments" effect of "big"
	
	// I have also tried subtracting 1 from deg instead of dividing by 2 (which is an arbitrary factor) to explore ALL granularities.
	// but the problem is that the probabilities for precise granularities become too low.
	
	// precisionP is the probability that the listener will elect to tighten the granularity by another factor.
	
	// also the precision is limited at 1 on a lower bound (no decimal granularities for our purposes).
  
  // flat prior for granularities! method
  var maximumDifferential = c[c.length - 1][1] - c[0][1];
  
  var degs = repeat(maximumDifferential, function() {return 0;});
  var degs2 = mapIndexed(function(x,y) {return x + 1;}, degs);
    
  var degs3 = filter(function(deg) {
    var offsets = repeat(Math.ceil(deg), function() {return 0;});
    var offsets2 = mapIndexed(function(x,y) {return x;}, offsets);
    var res = map(function(offset) {
      return groupBy(function(x,y) {return _top.parseInt((x[1] - c[0][1] + offset) / deg) == _top.parseInt((y[1] - c[0][1] + offset) / deg)}, c).length == c.length ? 1 : 0;
    }, offsets2);
    if (sum(res) != res.length) {
      return deg;
    }
  }, degs2)
  console.log("DEGS: " + degs3);
  
  if (degs3.length > 0) {
  
//     var masses = map(function(deg) {
//       return Math.pow(Math.E, deg - degs3[0]);
//     }, degs3)
//     console.log(masses);
    
//     var deg = categorical(masses, degs3);
    
     var deg = uniformDraw(degs3);

    //console.log(deg);

    // randomly select an offset within the range of sizes spanned by the granularity (eg, if granularity is 10, the offset can be from 0-9).
    var offsets = repeat(Math.ceil(deg), function() {return 0;});
    var offsets2 = mapIndexed(function(x,y) {return x;}, offsets);
    var offset = uniformDraw(offsets2);

    return flip(imprecisionCost) ? [deg, offset] : [-1, -1];
  } else {
    // ie, no distinct imprecision grouping available.
    return [-1, -1];
  }
}

var granularityProcessor = function(c, deg, offset) {
  if (deg[0] == -1)
    return c;
  
  // we group the referents in the context based on the selected granularity / offset.
  var groups = groupBy(function(x,y) {return _top.parseInt((x[1] - c[0][1] + offset) / deg) == _top.parseInt((y[1] - c[0][1] + offset) / deg)}, c);
  
  return groups;
}





var meaning = function(description, granularity, context, referent) {
  if (referent == "none") {
    return true;
  }
  
  // make sure that
  if (!isInContext(context, referent))
    return false;
  //console.log(granularity.length);
  /*var positive = function() {
    var NPTrueFn = function() {
      // NOUNTRUE: checks if the referent in question is of the same type as the description (box)
      var nounTrue = referent[0] == description[1];
      
      // MODIFIERTRUE: checks if the conditions for "big" are satisfied.
      // In my case, it's just that some granularity exists and
      // that the referent in question is in the highest imprecision group.
      var modifierTrue = referent[1] > threshold;
      return nounTrue && modifierTrue;
    }
    
    var DPTrueFn = function() {
      // UNIQUENESSTRUE: checks if the highest imprecision group only has one referent in it.
      var uniquenessTrue = context[context.length - 1] == referent && context[context.length - 2][1] <= threshold;
      return uniquenessTrue;
    }
    
    return NPTrueFn() && DPTrueFn();
  }*/
  
  var comparative = function() {
    var NPTrueFn = function() {
      // NOUNTRUE: checks if the referent in question is of the same type as the description (box)
      var nounTrue = referent[0] == description[1];
      
      // COMPARATIVETRUE: checks if the conditions for "bigger" are satisfied.
      // In my case, it's just that an imprecision grouping of 2 is used, or granularity = 1 (one bar) and
      // that the referent in question is in the highest imprecision group.
      var comparativeTrue = granularity.length == 2 && isInContext(granularity[granularity.length - 1], referent);
      return nounTrue && comparativeTrue;
    }
    
    var DPTrueFn = function() {
      // UNIQUENESSTRUE: checks if the highest imprecision group only has one referent in it.
      var uniquenessTrue = granularity[granularity.length - 1].length == 1;
      return uniquenessTrue;
    }
        
    return NPTrueFn() && DPTrueFn();
  }
  
  var comparativeStrict = function() {
    var NPTrueFn = function() {
      // NOUNTRUE: checks if the referent in question is of the same type as the description (box)
      var nounTrue = referent[0] == description[1];
      
      // COMPARATIVETRUE: checks if the conditions for "bigger" are satisfied.
      // In my case, it's just that an imprecision grouping of 2 is used, or granularity = 1 (one bar) and
      // that the referent in question is in the highest imprecision group.
      var comparativeTrue = context.length == 2 && referent == context[context.length - 1];
      return nounTrue && comparativeTrue;
    }
    
    var DPTrueFn = function() {
      // UNIQUENESSTRUE: checks if the highest imprecision group only has one referent in it.
      var uniquenessTrue = true;
      return uniquenessTrue;
    }
        
    return NPTrueFn() && DPTrueFn();
  }
  
  var superlative = function() {
    var NPTrueFn = function() {
      // NOUNTRUE: checks if the referent in question is of the same type as the description (box)
      var nounTrue = referent[0] == description[1];
      
      // SUPERLATIVETRUE: checks if the conditions for "biggest" are satisfied.
      // In my case, it's just that there are no larger or equivalent items in the scene than the referent in question.
      // because my items are ordered by size, we just need to make sure the referent is the last one in the context
      // and that the second-to-last item isn't the same size.
      
      // This also takes care of the presupposition failure of the definite automatically,
      // since the superlative automatically assumes uniqueness presupp in a way.
      
      var superlativeTrue = referent == context[context.length - 1] && context[context.length - 1][1] != context[context.length - 2][1];
      return nounTrue && superlativeTrue;
    }
    
    // The definite determiner has no effect on the meaning because the superlative already
    // handles the uniqueness presupposition.
    var DPTrueFn = function() {return true;};
    
    return NPTrueFn() && DPTrueFn();
  }
  
  if (description[0] == "big") {
    //return positive();
  } else if (description[0] == "bigger") {
    return comparative();
  } else if (description[0] == "bigger-strict") {
    return comparativeStrict();
  } else if (description[0] == "biggest") {
    return superlative();
  } else if (description[0] == "biggest-strict") {
    return superlative();
  } else if (description[0] == "silence") {
    // empty utterance - anything goes!
    // divvies up the probability mass among the referents evenly.
    // problematic - this means that "silence" as an option gets worse as cardinality increases.
    return true;
  } else {
    //ERROR
    return true;
  }
}




var literalListener = function(description, granularity, context) {
  return Infer({method: "enumerate"}, function() {
    // Infer over referents
    var referent = referentsPrior(context);
    
    condition(meaning(description, granularity, context, referent));
    return referent;
  })
}

var rationality = 1;

//returns a distribution on possible descriptions (in this case, only adjectives)
var speaker = function(context, granularity, referent) {
  return Infer({method: "enumerate"}, function() {
    // Infer over descriptions--should cost-based description be used here then as well as in the pragmatic speaker?
    var description = descriptionPrior();
    
    factor(rationality * literalListener(description, granularity, context).score(referent));
    
    return description;
  })
}

//returns a distribution on granularity / referent combinations
var pragmaticListener = function(description, context) {
	return Infer({method: "enumerate"}, function() {
      // Infer over both granularities AND referents.
      // if I only return distribution on granularities it makes no sense for pS.
      
      var g = granularitySelection(context);
      var g2 = granularityProcessor(context, g[0], g[1]);
      
      //console.log(JSON.stringify(g2));
      
      // grab a threshold too
      //var th = thresholdSelection(context);
      
      
      var referent = referentsPrior(context);
      
      factor(speaker(context, g2, referent).score(description))
            
      return {granularity: [g2], referent: referent};
	})
}

console.log("");
viz.table(marginalize(pragmaticListener(descriptions[0], [["box", 10],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[0], [["box", 10],["box", 12],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[0], [["box", 10],["box", 12],["box", 14],["box", 30]]), function(x) {return {"referent": x.referent}}));

viz.table(marginalize(pragmaticListener(descriptions[1], [["box", 10],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[1], [["box", 10],["box", 12],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[1], [["box", 10],["box", 12],["box", 14],["box", 30]]), function(x) {return {"referent": x.referent}}));

viz.table(marginalize(pragmaticListener(descriptions[0], [["box", 25],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[0], [["box", 20],["box", 25],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[0], [["box", 15],["box", 20],["box", 25],["box", 30]]), function(x) {return {"referent": x.referent}}));

viz.table(marginalize(pragmaticListener(descriptions[1], [["box", 25],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[1], [["box", 20],["box", 25],["box", 30]]), function(x) {return {"referent": x.referent}}));
viz.table(marginalize(pragmaticListener(descriptions[1], [["box", 15],["box", 20],["box", 25],["box", 30]]), function(x) {return {"referent": x.referent}}));


//returns a distribution on descriptions
// var pragmaticSpeaker = function() {
// 	return Infer({method: "enumerate"}, function() {
//       // Infer over descriptions.
      
//       var description = uniformDraw(descriptions);
//       var context = referents;
//       var r = context[context.length - 1];
      
//       factor(marginalize(pragmaticListener(description, context), "referent").score(r))
//       return {"description": surfaceForm[description[0]]};
// 	})
// }

// viz.table(pragmaticSpeaker());

//           BIGGER     BIGGEST
// COARSE 2: .61        .30
// COARSE 3: .45        .47
// COARSE 4: .45        .50

//           BIGGER     BIGGEST
// PROGR. 2: .61        .30
// PROGR. 3: .28        .60
// PROGR. 4: .18        .72





//           BIGGER     BIGGEST
// COARSE 2: .43        .43
// COARSE 3: .34        .56
// COARSE 4: .33        .60

//           BIGGER     BIGGEST
// PROGR. 2: .43        .43
// PROGR. 3: .22        .66
// PROGR. 4: .10        .80
