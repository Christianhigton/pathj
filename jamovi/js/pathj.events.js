
const DEBUG=true;

const events = {
    update: function(ui) {
      console.log("general update");
      initializeAll(ui, this);
    },

    onChange_factors: function(ui) {
         markGuiBuilderEdit(ui, this);
         updateSuppliers(ui,this);
         updateContrasts(ui,this);
    },

    onChange_endogenous: function(ui) {
       markGuiBuilderEdit(ui, this);
       prepareEndogenousTerms(ui,this);
       updateSuppliers(ui,this);
       updateScaling(ui,this);

    },

    onChange_covariates: function(ui) {
        markGuiBuilderEdit(ui, this);
        updateSuppliers(ui,this);
        updateScaling(ui,this)
        
    },


    onChange_endogenousSupplier: function(ui) {
       log("endogenousSupplier has changed");
       cleanEndogenousTerms(ui, this);
//       fromSupplierToEndogenousTerms(ui, this);

    },

    onUpdate_endogenousSupplier: function(ui) {
            log("endogenousSupplier update");
            let factorsList = this.cloneArray(ui.factors.value(), []);
            let covariatesList = this.cloneArray(ui.covs.value(), []);
            var variablesList = factorsList.concat(covariatesList);
            ui.endogenousSupplier.setValue(this.valuesToItems(variablesList, FormatDef.variable));

    },

     onChange_endogenousTerms: function(ui) {
      markGuiBuilderEdit(ui, this);
      cleanRecursiveTerms(ui,this);
    },


     onEvent_nothing: function(ui) {
      console.log("I did not do anything");
    },

     onChange_nothing: function(ui) {
      console.log("I did not do anything");
    },
     onChange_varcovSupplier: function(ui) {
      markGuiBuilderEdit(ui, this);
      console.log("varcovsup change");
       let values = this.itemsToValues(ui.varcovSupplier.value());
        this.checkPairsValue(ui.varcov, values);
      
    },
    onChange_syntaxApply: function(ui) {
      syncSyntaxEditorToOption(ui);
      populateGuiFromSyntax(ui, this);
    },
    onChange_syntaxSource: function(ui) {
      updateSyntaxEditor(ui);
    },
    onCreate_syntaxEditor: function(ui) {
      createSyntaxEditor(ui, this);
    },
    onUpdate_syntaxEditor: function(ui) {
      createSyntaxEditor(ui, this);
      updateSyntaxEditor(ui);
    },
    onChange_syntaxExampleCopy: function(ui) {
      copySelectedSyntaxExample(ui);
    },
    onChange_syntaxExampleInsert: function(ui) {
      insertSelectedSyntaxExample(ui);
    },
    onChange_syntaxExampleChoice: function(ui) {
      updateSyntaxEditor(ui);
    },
    onUpdate_varcovSupplier: function(ui) {
      console.log("varcovsup update");
      
    },


};



var initializeAll = function(ui, context) {
    
    updateSuppliers(ui,context);
    prepareEndogenousTerms(ui,context);

};

var markGuiBuilderEdit = function(ui, context) {
    if (context.workspace !== undefined && context.workspace.importingSyntax === true)
        return;
    if (getOptionValue(ui, "syntaxSource", "gui") !== "gui") {
        setOptionValue(ui, "syntaxSource", "gui");
        setOptionValue(ui, "syntaxApply", false);
        updateSyntaxEditor(ui);
    }
};



var updateSuppliers= function(ui,context) {

   // here we transfer all variables in the supplier for the endogenous models
   
    var factorsList = context.cloneArray(ui.factors.value(), []);
    var covariatesList = context.cloneArray(ui.covs.value(), []);
    var indList = factorsList.concat(covariatesList);
    var endogenousList = context.cloneArray(ui.endogenous.value(), []);
    var allList = factorsList.concat(covariatesList).concat(endogenousList);
    ui.endogenousSupplier.setValue(context.valuesToItems(allList, FormatDef.variable));
    ui.varcovSupplier.setValue(context.valuesToItems(allList, FormatDef.variable));
    context.workspace.endogenousSupplierList=allList;
    context.workspace.varcovSupplierList=allList;
    

};

var prepareEndogenousTerms= function(ui,context) {

     // here we prepare a list of lists, one of each endogenous variable.
     // we also want to put a label to show which dependent variable the user is working on

     console.log("prepareEndogenousTerms");
     var endogenous = context.cloneArray(ui.endogenous.value(),[]);
     var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);
     
 
     // we make sure that there are enough arrays in the array list, each for each endogeneous
     var okList= [];
     for (var i = 0; i < endogenous.length; i++) {
         var aList = endogenousTerms[i] === undefined ? [] : endogenousTerms[i] ;
             okList.push(aList);
     }
      console.log(okList);
      ui.endogenousTerms.setValue(okList);    
      
     // we give a label for each endogeneous model
     labelize(ui.endogenousTerms,endogenous, "Endogenous");  
     storeComponent("endogenousTerms",endogenousTerms,context);
  
  
};


var cleanRecursiveTerms= function(ui,context) {

    console.log("cleanRecursiveTerms");
    var endogenous = context.cloneArray(ui.endogenous.value(), []);
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);

    for (var i = 0; i < endogenous.length; i++)
        endogenousTerms[i]=removeFromList(endogenous[i],endogenousTerms[i],context,1);

    for (var i = 0; i < endogenousTerms.length; i++) {
       for (var j = 0; j < endogenousTerms[i].length; j++) {
         endogenousTerms[i][j].sort();
       }
      
    }
        
    ui.endogenousTerms.setValue(endogenousTerms);
    storeComponent("endogenousTerms",endogenousTerms,context);

};


var cleanEndogenousTerms= function(ui,context) {

    console.log("cleanEndogenousTerms");
    var endogenous = context.cloneArray(ui.endogenous.value(), []);
    var endogenousTerms = context.cloneArray(ui.endogenousTerms.value(),[]);

    for (var i = 0; i < endogenous.length; i++)
        endogenousTerms[i]=removeFromList(endogenous[i],endogenousTerms[i],context,1);

    var endogenousSupplierList = context.cloneArray(context.itemsToValues(ui.endogenousSupplier.value()),[]);
    var diff = context.findChanges("endogenousSupplierList",endogenousSupplierList,context);
    if (diff.hasChanged) {
      for (var i = 0; i < endogenous.length; i++) 
           for (var j = 0; j < diff.removed.length; j++) {
                console.log(diff.removed[j]);
                endogenousTerms[i]=removeFromList(diff.removed[j],endogenousTerms[i],context,1);
           }
           
    }
    ui.endogenousTerms.setValue(endogenousTerms);
    storeComponent("endogenousTerms",endogenousTerms,context);

};






var updateScaling = function(ui,context) {
    log("updateScaling");
    var currentList = context.cloneArray(ui.scaling.value(), []);
    var variableList1 = context.cloneArray(ui.covs.value(), []);
    var variableList2 = context.cloneArray(ui.endogenous.value(), []);
    var variableList = variableList2.concat(variableList1);

    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "none" });
        else
            list3.push(found);
    }

    ui.scaling.setValue(list3);
};

var updateContrasts = function(ui, context) {
    var currentList = context.cloneArray(ui.contrasts.value(), []);
    var variableList = context.cloneArray(ui.factors.value(), [])
    var list3 = [];
    for (let i = 0; i < variableList.length; i++) {
        let found = null;
        for (let j = 0; j < currentList.length; j++) {
            if (currentList[j].var === variableList[i]) {
                found = currentList[j];
                break;
            }
        }
        if (found === null)
            list3.push({ var: variableList[i], type: "simple" });
        else
            list3.push(found);
    }

    ui.contrasts.setValue(list3);
};

var createSyntaxEditor = function(ui, context) {
    var control = ui.syntaxEditor;
    if (control === undefined || control.$el === undefined)
        return;

    var root = control.$el[0];
    if (root === undefined || root.querySelector(".pathj-syntax-editor") !== null)
        return;

    root.innerHTML = "";
    var wrap = document.createElement("div");
    wrap.className = "pathj-syntax-editor";
    wrap.style.width = "360px";
    wrap.style.maxWidth = "100%";
    wrap.style.boxSizing = "border-box";

    var textarea = document.createElement("textarea");
    textarea.className = "pathj-syntax-textarea";
    textarea.placeholder = "Paste lavaan or Mermaid syntax here";
    textarea.value = getOptionValue(ui, "syntaxText", "");
    textarea.style.width = "100%";
    textarea.style.minHeight = "70px";
    textarea.style.resize = "vertical";
    textarea.style.boxSizing = "border-box";
    textarea.style.fontFamily = "Menlo, Consolas, monospace";
    textarea.style.fontSize = "12px";
    textarea.style.lineHeight = "1.35";
    textarea.style.border = "1px solid #aaa";
    textarea.style.borderRadius = "3px";
    textarea.style.padding = "8px";

    textarea.addEventListener("input", function() {
        setOptionValue(ui, "syntaxText", textarea.value);
    });

    var sample = document.createElement("textarea");
    sample.className = "pathj-syntax-sample";
    sample.readOnly = true;
    sample.style.width = "100%";
    sample.style.minHeight = "70px";
    sample.style.marginTop = "8px";
    sample.style.resize = "vertical";
    sample.style.boxSizing = "border-box";
    sample.style.fontFamily = "Menlo, Consolas, monospace";
    sample.style.fontSize = "12px";
    sample.style.lineHeight = "1.35";
    sample.style.border = "1px solid #c7c7c7";
    sample.style.borderRadius = "3px";
    sample.style.padding = "8px";
    sample.style.backgroundColor = "#f7f7f7";

    wrap.appendChild(textarea);
    wrap.appendChild(sample);
    root.appendChild(wrap);
    updateSyntaxEditor(ui);
};

var updateSyntaxEditor = function(ui) {
    var control = ui.syntaxEditor;
    if (control === undefined || control.$el === undefined)
        return;
    var root = control.$el[0];
    var textarea = root.querySelector(".pathj-syntax-textarea");
    var sample = root.querySelector(".pathj-syntax-sample");
    var wrap = root.querySelector(".pathj-syntax-editor");
    if (wrap !== null)
        wrap.style.display = getOptionValue(ui, "syntaxSource", "gui") === "gui" ? "none" : "block";
    if (textarea !== null && document.activeElement !== textarea)
        textarea.value = getOptionValue(ui, "syntaxText", "");
    if (sample !== null)
        sample.value = getSelectedSyntaxExample(ui);
};

var syncSyntaxEditorToOption = function(ui) {
    var control = ui.syntaxEditor;
    if (control === undefined || control.$el === undefined)
        return;
    var textarea = control.$el[0].querySelector(".pathj-syntax-textarea");
    if (textarea !== null)
        setOptionValue(ui, "syntaxText", textarea.value);
};

var getSelectedSyntaxExample = function(ui) {
    var choice = getOptionValue(ui, "syntaxExampleChoice", "lavaan");
    var example = choice === "mermaid" ? getOptionValue(ui, "syntaxExampleMermaid", "") : getOptionValue(ui, "syntaxExampleLavaan", "");
    return example.replace(/\\n/g, "\n");
};

var insertSelectedSyntaxExample = function(ui) {
    var example = getSelectedSyntaxExample(ui);
    setOptionValue(ui, "syntaxText", example);
    var control = ui.syntaxEditor;
    if (control !== undefined && control.$el !== undefined) {
        var textarea = control.$el[0].querySelector(".pathj-syntax-textarea");
        if (textarea !== null)
            textarea.value = example;
    }
};

var copySelectedSyntaxExample = function(ui) {
    var example = getSelectedSyntaxExample(ui);
    if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(example).catch(function() {
            fallbackCopyText(example);
        });
    } else {
        fallbackCopyText(example);
    }
};

var fallbackCopyText = function(text) {
    var node = document.createElement("textarea");
    node.value = text;
    node.style.position = "fixed";
    node.style.left = "-9999px";
    document.body.appendChild(node);
    node.focus();
    node.select();
    try {
        document.execCommand("copy");
    } catch (e) {
        console.log(e);
    }
    document.body.removeChild(node);
};

var getOptionValue = function(ui, name, fallback) {
    if (ui[name] === undefined || ui[name].value === undefined)
        return fallback;
    var value = ui[name].value();
    if (value === null || value === undefined)
        return fallback;
    return value;
};

var setOptionValue = function(ui, name, value) {
    if (ui[name] !== undefined && ui[name].setValue !== undefined)
        ui[name].setValue(value);
};

var populateGuiFromSyntax = function(ui, context) {
    syncSyntaxEditorToOption(ui);
    var source = ui.syntaxSource.value();
    if (source === "gui")
        return;

    var syntax = ui.syntaxText.value();
    if (syntax === null || syntax === undefined)
        return;

    var paths = source === "mermaid" ? parseMermaidPaths(syntax) : parseLavaanPaths(syntax);
    if (paths.length === 0)
        return;

    var endogenous = [];
    var predictors = [];
    var allVars = [];

    for (var i = 0; i < paths.length; i++) {
        addUnique(endogenous, paths[i].lhs);
        addUnique(allVars, paths[i].lhs);
        for (var j = 0; j < paths[i].rhs.length; j++) {
            addUnique(predictors, paths[i].rhs[j]);
            addUnique(allVars, paths[i].rhs[j]);
        }
    }

    var covs = [];
    for (var k = 0; k < predictors.length; k++) {
        if (endogenous.indexOf(predictors[k]) === -1)
            covs.push(predictors[k]);
    }

    context.workspace.importingSyntax = true;
    ui.endogenous.setValue(endogenous);
    ui.covs.setValue(covs);
    ui.syntaxVars.setValue(allVars);

    var terms = [];
    for (var e = 0; e < endogenous.length; e++) {
        var rhs = [];
        for (var p = 0; p < paths.length; p++) {
            if (paths[p].lhs === endogenous[e]) {
                for (var r = 0; r < paths[p].rhs.length; r++)
                    addUnique(rhs, paths[p].rhs[r]);
            }
        }
        terms.push(rhs);
    }
    ui.endogenousTerms.setValue(terms);
    updateSuppliers(ui, context);
    context.workspace.importingSyntax = false;
};

var parseLavaanPaths = function(syntax) {
    var lines = syntax.replace(/;/g, "\n").split(/\n/);
    var paths = [];
    for (var i = 0; i < lines.length; i++) {
        var line = lines[i].trim();
        if (line.length === 0 || line.indexOf("#") === 0)
            continue;
        var match = line.match(/^([^~]+)~([^~].*)$/);
        if (match === null)
            continue;
        var lhs = cleanVarName(match[1]);
        var rhs = match[2].split("+").map(cleanVarName).filter(function(x) { return x.length > 0; });
        if (lhs.length > 0 && rhs.length > 0)
            paths.push({ lhs: lhs, rhs: rhs });
    }
    return paths;
};

var parseMermaidPaths = function(syntax) {
    var text = syntax.replace(/;/g, " ");
    text = text.replace(/^\s*(graph|flowchart)\s+[A-Za-z]+\s*/i, "");
    var node = "[A-Za-z0-9_.]+(?:\\[[^\\]]+\\]|\\([^\\)]*\\)|\\{[^\\}]*\\})?";
    var regex = new RegExp("(" + node + ")\\s*-->\\s*(?:\\|[^|]*\\|\\s*)?(" + node + ")", "g");
    var paths = [];
    var match;
    while ((match = regex.exec(text)) !== null) {
        var rhs = cleanMermaidNode(match[1]);
        var lhs = cleanMermaidNode(match[2]);
        if (lhs.length > 0 && rhs.length > 0)
            paths.push({ lhs: lhs, rhs: [ rhs ] });
    }
    return paths;
};

var cleanMermaidNode = function(value) {
    value = value.trim();
    value = value.replace(/\[.*$/, "");
    value = value.replace(/\(.*$/, "");
    value = value.replace(/\{.*$/, "");
    return cleanVarName(value);
};

var cleanVarName = function(value) {
    value = value.trim();
    value = value.replace(/^`|`$/g, "");
    value = value.replace(/^["']|["']$/g, "");
    return value.trim();
};

var addUnique = function(list, value) {
    if (value.length > 0 && list.indexOf(value) === -1)
        list.push(value);
};



// helper functions

// add an item or a list of items (quantum) to a list (cosmos).
// it tries to understand what kind of input it has, but it is not clear
// when it works
var addToList = function(quantum, cosmos, context) {
  
    cosmos = normalize(context.cloneArray(cosmos));
    quantum = normalize(context.cloneArray(quantum));
    
    for (var i = 0; i < quantum.length; i++) {
          if (dim(quantum[i])===0)
              cosmos.push([quantum[i]]);
          if (dim(quantum[i])===1)
              cosmos.push(quantum[i]);
          }
    return unique(cosmos);
};


var removeFromMultiList = function(quantum, cosmos, context, strict = 1) {

    var cosmos = context.cloneArray(cosmos);
    var dimq = dim(quantum);
        for (var j = 0; j < cosmos.length; j++) 
           cosmos[j]=removeFromList(quantum,cosmos[j],context, strict);
    return(cosmos);
};



// remove a list or a item from list
// order=0 remove only if term and target term are equal
// order>0 remove if term length>=order 
// for instance, order=1 remove any matching interaction with terms, keeps main effects
// order=2 remove from 3-way interaction on (keep up to 2-way interactions)

var removeFromList = function(quantum, cosmos, context, order = 1) {

     cosmos=normalize(cosmos);
     quantum=normalize(quantum);
     if (cosmos===undefined)
        return([]);
     var cosmos = context.cloneArray(cosmos);
       for (var i = 0; i < cosmos.length; i++) {
          if (cosmos[i]===undefined)
             break;
          var aCosmos = context.cloneArray(cosmos[i]);
           for (var k = 0; k < quantum.length; k++) {
             var  test = order === 0 ? FormatDef.term.isEqual(aCosmos,quantum[k]) : FormatDef.term.contains(aCosmos,quantum[k]);
                 if (test && (aCosmos.length >= order)) {
                        cosmos.splice(i, 1);
                        i -= 1;
                    break;    
                    }
          }
            
       }
  
    return(cosmos);
};




var unique=function(arr) {
    var u = {}, a = [];
    for(var i = 0, l = arr.length; i < l; ++i){
        var prop=ssort(JSON.stringify(arr[i]));
        if(!u.hasOwnProperty(prop) && arr[i].length>0) {
            a.push(arr[i]);
            u[prop] = 1;
        }
    }
    return a;
};

var ssort= function(str){
  str = str.replace(/[`\[\]"\\\/]/gi, '');
  var arr = str.split(',');
  var sorted = arr.sort();
  return sorted.join('');
}

var labelize = function(widget, labels, prefix) {

     widget.applyToItems(0, (item, index) => {
           item.controls[0].setPropertyValue("label",prefix +" = "+labels[index]);
        });
};


var flatMulti = function(cosmos,context) {
  var light = []
  for (var i=0 ; i < cosmos.length; i++) {
    light=addToList(light,cosmos[i],context);
  }
  return unique(light);
};

var combineOne = function(values, mod, context) {
        if (mod===undefined)
            return(values);
        var list = unique(values.concat([mod]));
        for (var i = 0; i < values.length; i++) {
            var newValue = context.clone(mod);
            var value = values[i];
            if (context.listContains(value,newValue[0],FormatDef.term)===false)
                   if (FormatDef.term.isEqual(newValue,value)===false) {
                      if (Array.isArray(value)) 
                          newValue = newValue.concat(value);
                      else
                         newValue.push(value);   
            list.push(newValue);
            }
        }
        return unique(list);
};

var getInteractions = function(aList,context,order=2) {
  
  var iList = context.getCombinations(aList);
  for (var i = 0; i < iList.length; i++ )
          if (iList[i].length===1 || iList[i].length>order) {
              iList.splice(i, 1);
               i -= 1;
          }
 return(iList);
  
};

var normalize = function(cosmos) {

  if (cosmos===undefined)
          return [];
  if (dim(cosmos)===0)
          cosmos=[cosmos]
          
        for (var i = 0; i < cosmos.length; i++) {
            var aValue = cosmos[i];
            var newValue=dim(aValue)>0 ? aValue : [aValue];
            cosmos[i]=newValue
        }
        return cosmos;
}

// get interaction between two lists
// order==0 include main effects
// 1  only interaction


var combine = function(cosmos1, cosmos2 , context, order=2) {
        if (cosmos1===cosmos2)
               return;
        if (cosmos1===undefined)
               return order===0 ? cosmos2 : [] ;
        if (cosmos2===undefined)
               return order===0 ? cosmos1 : [] ;
        cosmos1 = normalize(cosmos1)
        cosmos2 = normalize(cosmos2)        

        var light=[];
        for (var i = 0; i < cosmos1.length; i++) {
            var aValue1 = context.clone(cosmos1[i]);
            for (var j = 0 ; j < cosmos2.length; j ++) {
            var aValue2 = context.clone(cosmos2[j]);
            var join = aValue1.concat(aValue2);
            if (join.length===unique(join).length)
                   light.push(join);
              }
            }

        if (order===0)
              light = cosmos1.concat(cosmos2).concat(light);
        return unique(light);
};


var dim = function(aList) {

    if (!Array.isArray(aList))
           return(0);
    if (!Array.isArray(aList[0]))
           return(1);
    if (!Array.isArray(aList[0][0]))
           return(2);
    if (!Array.isArray(aList[0][0][0]))
           return(3);
    if (!Array.isArray(aList[0][0][0][0]))
           return(4);

  
    return(value);
};

var findChangesMulti= function(id,cosmos,context,save=true) {

  var old = context.workspace[id];
 if (old===undefined)
        old=[];
      
  var light = [];
  var len = Math.max(cosmos.length,old.length);
  var changeIndex = -1;
  for (var i = 0; i < len; i++) {
    var photon=[];
    photon.added = removeFromList(old[i], cosmos[i], context, 0);
    photon.removed = removeFromList(cosmos[i],old[i], context, 0);
    if  (photon.added.length > 0 || photon.removed.length > 0) 
          changeIndex = i
    light[i] = photon;
}
  light = { changes: light, index: changeIndex };
  if (save)
        storeComponent(id,cosmos,context);
  return(light);
};

var storeComponent = function(id,cosmos,context) {
                    context.workspace[id]=cosmos;
};

var log=function(obj) {
    if (DEBUG)
      console.log(obj);
};

var dlog=function(obj) {
    if (DEBUG)
      console.log(obj);
};

var flashMGridOptionListControl = function(widget,note=false) {
        var i =widget.getSelectedRowIndices();
        if (i.length==0) i=0;
        widget.controls[i].controls[1].$el[0].style.backgroundColor="#ffcccc";
        widget.controls[i].controls[1].$el[0].style.borderColor="red";
        if (note!==false)
           note.$el[0].style.visibility="visible";
        

};

var unflashMGridOptionListControl = function(widget,note=false) {
        var i =widget.getSelectedRowIndices();
        if (i.length==0) i=0;
        widget.controls[i].controls[1].$el[0].style.backgroundColor="white";
        widget.controls[i].controls[1].$el[0].style.borderColor="rgb(46,138,199)";
        if (note!==false)
           note.$el[0].style.visibility="hidden";
};

var   cleanInteractions= function(quantum,cosmos,context){
  if (quantum===undefined)
    return(cosmos);
    
  var deny= context.getCombinations(quantum)  
  for (var i=0; i < deny.length; i++) {
    if (deny[i].length==1) {
      deny.splice(i, 1);
      i -= 1;
    }
  }
  cosmos=context.cloneArray(cosmos)
  for (var j=0; j< cosmos.length; j++) {
    for (var i=0; i < deny.length; i++) {
      if (FormatDef.term.contains(cosmos[j],deny[i])) {
        cosmos.splice(j,1);
        j -= 1;
        break;
      }
  }
}
 return cosmos;
  
};


module.exports = events;
