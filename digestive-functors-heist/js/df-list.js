// Requires that JQuery also be in scope
function endsWith(str, suffix) {
  if ( typeof str === "undefined" )
    return false;
  return str.indexOf(suffix, str.length - suffix.length) !== -1;
}

function findParentClass(curElement, classSuffix) {
  var mainDiv = $(curElement).parent();
  while ( !endsWith(mainDiv.attr('class'), classSuffix) ) {
    mainDiv = $(mainDiv).parent();
  }
  return mainDiv;
}

function findItems(listTop, prefix) {
  var selector = '[class ^= "'+prefix+'.inputList"]';
  return $(selector, listTop);
}

function findTemplate(listTop, prefix) {
  var selector = '[class = "'+prefix+'.inputListTemplate"]';
  return $(selector, listTop);
}

function reIndex(item, prefix, curInd, newInd) {
  var itemObj = $(item);
  var pre = prefix+'.'
  var a = pre+curInd
  var b = pre+newInd

  // We have to change the raw html because IE doesn't allow the
  // name field to be changed.
  var newH = itemObj.html().replace(new RegExp(a, 'g'), b);
  itemObj.html(newH);
  itemObj.attr('data-ind', b);
}

function getIndicesElem(listTop, prefix) {
  var indicesSelector = '[name = "'+prefix+'.indices"]';
  return $(indicesSelector, listTop);
}

function getIndices(indicesElem) {
  var indicesText = indicesElem.val().split(',');
  if ( indicesText[0] == '' ) {
    indicesText[0] = '-1';
  }
  return indicesText.map(function(n){return parseInt(n,10)});
}

function removeIndex(listTop, prefix, ind) {
  var indicesElem = getIndicesElem(listTop, prefix);
  var indices = getIndices(indicesElem);
  var newIndices = [];
  var i;
  for ( i = 0; i < indices.length; i++ ) {
    if ( indices[i] != ind ) {
      newIndices.push(indices[i]);
    }
  }

  var newIndicesString = "";
  if ( newIndices.length > 0 ) {
    newIndicesString = newIndices[0].toString();
    for ( i = 1; i < newIndices.length; i++ ) {
      newIndicesString += ',' + newIndices[i];
    }
  }
  var a = indicesElem.val();
  indicesElem.val(newIndicesString);
  var b = indicesElem.val();
}

function addInputListItem(button, prefix) {
  var listTop = findParentClass(button, 'inputList');
  var indicesElem = getIndicesElem(listTop, prefix);
  var indices = getIndices(indicesElem);
  var nextIndex = Math.max.apply(Math, indices)+1;
  var cur = indicesElem.val();
  if ( cur != '' ) {
    cur = cur + ',';
  }
  indicesElem.val(cur+nextIndex);

  $('.inputListInstance', listTop).each(function(i, instanceTop) {
    var template = findTemplate(instanceTop, prefix)[0];
    var pathComponents = $(template).attr('data-ind').split('.');
    var ind = parseInt(pathComponents[pathComponents.length-1]);
    var newItem = $(template).clone(true);

    reIndex(newItem, prefix, ind, nextIndex);
    newItem.removeAttr('style')
           .removeClass(prefix+'.inputListTemplate')
           .addClass(prefix+'.inputListItem');
    var items = findItems(instanceTop, prefix);
    newItem.appendTo($(items[items.length-1]).parent());
  })
}

function removeInputListItem(button, prefix) {
  var listTop = findParentClass(button, 'inputList');
  var curListItem = findParentClass(button, 'inputListItem');
  var itemPrefix = curListItem.attr('data-ind');
  var prefixArr = itemPrefix.split('.');
  var curInd = parseInt(prefixArr[prefixArr.length-1]);
  removeIndex(listTop, prefix, curInd);

  $('.inputListInstance', listTop).each(function(i, instanceTop) {
    var r = prefix + '.' + curInd;
    $('[data-ind="' + r +'"]', instanceTop).remove();
  });
}

