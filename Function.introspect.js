//  Function.introspect.js 1.0
//      Copyright (c) 2009-2010 Jonathan 'Wolf' Rentzsch: <http://rentzsch.com>
//      Some rights reserved: <http://opensource.org/licenses/mit-license.php>
//
//  Interrogates a function to return its name, argument names and source code (if not native).
//
//  Compatibility: IE 6-8, Firefox 3-3.5, Safari 3-4, Chrome 3.
//
// http://github.com/rentzsch/Function.introspect.js
//

if (Function.introspect === undefined) {
	Function.introspect = function (func) {
		var functionName,
			functionArgs = '',
			functionArgNames = [],
			functionCode,
			functionIsNative = false,
			regexResult;

		var functionCode = func.toString();

		if (functionCode.indexOf('(') !== -1) {
			// Grep function's name, if any.
			regexResult = /function ([^(]+)/.exec(functionCode);
			if (regexResult !== null) {
				functionName = regexResult[1];
				//functionName = functionName.replace(/\s*(.*)\s*/,'$1');
				functionName = functionName.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
			}
			// Grep functions args, if any.
			regexResult = /\((.*\w.*)\)/.exec(functionCode);
			if (regexResult !== null) {
				functionArgs = regexResult[1];
				//  Toss whitespace.
				functionArgs = functionArgs.replace(/\s/g,'');
				functionArgNames = functionArgs.split(',');
			}

			// Strip 'function(){' portion off code string.
			functionCode = functionCode.substring(functionCode.indexOf('{')+1, functionCode.lastIndexOf('}'));
			//  Trim whitespace.
			functionCode = functionCode.replace(/^\s\s*/, '').replace(/\s\s*$/, '');

			if (functionCode === '[native code]') {
				functionIsNative = true;
			}
		} else {
			// QUIRK
			// For TypeError.toString(), IE6-7 return '[object Error]' while IE8 returns just 'Error'.
			// Fortunately IE6-8 support TypeError.name, which return 'TypeError'.
			functionName = func.name;
			functionCode = '[native code]';
			functionIsNative = true;
		}

		return {name:functionName, argNames:functionArgNames, args:functionArgs, code:functionCode, isNative:functionIsNative};
	};
}