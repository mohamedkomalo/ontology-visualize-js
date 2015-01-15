/** @license MIT - N3.js library (browser version) - ©Ruben Verborgh */
(function (N3) {
// **N3Util** provides N3 utility functions

var XsdString = 'http://www.w3.org/2001/XMLSchema#string';
var RdfLangString = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#langString';

var N3Util = {
  // Tests whether the given entity (triple object) represents a URI in the N3 library
  isUri: function (entity) {
    if (!entity)
      return entity;
    var firstChar = entity[0];
    return firstChar !== '"' && firstChar !== '_';
  },

  // Tests whether the given entity (triple object) represents a literal in the N3 library
  isLiteral: function (entity) {
    return entity && entity[0] === '"';
  },

  // Tests whether the given entity (triple object) represents a blank node in the N3 library
  isBlank: function (entity) {
    return entity && entity.substr(0, 2) === '_:';
  },

  // Gets the string value of a literal in the N3 library
  getLiteralValue: function (literal) {
    var match = /^"([^]*)"/.exec(literal);
    if (!match)
      throw new Error(literal + ' is not a literal');
    return match[1];
  },

  // Gets the type of a literal in the N3 library
  getLiteralType: function (literal) {
    var match = /^"[^]*"(?:\^\^([^"]+)|(@)[^@"]+)?$/.exec(literal);
    if (!match)
      throw new Error(literal + ' is not a literal');
    return match[1] || (match[2] ? RdfLangString : XsdString);
  },

  // Gets the language of a literal in the N3 library
  getLiteralLanguage: function (literal) {
    var match = /^"[^]*"(?:@([^@"]+)|\^\^[^"]+)?$/.exec(literal);
    if (!match)
      throw new Error(literal + ' is not a literal');
    return match[1] ? match[1].toLowerCase() : '';
  },

  // Tests whether the given entity (triple object) represents a prefixed name
  isPrefixedName: function (entity) {
    return entity && /^[^:\/"']*:[^:\/"']+$/.test(entity);
  },

  // Expands the prefixed name to a full URI (also when it occurs as a literal's type)
  expandPrefixedName: function (prefixedName, prefixes) {
    var match = /(?:^|"\^\^)([^:\/#"'\^_]*):[^\/]/.exec(prefixedName);
    if (!match) return prefixedName;

    var prefix = match[1], base = prefixes[prefix], index = match.index;
    if (base === undefined)
      throw new Error('Unknown prefix: ' + prefix);

    // The match index is non-zero when expanding a literal's type.
    return index === 0 ? base + prefixedName.substr(prefix.length + 1)
                       : prefixedName.substr(0, index + 3) +
                         base + prefixedName.substr(index + prefix.length + 4);
  },
};

// Add the N3Util functions to the given object or its prototype
function AddN3Util(parent, toPrototype) {
  for (var name in N3Util)
    if (!toPrototype)
      parent[name] = N3Util[name];
    else
      parent.prototype[name] = ApplyToThis(N3Util[name]);

  return parent;
}

// Returns a function that applies `f` to the `this` object
function ApplyToThis(f) {
  return function (a) { return f(this, a); };
}

// Expose N3Util, attaching all functions to it

N3.Util = AddN3Util(AddN3Util);
// **N3Lexer** tokenizes N3 documents.
var fromCharCode = String.fromCharCode;
var immediately = typeof setImmediate === 'function' ? setImmediate :
                  function setImmediate(func) { setTimeout(func, 0); };

// Regular expression and replacement string to escape N3 strings.
// Note how we catch invalid unicode sequences separately (they will trigger an error).
var escapeSequence = /\\u([a-fA-F0-9]{4})|\\U([a-fA-F0-9]{8})|\\[uU]|\\(.)/g;
var escapeReplacements = { '\\': '\\', "'": "'", '"': '"',
                           'n': '\n', 'r': '\r', 't': '\t', 'f': '\f', 'b': '\b',
                           '_': '_', '~': '~', '.': '.', '-': '-', '!': '!', '$': '$', '&': '&',
                           '(': '(', ')': ')', '*': '*', '+': '+', ',': ',', ';': ';', '=': '=',
                           '/': '/', '?': '?', '#': '#', '@': '@', '%': '%' };
var illegalIriChars = /[\x00-\x20<>\\"\{\}\|\^\`]/;

// ## Constructor
function N3Lexer() {
  if (!(this instanceof N3Lexer))
    return new N3Lexer();
}

N3Lexer.prototype = {
  // ## Regular expressions
  // It's slightly faster to have these as properties than as in-scope variables.

  _iri: /^<((?:[^>\\]|\\[uU])+)>/, // IRI with escape sequences; needs sanity check after unescaping
  _unescapedIri: /^<([^\x00-\x20<>\\"\{\}\|\^\`]*)>/, // IRI without escape sequences; no unescaping
  _unescapedString: /^"[^"\\]+"(?=[^"\\])/, // non-empty string without escape sequences
  _singleQuotedString: /^"[^"\\]*(?:\\.[^"\\]*)*"(?=[^"\\])|^'[^'\\]*(?:\\.[^'\\]*)*'(?=[^'\\])/,
  _tripleQuotedString: /^""("[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*")""|^''('[^'\\]*(?:(?:\\.|'(?!''))[^'\\]*)*')''/,
  _langcode: /^@([a-z]+(?:-[a-z0-9]+)*)(?=[^a-z0-9\-])/i,
  _prefix: /^((?:[A-Za-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])(?:[\.\-0-9A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])*)?:(?=\s|<)/,
  _prefixedName: /^((?:[A-Z_a-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])(?:[\.\-0-9A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff])*)?:((?:(?:[0-:A-Z_a-z\xc0-\xd6\xd8-\xf6\xf8-\u02ff\u0370-\u037d\u037f-\u1fff\u200c\u200d\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff]|%[0-9a-fA-F]{2}|\\[!#-\/;=?\-@_~])(?:(?:[\.\-0-:A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff]|%[0-9a-fA-F]{2}|\\[!#-\/;=?\-@_~])*(?:[\-0-:A-Z_a-z\xb7\xc0-\xd6\xd8-\xf6\xf8-\u037d\u037f-\u1fff\u200c\u200d\u203f\u2040\u2070-\u218f\u2c00-\u2fef\u3001-\ud7ff\uf900-\ufdcf\ufdf0-\ufffd]|[\ud800-\udb7f][\udc00-\udfff]|%[0-9a-fA-F]{2}|\\[!#-\/;=?\-@_~]))?)?)(?=[,;\s#()\[\]]|\.[\s#()\[\]<"'])/,
  _number: /^[\-+]?(?:\d+\.?\d*([eE](?:[\-\+])?\d+)|\d*\.?\d+)(?=[.,;\s#()\[\]])/,
  _boolean: /^(?:true|false)(?=[.,;\s#()\[\]])/,
  _keyword: /^@[a-z]+(?=\s)/,
  _sparqlKeyword: /^(?:PREFIX|BASE)(?=\s)/i,
  _shortPredicates: /^a(?=\s+|<)/,
  _newline: /^[ \t]*(?:#[^\n\r]*)?(?:\r\n|\n|\r)[ \t]*/,
  _whitespace: /^[ \t]+/,
  _endOfFile: /^(?:#[^\n\r]*)?$/,


  // ## Private methods

  // ### `_tokenizeToEnd` tokenizes as for as possible, emitting tokens through the callback.
  _tokenizeToEnd: function (callback, inputFinished) {
    // Continue parsing as far as possible; the loop will return eventually.
    var input = this._input;
    while (true) {
      // Count and skip whitespace lines.
      var whiteSpaceMatch;
      while (whiteSpaceMatch = this._newline.exec(input))
        input = input.substr(whiteSpaceMatch[0].length, input.length), this._line++;
      // Skip whitespace on current line.
      if (whiteSpaceMatch = this._whitespace.exec(input))
        input = input.substr(whiteSpaceMatch[0].length, input.length);

      // Stop for now if we're at the end.
      if (this._endOfFile.test(input)) {
        // If the input is finished, emit EOF.
        if (inputFinished)
          callback(input = null, { line: this._line, type: 'eof', value: '', prefix: '' });
        return this._input = input;
      }

      // Look for specific token types based on the first character.
      var line = this._line, type = '', value = '', prefix = '',
          firstChar = input[0], match = null, matchLength = 0, unescaped, inconclusive = false;
      switch (firstChar) {
      case '^':
        // Try to match a type.
        if (input.length === 1) break;
        else if (input[1] !== '^') return reportSyntaxError(this);
        this._prevTokenType = '^';
        // Move to type IRI or prefixed name.
        input = input.substr(2);
        if (input[0] !== '<') {
          inconclusive = true;
          break;
        }
        // Fall through in case the type is an IRI.

      case '<':
        // Try to find a full IRI without escape sequences.
        if (match = this._unescapedIri.exec(input)) {
          type = 'IRI';
          value = match[1];
        }
        // Try to find a full IRI with escape sequences.
        else if (match = this._iri.exec(input)) {
          unescaped = this._unescape(match[1]);
          if (unescaped === null || illegalIriChars.test(unescaped))
            return reportSyntaxError(this);
          type = 'IRI';
          value = unescaped;
        }
        break;

      case '"':
      case "'":
        // Try to find a non-empty double-quoted literal without escape sequences.
        if (match = this._unescapedString.exec(input)) {
          type = 'literal';
          value = match[0];
        }
        // Try to find any other literal wrapped in a pair of single or double quotes.
        else if (match = this._singleQuotedString.exec(input)) {
          unescaped = this._unescape(match[0]);
          if (unescaped === null)
            return reportSyntaxError(this);
          type = 'literal';
          value = unescaped.replace(/^'|'$/g, '"');
        }
        // Try to find a literal wrapped in three pairs of single or double quotes.
        else if (match = this._tripleQuotedString.exec(input)) {
          unescaped = match[1] || match[2];
          // Count the newlines and advance line counter.
          this._line += unescaped.split(/\r\n|\r|\n/).length - 1;
          unescaped = this._unescape(unescaped);
          if (unescaped === null)
            return reportSyntaxError(this);
          type = 'literal';
          value = unescaped.replace(/^'|'$/g, '"');
        }
        break;

      case '@':
        // Try to find a language code.
        if (this._prevTokenType === 'literal' && (match = this._langcode.exec(input))) {
          type = 'langcode';
          value = match[1];
        }
        // Try to find a keyword.
        else if (match = this._keyword.exec(input)) {
          type = match[0];
        }
        break;

      case '.':
        // Try to find a dot as punctuation.
        if (input.length === 1 ? inputFinished : (input[1] < '0' || input[1] > '9')) {
          type = '.';
          matchLength = 1;
          break;
        }
        // Fall through to numerical case (could be a decimal dot).

      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
      case '+':
      case '-':
        // Try to find a number.
        if (match = this._number.exec(input)) {
          type = 'literal';
          value = '"' + match[0] + '"^^http://www.w3.org/2001/XMLSchema#' +
                  (match[1] ? 'double' : (/^[+\-]?\d+$/.test(match[0]) ? 'integer' : 'decimal'));
        }
        break;

      case 'B':
      case 'b':
      case 'p':
      case 'P':
        // Try to find a SPARQL-style keyword.
        if (match = this._sparqlKeyword.exec(input))
          type = match[0].toUpperCase();
        else
          inconclusive = true;
        break;

      case 'f':
      case 't':
        // Try to match a boolean.
        if (match = this._boolean.exec(input)) {
          type = 'literal';
          value = '"' + match[0] + '"^^http://www.w3.org/2001/XMLSchema#boolean';
        }
        else
          inconclusive = true;
        break;

      case 'a':
        // Try to find an abbreviated predicate.
        if (match = this._shortPredicates.exec(input)) {
          type = 'abbreviation';
          value = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
        }
        else
          inconclusive = true;
        break;

      case ',':
      case ';':
      case '[':
      case ']':
      case '(':
      case ')':
        // The next token is punctuation
        matchLength = 1;
        type = firstChar;
        break;

      default:
        inconclusive = true;
      }

      // Some first characters do not allow an immediate decision, so inspect more.
      if (inconclusive) {
        // Try to find a prefix.
        if ((this._prevTokenType === '@prefix' || this._prevTokenType === 'PREFIX') &&
            (match = this._prefix.exec(input))) {
          type = 'prefix';
          value = match[1] || '';
        }
        // Try to find a prefixed name. Since it can contain (but not end with) a dot,
        // we always need a non-dot character before deciding it is a prefixed name.
        // Therefore, try inserting a space if we're at the end of the input.
        else if ((match = this._prefixedName.exec(input)) ||
                 inputFinished && (match = this._prefixedName.exec(input + ' '))) {
          type = 'prefixed';
          prefix = match[1] || '';
          value = this._unescape(match[2]);
        }
      }

      // A type token is special: it can only be emitted after an IRI or prefixed name is read.
      if (this._prevTokenType === '^')
        type = (type === 'IRI' || type === 'prefixed') ? 'type' : '';

      // What if nothing of the above was found?
      if (!type) {
        // We could be in streaming mode, and then we just wait for more input to arrive.
        // Otherwise, a syntax error has occurred in the input.
        // One exception: error on an unaccounted linebreak (= not inside a triple-quoted literal).
        if (inputFinished || (!/^'''|^"""/.test(input) && /\n|\r/.test(input)))
          return reportSyntaxError(this);
        else
          return this._input = input;
      }

      // Emit the parsed token.
      callback(null, { line: line, type: type, value: value, prefix: prefix });
      this._prevTokenType = type;

      // Advance to next part to tokenize.
      input = input.substr(matchLength || match[0].length, input.length);
    }

    // Signals the syntax error through the callback
    function reportSyntaxError(self) {
      self._input = null;
      match = /^\S*/.exec(input);
      callback(new Error('Syntax error: unexpected "' + match[0] + '" on line ' + self._line + '.'));
    }
  },

  // ### `unescape` replaces N3 escape codes by their corresponding characters.
  _unescape: function (item) {
    try {
      return item.replace(escapeSequence, function (sequence, unicode4, unicode8, escapedChar) {
        var charCode;
        if (unicode4) {
          charCode = parseInt(unicode4, 16);
          if (isNaN(charCode)) throw new Error(); // can never happen (regex), but helps performance
          return fromCharCode(charCode);
        }
        else if (unicode8) {
          charCode = parseInt(unicode8, 16);
          if (isNaN(charCode)) throw new Error(); // can never happen (regex), but helps performance
          if (charCode < 0xFFFF) return fromCharCode(charCode);
          return fromCharCode(0xD800 + ((charCode -= 0x10000) >> 10), 0xDC00 + (charCode & 0x3FF));
        }
        else {
          var replacement = escapeReplacements[escapedChar];
          if (!replacement)
            throw new Error();
          return replacement;
        }
      });
    }
    catch (error) { return null; }
  },


  // ## Public methods

  // ### `tokenize` starts the transformation of an N3 document into an array of tokens.
  // The input can be a string or a stream.
  tokenize: function (input, callback) {
    var self = this;
    this._line = 1;

    // If the input is a string, continuously emit tokens through the callback until the end.
    if (typeof input === 'string') {
      this._input = input;
      immediately(function () { self._tokenizeToEnd(callback, true); });
    }
    // Otherwise, the input will be streamed.
    else {
      this._input = '';

      // If no input was given, it will be streamed through `addChunk` and ended with `end`
      if (!input || typeof input === 'function') {
        this.addChunk = addChunk;
        this.end = end;
        if (!callback)
          callback = input;
      }
      // Otherwise, the input itself must be a stream
      else {
        if (typeof input.setEncoding === 'function')
          input.setEncoding('utf8');
        input.on('data', addChunk);
        input.on('end', end);
      }
    }

    // Adds the data chunk to the buffer and parses as far as possible
    function addChunk(data) {
      if (self._input !== null) {
        self._input += data;
        self._tokenizeToEnd(callback, false);
      }
    }

    // Parses until the end
    function end() {
      if (self._input !== null) {
        self._tokenizeToEnd(callback, true);
      }
    }
  },
};

// ## Exports

// Export the `N3Lexer` class as a whole.

N3.Lexer = N3Lexer;
// **N3Parser** parses N3 documents.
var N3Lexer = N3Lexer;

var RDF_PREFIX = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    RDF_NIL    = RDF_PREFIX + 'nil',
    RDF_FIRST  = RDF_PREFIX + 'first',
    RDF_REST   = RDF_PREFIX + 'rest';

var absoluteURI = /:/,
    documentPart = /[^\/]*$/,
    rootURI = /^(?:[^:]+:\/*)?[^\/]*/;

// The next ID for new blank nodes
var blankNodeCount = 0;

// ## Constructor
function N3Parser(config) {
  if (!(this instanceof N3Parser))
    return new N3Parser(config);

  // Initialize the parser settings
  config = config || {};
  this._tripleStack = [];
  this._lexer = config.lexer || new N3Lexer();
  this._blankNodes = Object.create(null);

  // Set the document URI
  if (!config.documentURI) {
    this._baseURI = null;
    this._baseURIPath = null;
  }
  else {
    if (config.documentURI.indexOf('#') > 0)
      throw new Error('Invalid document URI');
    this._baseURI = config.documentURI;
    this._baseURIPath = this._baseURI.replace(documentPart, '');
    this._baseURIRoot = this._baseURI.match(rootURI)[0];
  }
}

// ## Private class methods

// ### `_resetBlankNodeIds` restarts blank node identification.
N3Parser._resetBlankNodeIds = function () {
  blankNodeCount = 0;
};

N3Parser.prototype = {
  // ## Private methods

  // ### `_readInTopContext` reads a token when in the top context.
  _readInTopContext: function (token) {
    switch (token.type) {
    // If an EOF token arrives in the top context, signal that we're done.
    case 'eof':
      return this._callback(null, null, this._prefixes);
    // It could be a prefix declaration.
    case '@prefix':
      this._sparqlStyle = false;
      return this._readPrefix;
    case 'PREFIX':
      this._sparqlStyle = true;
      return this._readPrefix;
    // It could be a base declaration.
    case '@base':
      this._sparqlStyle = false;
      return this._readBaseURI;
    case 'BASE':
      this._sparqlStyle = true;
      return this._readBaseURI;
    // Otherwise, the next token must be a subject.
    default:
      return this._readSubject(token);
    }
  },

  // ### `_readSubject` reads a triple's subject.
  _readSubject: function (token) {
    switch (token.type) {
    case 'IRI':
      if (this._baseURI === null || absoluteURI.test(token.value))
        this._subject = token.value;
      else
        this._subject = this._resolveURI(token.value);
      break;
    case 'prefixed':
      if (token.prefix === '_') {
        this._subject = this._blankNodes[token.value] ||
                        (this._blankNodes[token.value] = '_:b' + blankNodeCount++);
      }
      else {
        var prefix = this._prefixes[token.prefix];
        if (prefix === undefined)
          return this._error('Undefined prefix "' + token.prefix + ':"', token);
        this._subject = prefix + token.value;
      }
      break;
    case '[':
      // Start a new triple with a new blank node as subject.
      this._subject = '_:b' + blankNodeCount++;
      this._tripleStack.push({ subject: this._subject, predicate: null, object: null, type: 'blank' });
      return this._readBlankNodeHead;
    case '(':
      // Start a new list
      this._tripleStack.push({ subject: RDF_NIL, predicate: null, object: null, type: 'list' });
      this._subject = null;
      return this._readListItem;
    default:
      return this._error('Expected subject but got ' + token.type, token);
    }
    this._subjectHasPredicate = false;
    // The next token must be a predicate.
    return this._readPredicate;
  },

  // ### `_readPredicate` reads a triple's predicate.
  _readPredicate: function (token) {
    switch (token.type) {
    case 'IRI':
    case 'abbreviation':
      if (this._baseURI === null || absoluteURI.test(token.value))
        this._predicate = token.value;
      else
        this._predicate = this._resolveURI(token.value);
      break;
    case 'prefixed':
      if (token.prefix === '_') {
        return this._error('Disallowed blank node as predicate', token);
      }
      else {
        var prefix = this._prefixes[token.prefix];
        if (prefix === undefined)
          return this._error('Undefined prefix "' + token.prefix + ':"', token);
        this._predicate = prefix + token.value;
      }
      break;
    case ']':
      // Expected predicate didn't come, must have been trailing semicolon.
      return this._readBlankNodeTail(token, true);
    case '.':
      // A dot is not allowed if the subject did not have a predicate yet
      if (!this._subjectHasPredicate)
        return this._error('Unexpected dot', token);
      // Expected predicate didn't come, must have been trailing semicolon.
      return this._readPunctuation(token, true);
    case ';':
      // Extra semicolons can be safely ignored
      return this._readPredicate;
    default:
      return this._error('Expected predicate to follow "' + this._subject + '"', token);
    }
    this._subjectHasPredicate = true;
    // The next token must be an object.
    return this._readObject;
  },

  // ### `_readObject` reads a triple's object.
  _readObject: function (token) {
    switch (token.type) {
    case 'IRI':
      if (this._baseURI === null || absoluteURI.test(token.value))
        this._object = token.value;
      else
        this._object = this._resolveURI(token.value);
      break;
    case 'prefixed':
      if (token.prefix === '_') {
        this._object = this._blankNodes[token.value] ||
                       (this._blankNodes[token.value] = '_:b' + blankNodeCount++);
      }
      else {
        var prefix = this._prefixes[token.prefix];
        if (prefix === undefined)
          return this._error('Undefined prefix "' + token.prefix + ':"', token);
        this._object = prefix + token.value;
      }
      break;
    case 'literal':
      this._object = token.value;
      return this._readDataTypeOrLang;
    case '[':
      // Start a new triple with a new blank node as subject.
      var blank = '_:b' + blankNodeCount++;
      this._tripleStack.push({ subject: this._subject, predicate: this._predicate, object: blank, type: 'blank' });
      this._subject = blank;
      return this._readBlankNodeHead;
    case '(':
      // Start a new list
      this._tripleStack.push({ subject: this._subject, predicate: this._predicate, object: RDF_NIL, type: 'list' });
      this._subject = null;
      return this._readListItem;
    default:
      return this._error('Expected object to follow "' + this._predicate + '"', token);
    }
    return this._getTripleEndReader();
  },

  // ### `_readBlankNodeHead` reads the head of a blank node.
  _readBlankNodeHead: function (token) {
    if (token.type === ']')
      return this._readBlankNodeTail(token, true);
    else
      return this._readPredicate(token);
  },

  // ### `_readBlankNodeTail` reads the end of a blank node.
  _readBlankNodeTail: function (token, empty) {
    if (token.type !== ']')
      return this._readPunctuation(token);

    // Store blank node triple.
    if (empty !== true)
      this._callback(null, { subject: this._subject,
                             predicate: this._predicate,
                             object: this._object,
                             context: 'n3/contexts#default' });

    // Restore parent triple that contains the blank node.
    var triple = this._tripleStack.pop();
    this._subject = triple.subject;
    // Was the blank node the object?
    if (triple.object !== null) {
      // Restore predicate and object as well, and continue by reading punctuation.
      this._predicate = triple.predicate;
      this._object = triple.object;
      return this._getTripleEndReader();
    }
    // The blank node was the subject, so continue reading the predicate.
    return this._readPredicate;
  },

  // ### `_readDataTypeOrLang` reads an _optional_ data type or language.
  _readDataTypeOrLang: function (token) {
    switch (token.type) {
    case 'type':
      var value;
      if (token.prefix === '') {
        value = token.value;
      }
      else {
        var prefix = this._prefixes[token.prefix];
        if (prefix === undefined)
          return this._error('Undefined prefix "' + token.prefix + ':"', token);
        value = prefix + token.value;
      }
      this._object += '^^' + value;
      return this._getTripleEndReader();
    case 'langcode':
      this._object += '@' + token.value.toLowerCase();
      return this._getTripleEndReader();
    default:
      return this._getTripleEndReader().call(this, token);
    }
  },

  // ### `_readListItem` reads items from a list.
  _readListItem: function (token) {
    var item = null,                  // The actual list item.
        itemHead = null,              // The head of the rdf:first predicate.
        prevItemHead = this._subject, // The head of the previous rdf:first predicate.
        stack = this._tripleStack,    // The stack of triples part of recursion (lists, blanks, etc.).
        parentTriple = stack[stack.length - 1], // The triple containing the current list.
        next = this._readListItem;    // The next function to execute.

    switch (token.type) {
    case 'IRI':
      item = token.value;
      break;
    case 'prefixed':
      if (token.prefix === '_') {
        item = this._blankNodes[token.value] ||
                       (this._blankNodes[token.value] = '_:b' + blankNodeCount++);
      }
      else {
        var prefix = this._prefixes[token.prefix];
        if (prefix === undefined)
          return this._error('Undefined prefix "' + token.prefix + ':"', token);
        item = prefix + token.value;
      }
      break;
    case 'literal':
      item = token.value;
      next = this._readDataTypeOrLang;
      break;
    case '[':
      // Stack the current list triple and start a new triple with a blank node as subject.
      itemHead = '_:b' + blankNodeCount++;
      item     = '_:b' + blankNodeCount++;
      stack.push({ subject: itemHead, predicate: RDF_FIRST, object: item, type: 'blank' });
      this._subject = item;
      next = this._readBlankNodeHead;
      break;
    case '(':
      // Stack the current list triple and start a new list
      itemHead = '_:b' + blankNodeCount++;
      stack.push({ subject: itemHead, predicate: RDF_FIRST, object: RDF_NIL, type: 'list' });
      this._subject = null;
      next = this._readListItem;
      break;
    case ')':
      // Restore the parent triple.
      stack.pop();
      // If this list is contained within a parent list, return the membership triple here.
      // This will be `<parent list elemen>t rdf:first <this list>.`.
      if (stack.length !== 0 && stack[stack.length - 1].type === 'list')
        this._callback(null, { subject: parentTriple.subject,
                               predicate: parentTriple.predicate,
                               object: parentTriple.object,
                               context: 'n3/contexts#default' });
      // Restore the parent triple's subject.
      this._subject = parentTriple.subject;
      // Was this list in the parent triple's subject?
      if (parentTriple.predicate === null) {
        // The next token is the predicate.
        next = this._readPredicate;
        // Skip writing the list tail if this was an empty list.
        if (parentTriple.subject === RDF_NIL)
          return next;
      }
      // The list was in the parent triple's object.
      else {
        // Restore the parent triple's predicate and object as well.
        this._predicate = parentTriple.predicate;
        this._object = parentTriple.object;
        next = this._getTripleEndReader();
        // Skip writing the list tail if this was an empty list.
        if (parentTriple.object === RDF_NIL)
          return next;
      }
      // Close the list by making the item head nil.
      itemHead = RDF_NIL;
      break;
    default:
      return this._error('Expected list item instead of "' + token.type + '"', token);
    }

     // Create a new blank node if no item head was assigned yet.
    if (itemHead === null)
      this._subject = itemHead = '_:b' + blankNodeCount++;

    // Is this the first element of the list?
    if (prevItemHead === null) {
      // This list is either the object or the subject.
      if (parentTriple.object === RDF_NIL)
        parentTriple.object = itemHead;
      else
        parentTriple.subject = itemHead;
    }
    else {
      // The rest of the list is in the current head.
      this._callback(null, { subject: prevItemHead,
                             predicate: RDF_REST,
                             object: itemHead,
                             context: 'n3/contexts#default' });
    }
    // Add the item's value.
    if (item !== null)
      this._callback(null, { subject: itemHead,
                             predicate: RDF_FIRST,
                             object: item,
                             context: 'n3/contexts#default' });
    return next;
  },

  // ### `_readPunctuation` reads punctuation between triples or triple parts.
  _readPunctuation: function (token, empty) {
    var next;
    switch (token.type) {
    // A dot just ends the statement, without sharing anything with the next.
    case '.':
      next = this._readInTopContext;
      break;
    // Semicolon means the subject is shared; predicate and object are different.
    case ';':
      next = this._readPredicate;
      break;
    // Comma means both the subject and predicate are shared; the object is different.
    case ',':
      next = this._readObject;
      break;
    default:
      return this._error('Expected punctuation to follow "' + this._object + '"', token);
    }
    // A triple has been completed now, so return it.
    if (!empty)
      this._callback(null, { subject: this._subject,
                             predicate: this._predicate,
                             object: this._object,
                             context: 'n3/contexts#default' });
    return next;
  },

  // ### `_readPrefix` reads the prefix of a prefix declaration.
  _readPrefix: function (token) {
    if (token.type !== 'prefix')
      return this._error('Expected prefix to follow @prefix', token);
    this._prefix = token.value;
    return this._readPrefixURI;
  },

  // ### `_readPrefixURI` reads the URI of a prefix declaration.
  _readPrefixURI: function (token) {
    if (token.type !== 'IRI')
      return this._error('Expected IRI to follow prefix "' + this._prefix + ':"', token);
    var prefixURI;
    if (this._baseURI === null || absoluteURI.test(token.value))
      prefixURI = token.value;
    else
      prefixURI = this._resolveURI(token.value);
    this._prefixes[this._prefix] = prefixURI;
    this._prefixCallback(this._prefix, prefixURI);
    return this._readDeclarationPunctuation;
  },

  // ### `_readBaseURI` reads the URI of a base declaration.
  _readBaseURI: function (token) {
    if (token.type !== 'IRI')
      return this._error('Expected IRI to follow base declaration', token);
    if (token.value.indexOf('#') > 0)
      return this._error('Invalid base URI', token);
    if (this._baseURI === null || absoluteURI.test(token.value))
      this._baseURI = token.value;
    else
      this._baseURI = this._resolveURI(token.value);
    this._baseURIPath = this._baseURI.replace(documentPart, '');
    this._baseURIRoot = this._baseURI.match(rootURI)[0];
    return this._readDeclarationPunctuation;
  },

  // ### `_readDeclarationPunctuation` reads the punctuation of a declaration.
  _readDeclarationPunctuation: function (token) {
    // SPARQL-style declarations don't have punctuation.
    if (this._sparqlStyle)
      return this._readInTopContext(token);

    if (token.type !== '.')
      return this._error('Expected declaration to end with a dot', token);
    return this._readInTopContext;
  },

  // ### `_getTripleEndReader` gets the next reader function at the end of a triple.
  _getTripleEndReader: function () {
    var stack = this._tripleStack;
    if (stack.length === 0)
      return this._readPunctuation;

    switch (stack[stack.length - 1].type) {
    case 'blank':
      return this._readBlankNodeTail;
    case 'list':
      return this._readListItem;
    }
  },

  // ### `_error` emits an error message through the callback.
  _error: function (message, token) {
    this._callback(new Error(message + ' at line ' + token.line + '.'));
  },

  // ### `_resolveURI` resolves a URI against a base path
  _resolveURI: function (uri) {
    switch (uri[0]) {
    // An empty relative URI indicates the base URI
    case undefined:
      return this._baseURI;
    // Resolve relative fragment URIs against the base URI
    case '#':
      return this._baseURI     + uri;
    // Resolve relative query string URIs by replacing the query string
    case '?':
      return this._baseURI.replace(/(?:\?.*)?$/, uri);
    // Resolve root relative URIs at the root of the base URI
    case '/':
      return this._baseURIRoot + uri;
    // Resolve all other URIs at the base URI's path
    default:
      return this._baseURIPath + uri;
    }
  },

  // ## Public methods

  // ### `parse` parses the N3 input and emits each parsed triple through the callback.
  parse: function (input, tripleCallback, prefixCallback) {
    // The read callback is the next function to be executed when a token arrives.
    // We start reading in the top context.
    this._readCallback = this._readInTopContext;
    this._prefixes = {};

    // If the input argument is not given, shift parameters
    if (typeof input === 'function')
      prefixCallback = tripleCallback, tripleCallback = input, input = null;

    // Set the triple and prefix callbacks.
    this._callback = tripleCallback || noop;
    this._prefixCallback = prefixCallback || noop;

    // Execute the read callback when a token arrives.
    var self = this;
    this._lexer.tokenize(input, function (error, token) {
      if (self._readCallback !== undefined) {
        if (error !== null)
          self._callback(error);
        else
          self._readCallback = self._readCallback(token);
      }
    });

    // If no input was given, it can be added with `addChunk` and ended with `end`
    if (!input) {
      this.addChunk = this._lexer.addChunk;
      this.end = this._lexer.end;
    }
  }
};

// The empty function
function noop() {}

// ## Exports

// Export the `N3Parser` class as a whole.

N3.Parser = N3Parser;
// **N3Writer** writes N3 documents.

// Matches a literal as represented in memory by the N3 library
var N3LiteralMatcher = /^"([^]*)"(?:\^\^(.+)|@([\-a-z]+))?$/i;

// rdf:type predicate (for 'a' abbreviation)
var RDF_PREFIX = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    RDF_TYPE   = RDF_PREFIX + 'type';

// Characters in literals that require escaping
var literalEscape    = /["\\\t\n\r\b\f]/,
    literalEscapeAll = /["\\\t\n\r\b\f]/g,
    literalReplacements = { '\\': '\\\\', '"': '\\"', '\t': '\\t',
                            '\n': '\\n', '\r': '\\r', '\b': '\\b', '\f': '\\f' };

// ## Constructor
function N3Writer(outputStream, prefixes) {
  if (!(this instanceof N3Writer))
    return new N3Writer(outputStream, prefixes);

  // Shift arguments if the first argument is not a stream
  if (outputStream && typeof outputStream.write !== 'function')
    prefixes = outputStream, outputStream = null;

  // If no output stream given, send the output as string through the end callback
  if (!outputStream) {
    outputStream = this;
    this._output = '';
    this.write = function (chunk, encoding, callback) {
      this._output += chunk;
      callback && callback();
    };
  }

  this._outputStream = outputStream;
  this._prefixURIs = Object.create(null);
  if (prefixes)
    this.addPrefixes(prefixes);
}

N3Writer.prototype = {
  // ## Private methods

  // ### `_write` writes the arguments to the output stream (last argument is callback)
  _write: function () {
    for (var i = 0, l = arguments.length - 2; i <= l; i++)
      this._outputStream.write(arguments[i], 'utf8', i === l ? arguments[l + 1] : null);
  },

  // ### `_writeUriOrBlankNode` writes a URI or blank node to the output stream
  _writeUriOrBlankNode: function (uri, done) {
    // Write blank node
    if (uri[0] === '_' && uri[1] === ':')
      return this._write(uri, done);

    // Write prefixed name if possible
    var prefixMatch = this._prefixRegex.exec(uri);
    if (prefixMatch)
      this._write(this._prefixURIs[prefixMatch[1]], prefixMatch[2], done);
    // Otherwise, just write the URI
    else
      this._write('<', uri, '>', done);
  },

  // ### `_writeLiteral` writes a literal to the output stream
  _writeLiteral: function (value, type, language, done) {
    // Escape the literal if necessary
    if (literalEscape.test(value)) {
      value = value.replace(literalEscapeAll, function (match) {
        return literalReplacements[match];
      });
    }

    // Write the literal, possibly with type or language
    this._write('"', value, '"', type || language ? null : done);
    if (type) {
      this._write('^^', null);
      this._writeUriOrBlankNode(type, done);
    }
    else if (language) {
      this._write('@', language, done);
    }
  },

  // ### `_writeSubject` writes a subject to the output stream
  _writeSubject: function (subject, done) {
    if (subject[0] === '"')
      throw new Error('A literal as subject is not allowed: ' + subject);
    this._writeUriOrBlankNode(subject, done);
  },

  // ### `_writePredicate` writes a predicate to the output stream
  _writePredicate: function (predicate, done) {
    if (predicate[0] === '"')
      throw new Error('A literal as predicate is not allowed: ' + predicate);
    if (predicate === RDF_TYPE)
      this._write('a', done);
    else
      this._writeUriOrBlankNode(predicate, done);
  },

  // ### `_writeObject` writes an object to the output stream
  _writeObject: function (object, done) {
    var literalMatch = N3LiteralMatcher.exec(object);
    if (literalMatch !== null)
      this._writeLiteral(literalMatch[1], literalMatch[2], literalMatch[3], done);
    else
      this._writeUriOrBlankNode(object, done);
  },

  // ### `_blockedWrite` replaces `_writer_ after the writer has been closed
  _blockedWrite: function () {
    throw new Error('Cannot write because the writer has been closed.');
  },

  // ### `addTriple` adds the triple to the output stream
  addTriple: function (subject, predicate, object, done) {
    // If the triple was given as a triple object, shift parameters
    if (!object) {
      done = predicate;
      object = subject.object;
      predicate = subject.predicate;
      subject = subject.subject;
    }
    // Write the triple
    try {
      // Don't repeat the subject if it's the same
      if (this._prevSubject === subject) {
        // Don't repeat the predicate if it's the same
        if (this._prevPredicate === predicate) {
          this._write(', ', null);
        }
        // Same subject, different predicate
        else {
          this._write(';\n    ', null);
          this._writePredicate(predicate);
          this._write(' ', null);

          this._prevPredicate = predicate;
        }
      }
      // Different subject; write the whole triple
      else {
        // End a possible previous triple
        if ('_prevSubject' in this)
          this._write('.\n', null);
        this._writeSubject(subject);
        this._write(' ', null);
        this._writePredicate(predicate);
        this._write(' ', null);

        this._prevSubject = subject;
        this._prevPredicate = predicate;
      }
      // In all cases, write the object
      this._writeObject(object, done);
    }
    catch (error) {
      done && done(error);
    }
  },

  // ### `addTriples` adds the triples to the output stream
  addTriples: function (triples) {
    for (var i = 0; i < triples.length; i++)
      this.addTriple(triples[i]);
  },

  // ### `addPrefix` adds the prefix to the output stream
  addPrefix: function (prefix, uri, done) {
    var prefixes = {};
    prefixes[prefix] = uri;
    this.addPrefixes(prefixes, done);
  },

  // ### `addPrefixes` adds the prefixes to the output stream
  addPrefixes: function (prefixes, done) {
    // Add all useful prefixes
    var hasPrefixes = false;
    for (var prefix in prefixes) {
      // Verify whether the prefix can be used and does not exist yet
      var uri = prefixes[prefix];
      if (/[#\/]$/.test(uri) && this._prefixURIs[uri] !== (prefix += ':')) {
        hasPrefixes = true;
        this._prefixURIs[uri] = prefix;
        // Finish a possible pending triple
        if ('_prevSubject' in this) {
          this._write('.\n', null);
          delete this._prevSubject;
        }
        // Write prefix
        this._write('@prefix ', prefix, ' <', uri, '>.\n', null);
      }
    }
    // Recreate the prefix matcher
    if (hasPrefixes) {
      var prefixURIs = '';
      for (var prefixURI in this._prefixURIs)
        prefixURIs += prefixURIs ? '|' + prefixURI : prefixURI;
      prefixURIs = prefixURIs.replace(/[\]\/\(\)\*\+\?\.\\\$]/g, '\\$&');
      this._prefixRegex = new RegExp('^(' + prefixURIs + ')([a-zA-Z][\\-_a-zA-Z0-9]*)$');
    }
    // End a prefix block with a newline
    this._write(hasPrefixes ? '\n' : '', done);
  },

  // ### `_prefixRegex` matches an URL that begins with one of the added prefixes
  _prefixRegex: { exec: function () { return null; } },

  // ### `end` signals the end of the output stream
  end: function (done) {
    // Finish a possible pending triple
    if ('_prevSubject' in this) {
      this._write('.\n', null);
      delete this._prevSubject;
    }
    // Disallow further writing
    this._write = this._blockedWrite;

    // If writing to a string instead of an actual stream, send the string
    if (this === this._outputStream)
      return done && done(null, this._output);

    // Try to end the underlying stream, ensuring done is called exactly one time
    var singleDone = done && function () { singleDone = null, done(); };
    // Ending a stream can error
    try { this._outputStream.end(singleDone); }
    // Execute the callback if it hasn't been executed
    catch (error) { singleDone && singleDone(); }
  },
};

// ## Exports

// Export the `N3Writer` class as a whole.

N3.Writer = N3Writer;
// **N3Store** objects store N3 triples with an associated context in memory.

var expandPrefixedName = N3Util.expandPrefixedName;

// ## Constructor
function N3Store(triples, prefixes) {
  if (!(this instanceof N3Store))
    return new N3Store(triples, prefixes);

  // The number of triples is initially zero.
  this._size = 0;
  // `_contexts` contains subject, predicate, and object indexes per context.
  this._contexts = Object.create(null);
  // `_entities` maps entities such as `http://xmlns.com/foaf/0.1/name` to numbers.
  // This saves memory, since only the numbers have to be stored in `_contexts`.
  this._entities = Object.create(null);
  this._entities['>>____unused_item_to_make_first_entity_key_non-falsy____<<'] = 0;
  this._entityCount = 0;
  // `_blankNodeIndex` is the index of the last created blank node that was automatically named
  this._blankNodeIndex = 0;

  // Shift parameters if `triples` is not given
  if (!prefixes && triples && !triples[0])
    prefixes = triples, triples = null;

  // Add triples and prefixes if passed
  this._prefixes = Object.create(null);
  triples && this.addTriples(triples);
  prefixes && this.addPrefixes(prefixes);
}

N3Store.prototype = {
  // ## Public properties

  // `defaultContext` is the default context wherein triples are stored.
  get defaultContext() {
    return 'n3/contexts#default';
  },

  // ### `size` returns the number of triples in the store.
  get size() {
    // Return the triple count if if was cached.
    var size = this._size;
    if (size !== null)
      return size;

    // Calculate the number of triples by counting to the deepest level.
    var contexts = this._contexts, subjects, subject;
    for (var contextKey in contexts)
      for (var subjectKey in (subjects = contexts[contextKey].subjects))
        for (var predicateKey in (subject = subjects[subjectKey]))
          size += Object.keys(subject[predicateKey]).length;
    return this._size = size;
  },

  // ## Private methods

  // ### `_addToIndex` adds a triple to a three-layered index.
  _addToIndex: function (index0, key0, key1, key2) {
    // Create layers as necessary.
    var index1 = index0[key0] || (index0[key0] = {});
    var index2 = index1[key1] || (index1[key1] = {});
    // Setting the key to _any_ value signalizes the presence of the triple.
    index2[key2] = null;
  },

  // ### `_removeFromIndex` removes a triple from a three-layered index.
  _removeFromIndex: function (index0, key0, key1, key2) {
    // Remove the triple from the index.
    var index1 = index0[key0], index2 = index1[key1], key;
    delete index2[key2];

    // Remove intermediary index layers if they are empty.
    for (key in index2) return;
    delete index1[key1];
    for (key in index1) return;
    delete index0[key0];
  },

  // ### `_findInIndex` finds a set of triples in a three-layered index.
  // The index base is `index0` and the keys at each level are `key0`, `key1`, and `key2`.
  // Any of these keys can be `null`, which is interpreted as a wildcard.
  // `name0`, `name1`, and `name2` are the names of the keys at each level,
  // used when reconstructing the resulting triple
  // (for instance: _subject_, _predicate_, and _object_).
  // Finally, `context` will be the context of the created triples.
  _findInIndex: function (index0, key0, key1, key2, name0, name1, name2, context) {
    var results = [], entityKeys = Object.keys(this._entities), tmp, index1, index2;

    // If a key is specified, use only that part of index 0.
    if (key0) (tmp = index0, index0 = {})[key0] = tmp[key0];
    for (var value0 in index0) {
      var entity0 = entityKeys[value0];

      if (index1 = index0[value0]) {
        // If a key is specified, use only that part of index 1.
        if (key1) (tmp = index1, index1 = {})[key1] = tmp[key1];
        for (var value1 in index1) {
          var entity1 = entityKeys[value1];

          if (index2 = index1[value1]) {
            // If a key is specified, use only that part of index 2, if it exists.
            var values = key2 ? (key2 in index2 ? [key2] : []) : Object.keys(index2);
            // Create triples for all items found in index 2.
            for (var l = values.length - 1; l >= 0; l--) {
              var result = { context: context };
              result[name0] = entity0;
              result[name1] = entity1;
              result[name2] = entityKeys[values[l]];
              results.push(result);
            }
          }
        }
      }
    }
    return results;
  },

  // ### `_countInIndex` counts matching triples in a three-layered index.
  // The index base is `index0` and the keys at each level are `key0`, `key1`, and `key2`.
  // Any of these keys can be `null`, which is interpreted as a wildcard.
  _countInIndex: function (index0, key0, key1, key2) {
    var count = 0, tmp, index1, index2;

    // If a key is specified, count only that part of index 0.
    if (key0) (tmp = index0, index0 = {})[key0] = tmp[key0];
    for (var value0 in index0) {
      if (index1 = index0[value0]) {

        // If a key is specified, count only that part of index 1.
        if (key1) (tmp = index1, index1 = {})[key1] = tmp[key1];
        for (var value1 in index1) {
          if (index2 = index1[value1]) {
            // If a key is specified, count the triple if it exists.
            if (key2) (key2 in index2) && count++;
            // Otherwise, count all triples.
            else count += Object.keys(index2).length;
          }
        }
      }
    }
    return count;
  },

  // ## Public methods

  // ### `addTriple` adds a new N3 triple to the store.
  addTriple: function (subject, predicate, object, context) {
    // Shift arguments if a triple object is given instead of components
    if (!predicate)
      context = subject.context, object = subject.object,
        predicate = subject.predicate, subject = subject.subject;

    // Find the context that will contain the triple.
    context = context || this.defaultContext;
    var contextItem = this._contexts[context];
    // Create the context if it doesn't exist yet.
    if (!contextItem) {
      contextItem = this._contexts[context] = {
        subjects: {},
        predicates: {},
        objects: {}
      };
      // Freezing a context helps subsequent `add` performance,
      // and properties will never be modified anyway.
      Object.freeze(contextItem);
    }

    // Since entities can often be long URIs, we avoid storing them in every index.
    // Instead, we have a separate index that maps entities to numbers,
    // which are then used as keys in the other indexes.
    var entities = this._entities;
    subject   = entities[subject]   || (entities[subject]   = ++this._entityCount);
    predicate = entities[predicate] || (entities[predicate] = ++this._entityCount);
    object    = entities[object]    || (entities[object]    = ++this._entityCount);

    this._addToIndex(contextItem.subjects,   subject,   predicate, object);
    this._addToIndex(contextItem.predicates, predicate, object,    subject);
    this._addToIndex(contextItem.objects,    object,    subject,   predicate);

    // The cached triple count is now invalid.
    this._size = null;
  },

  // ### `addTriples` adds multiple N3 triples to the store.
  addTriples: function (triples) {
    for (var i = triples.length - 1; i >= 0; i--)
      this.addTriple(triples[i]);
  },

  // ### `addPrefix` adds support for querying with the given prefix
  addPrefix: function (prefix, uri) {
    this._prefixes[prefix] = uri;
  },

  // ### `addPrefixex` adds support for querying with the given prefixes
  addPrefixes: function (prefixes) {
    for (var prefix in prefixes)
      this.addPrefix(prefix, prefixes[prefix]);
  },

  // ### `removeTriple` removes an N3 triple from the store if it exists.
  removeTriple: function (subject, predicate, object, context) {
    // Shift arguments if a triple object is given instead of components.
    if (!predicate)
      context = subject.context, object = subject.object,
        predicate = subject.predicate, subject = subject.subject;
    context = context || this.defaultContext;

    // Find internal identifiers for all components.
    var contextItem, entities = this._entities, contexts = this._contexts;
    if (!(subject     = entities[subject]))   return;
    if (!(predicate   = entities[predicate])) return;
    if (!(object      = entities[object]))    return;
    if (!(contextItem = contexts[context]))   return;

    // Verify that the triple exists.
    var subjects, predicates;
    if (!(subjects   = contextItem.subjects[subject])) return;
    if (!(predicates = subjects[predicate])) return;
    if (!(object in predicates)) return;

    // Remove it from all indexes.
    this._removeFromIndex(contextItem.subjects,   subject,   predicate, object);
    this._removeFromIndex(contextItem.predicates, predicate, object,    subject);
    this._removeFromIndex(contextItem.objects,    object,    subject,   predicate);
    if (this._size !== null) this._size--;

    // Remove the context if it is empty.
    for (subject in contextItem.subjects) return;
    delete contexts[context];
  },

  // ### `removeTriples` removes multiple N3 triples from the store.
  removeTriples: function (triples) {
    for (var i = triples.length - 1; i >= 0; i--)
      this.removeTriple(triples[i]);
  },

  // ### `find` finds a set of triples matching a pattern, expanding prefixes as necessary.
  // Setting `subject`, `predicate`, or `object` to `null` means an _anything_ wildcard.
  // Setting `context` to `null` means the default context.
  find: function (subject, predicate, object, context) {
    var prefixes = this._prefixes;
    return this.findByUri(
      expandPrefixedName(subject,   prefixes),
      expandPrefixedName(predicate, prefixes),
      expandPrefixedName(object,    prefixes),
      expandPrefixedName(context,   prefixes)
    );
  },

  // ### `findByUri` finds a set of triples matching a pattern.
  // Setting `subject`, `predicate`, or `object` to `null` means an _anything_ wildcard.
  // Setting `context` to `null` means the default context.
  findByUri: function (subject, predicate, object, context) {
    context = context || this.defaultContext;
    var contextItem = this._contexts[context], entities = this._entities;

    // If the specified context contain no triples, there are no results.
    if (!contextItem) return [];

    // Translate URIs to internal index keys.
    // Optimization: if the entity doesn't exist, no triples with it exist.
    if (subject   && !(subject   = entities[subject]))   return [];
    if (predicate && !(predicate = entities[predicate])) return [];
    if (object    && !(object    = entities[object]))    return [];

    // Choose the optimal index, based on what fields are present
    if (subject) {
      if (object)
        // If subject and object are given, the object index will be the fastest.
        return this._findInIndex(contextItem.objects, object, subject, predicate,
                                 'object', 'subject', 'predicate', context);
      else
        // If only subject and possibly predicate are given, the subject index will be the fastest.
        return this._findInIndex(contextItem.subjects, subject, predicate, null,
                                 'subject', 'predicate', 'object', context);
    }
    else if (predicate)
      // If only predicate and possibly object are given, the predicate index will be the fastest.
      return this._findInIndex(contextItem.predicates, predicate, object, null,
                               'predicate', 'object', 'subject', context);
    else if (object)
      // If only object is given, the object index will be the fastest.
      return this._findInIndex(contextItem.objects, object, null, null,
                               'object', 'subject', 'predicate', context);
    else
      // If nothing is given, iterate subjects and predicates first
      return this._findInIndex(contextItem.subjects, null, null, null,
                               'subject', 'predicate', 'object', context);
  },

  // ### `count` returns the number of triples matching a pattern, expanding prefixes as necessary.
  // Setting `subject`, `predicate`, or `object` to `null` means an _anything_ wildcard.
  // Setting `context` to `null` means the default context.
  count: function (subject, predicate, object, context) {
    var prefixes = this._prefixes;
    return this.countByUri(
      expandPrefixedName(subject,   prefixes),
      expandPrefixedName(predicate, prefixes),
      expandPrefixedName(object,    prefixes),
      expandPrefixedName(context,   prefixes)
    );
  },

  // ### `countByUri` returns the number of triples matching a pattern.
  // Setting `subject`, `predicate`, or `object` to `null` means an _anything_ wildcard.
  // Setting `context` to `null` means the default context.
  countByUri: function (subject, predicate, object, context) {
    context = context || this.defaultContext;
    var contextItem = this._contexts[context], entities = this._entities;

    // If the specified context contain no triples, there are no results.
    if (!contextItem) return 0;

    // Translate URIs to internal index keys.
    // Optimization: if the entity doesn't exist, no triples with it exist.
    if (subject   && !(subject   = entities[subject]))   return 0;
    if (predicate && !(predicate = entities[predicate])) return 0;
    if (object    && !(object    = entities[object]))    return 0;

    // Choose the optimal index, based on what fields are present
    if (subject) {
      if (object)
        // If subject and object are given, the object index will be the fastest.
        return this._countInIndex(contextItem.objects, object, subject, predicate);
      else
        // If only subject and possibly predicate are given, the subject index will be the fastest.
        return this._countInIndex(contextItem.subjects, subject, predicate, object);
    }
    else if (predicate) {
      // If only predicate and possibly object are given, the predicate index will be the fastest.
      return this._countInIndex(contextItem.predicates, predicate, object, subject);
    }
    else {
      // If only object is possibly given, the object index will be the fastest.
      return this._countInIndex(contextItem.objects, object, subject, predicate);
    }
  },

  // ### `createBlankNode` creates a new blank node, returning its name.
  createBlankNode: function (suggestedName) {
    var name;
    if (suggestedName) {
      name = suggestedName = '_:' + suggestedName;
      var index = 1;
      while (this._entities[name])
        name = suggestedName + index++;
    }
    else {
      do {
        name = '_:b' + this._blankNodeIndex++;
      }
      while (this._entities[name]);
    }
    this._entities[name] = this._entityCount++;
    return name;
  },
};

// ## Exports

// Export the `N3Store` class as a whole.

N3.Store = N3Store;
})(typeof exports !== "undefined" ? exports : this.N3 = {});
