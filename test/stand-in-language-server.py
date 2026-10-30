#!/usr/bin/env python3
# test/stand-in-language-server.py --- A language server for the tests.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""A language server for the tests: it speaks the Language Server Protocol on
its standard input and output, counts positions in octets (utf-8), and finds
the word "bad" an error and "meh" a warning wherever they stand. It completes
from the words of the text, finds a word's definition where "def WORD" stands,
its uses wherever it stands, renames it, describes and signs it, lays out the
text by putting single spaces for runs of them, offers to make "bad"
"good", hints ": int" after each name given a value, says a name after
"def" is a function and every other name a variable, finds the calls of a
function (each "NAME(" a call, from the function it stands in) and the
functions of the text by a part of their names; it lists a file's
functions with the names given values in them, finds a word's uses in the
file, and says it is indexing, 40% of the way, from its start until the
text first changes. A word's implementations are its "def"s, its type the
first "class" of the text, and its imports are put in order by sorting the
"import" lines at its top. It takes changes as ranges, and keeps how many
it was sent whole and in ranges as the lines "whole" and "ranges" of its log.
It asks to be told of its project's Python files changed on disk, and logs
each it is told of as "watched NAME TYPE". "ugh" it finds only when asked
(textDocument/diagnostic), as information. Among its completions is a call of
add as a snippet (addcall), its arguments fields. Each "def" is a block to fold and
has a code lens counting its calls (its title given only when resolved, its
command logged as "ran NAME"); the selection grows from the word to its line
to the whole text; and it finds "elsewhere found" in elsewhere.py, a file of
its project never opened. It writes what it was told, one method a line, to the file its first
argument names.

To try the editor's care: it asks a question no editor answers as it starts,
and logs the answer's error code ("answered unknown CODE"); a text holding
"editme" has it ask to change files in its project, outside it, on another
machine and not there, logging whether that was done ("applied true"); a
text holding "stall" has it stop reading what it is sent; asked to describe
"die" it ends without answering, and "huge" it describes in 200,000 words.

As a build run on each save does, it checks the file as saved for "oops", an
error: a second after each save it says what that check found, with what it
finds in the text it had when told of the save, under that text's number,
and logs "checked"; it says them again with what it finds after each change,
until the next check. What it says always carries the number of its text."""
import json, re, sys, threading, time
from urllib.parse import unquote, urlparse

LOG = open(sys.argv[1], 'a') if len(sys.argv) > 1 else None
TEXTS = {}
VERSIONS = {}
CHECKED = {}
WRITING = threading.Lock()

def read():
    length = None
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            break
        if line.lower().startswith(b'content-length:'):
            length = int(line.split(b':')[1])
    return json.loads(sys.stdin.buffer.read(length))

def send(message):
    body = json.dumps(message).encode()
    with WRITING:
        sys.stdout.buffer.write(b'Content-Length: %d\r\n\r\n' % len(body) + body)
        sys.stdout.buffer.flush()

def found_in(text, words):
    found = []
    for n, line in enumerate(text.encode().split(b'\n')):
        for word, severity in words:
            at = line.find(word)
            while at >= 0:
                found.append({'range': {'start': {'line': n, 'character': at}, 'end': {'line': n, 'character': at + len(word)}},
                              'severity': severity, 'source': 'stand-in', 'message': f'{word.decode()} found'})
                at = line.find(word, at + 1)
    return found

def diagnostics(uri):
    found = found_in(TEXTS[uri], ((b'bad', 1), (b'meh', 2))) + CHECKED.get(uri, [])
    send({'jsonrpc': '2.0', 'method': 'textDocument/publishDiagnostics',
          'params': {'uri': uri, 'version': VERSIONS.get(uri), 'diagnostics': found}})

def check_saved(uri):
    """What a check of the file as saved finds, said a second later of the text then."""
    try:
        saved = open(unquote(urlparse(uri).path), encoding='utf-8', errors='replace').read()
    except OSError:
        return
    text, version = TEXTS.get(uri, ''), VERSIONS.get(uri)
    def said():
        CHECKED[uri] = found_in(saved, ((b'oops', 1),))
        send({'jsonrpc': '2.0', 'method': 'textDocument/publishDiagnostics',
              'params': {'uri': uri, 'version': version, 'diagnostics': found_in(text, ((b'bad', 1), (b'meh', 2))) + CHECKED[uri]}})
        if LOG: LOG.write('checked\n'); LOG.flush()
    timer = threading.Timer(1.0, said)
    timer.daemon = True
    timer.start()

def lines(uri):
    return TEXTS[uri].encode().split(b'\n')

def word_at(params):
    line = lines(params['textDocument']['uri'])[params['position']['line']]
    at = params['position']['character']
    for m in re.finditer(rb'\w+', line):
        if m.start() <= at <= m.end():
            return m.group().decode()
    return None

def places(uri, word):
    found = []
    for n, line in enumerate(lines(uri)):
        for m in re.finditer(rb'\b' + re.escape(word.encode()) + rb'\b', line):
            found.append({'start': {'line': n, 'character': m.start()}, 'end': {'line': n, 'character': m.end()}})
    return found

def definition(params):
    uri, word = params['textDocument']['uri'], word_at(params)
    for n, line in enumerate(lines(uri)):
        m = re.search(rb'def (' + re.escape(word.encode()) + rb')\b', line) if word else None
        if m:
            return {'uri': uri, 'range': {'start': {'line': n, 'character': m.start(1)}, 'end': {'line': n, 'character': m.end(1)}}}
    return None

def completion(params):
    typed = word_at(params)
    words = sorted(set(re.findall(r'[A-Za-z_]\w+', TEXTS[params['textDocument']['uri']])) - {typed})
    # the documentation left for completionItem/resolve; a call of add as a snippet, its arguments fields
    items = [{'label': w, 'detail': 'a word of the text'} for w in words]
    items.append({'label': 'add(a, b)', 'filterText': 'addcall', 'detail': 'a call', 'insertTextFormat': 2,
                  'insertText': 'add(${1:a}, ${2:b})$0'})
    return items

def layout(params):
    text = TEXTS[params['textDocument']['uri']]
    new = re.sub(r'(?<=\S) {2,}', ' ', text)
    end = len(lines(params['textDocument']['uri'])) - 1
    return [] if new == text else [{'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': end + 1, 'character': 0}}, 'newText': new}]

def call_sites(uri, word):
    """Each place WORD is called in URI: (line, column, the function it stands in)."""
    found, inside = [], None
    for n, line in enumerate(lines(uri)):
        m = re.match(rb'def (\w+)', line)
        if m:
            inside = (n, m)
            continue
        for c in re.finditer(rb'\b' + re.escape(word.encode()) + rb'\(', line):
            found.append((n, c.start(), inside))
    return found

def item(uri, n, start, name):
    r = {'start': {'line': n, 'character': start}, 'end': {'line': n, 'character': start + len(name)}}
    return {'name': name, 'kind': 12, 'uri': uri, 'range': r, 'selectionRange': r}

def prepare_calls(params):
    word, uri = word_at(params), params['textDocument']['uri']
    d = definition(params)
    return [item(uri, d['range']['start']['line'], d['range']['start']['character'], word)] if d else None

def incoming(params):
    target = params['item']
    calls = []
    for n, at, inside in call_sites(target['uri'], target['name']):
        caller = item(target['uri'], inside[0], inside[1].start(1), inside[1].group(1).decode()) if inside else item(target['uri'], n, 0, 'top')
        calls.append({'from': caller, 'fromRanges': [{'start': {'line': n, 'character': at}, 'end': {'line': n, 'character': at + len(target['name'])}}]})
    return calls

def outgoing(params):
    target, uri = params['item'], params['item']['uri']
    body, called = False, []
    for n, line in enumerate(lines(uri)):
        if re.match(rb'def ', line):
            body = n == target['range']['start']['line']
        elif body:
            called += [w.decode() for w in re.findall(rb'(\w+)\(', line)]
    out = []
    for name in called:
        for n, line in enumerate(lines(uri)):
            m = re.match(rb'def (' + re.escape(name.encode()) + rb')\b', line)
            if m:
                out.append({'to': item(uri, n, m.start(1), name), 'fromRanges': []})
    return out

def symbols(params):
    found = []
    for uri in TEXTS:
        for n, line in enumerate(lines(uri)):
            m = re.match(rb'def (\w+)', line)
            if m and params['query'] in m.group(1).decode():
                found.append({'name': m.group(1).decode(), 'kind': 12, 'containerName': 'stand-in',
                              'location': {'uri': uri, 'range': item(uri, n, m.start(1), m.group(1).decode())['range']}})
    return found

def tokens(params):
    data, last_line, last_start = [], 0, 0
    for n, line in enumerate(lines(params['textDocument']['uri'])):
        for m in re.finditer(rb'[A-Za-z_]\w*', line):
            if m.group() == b'def' or m.group() == b'return':
                continue
            kind = 1 if line[:m.start()].rstrip().endswith(b'def') else 0
            data += [n - last_line, m.start() - (last_start if n == last_line else 0), m.end() - m.start(), kind, 0]
            last_line, last_start = n, m.start()
    return {'data': data}

def signature(params):
    line = lines(params['textDocument']['uri'])[params['position']['line']][:params['position']['character']]
    if line.count(b'(') <= line.count(b')'):
        return None
    return {'signatures': [{'label': 'add(a, b)', 'parameters': [{'label': 'a'}, {'label': 'b'}]}],
            'activeSignature': 0, 'activeParameter': 1}

def document_symbols(params):
    uri, found = params['textDocument']['uri'], []
    for n, line in enumerate(lines(uri)):
        m = re.match(rb'def (\w+)', line)
        if m:
            found.append(dict(item(uri, n, m.start(1), m.group(1).decode()), children=[]))
            continue
        m = re.match(rb'\s+(\w+) = ', line)
        if m and found:
            found[-1]['children'].append(dict(item(uri, n, m.start(1), m.group(1).decode()), kind=13, children=[]))
    for f in found:
        del f['uri']
    return found

def describe(params):
    word = word_at(params)
    if word == 'die':
        sys.exit(0)
    if word == 'huge':
        return {'contents': 'word ' * 200000}
    return {'contents': {'kind': 'markdown', 'value': f'```python\n{word}: int\n```\nA number of the stand-in.'}}

def ask_to_edit(uri):
    root = ROOT[0].rstrip('/')
    one = {'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': 0, 'character': 0}}, 'newText': 'edited '}
    send({'jsonrpc': '2.0', 'id': 'edit', 'method': 'workspace/applyEdit', 'params': {'label': 'stand-in edit', 'edit': {'changes': {
        root + '/inside.txt': [one], root + '/../outside.txt': [one], 'file://otherhost/x.txt': [one],
        root + '/missing.txt': [one]}}}})

def offset(data, position):
    """The octet of DATA (bytes) at POSITION, a line and an octet in it."""
    starts = [0] + [i + 1 for i, b in enumerate(data) if b == 10]
    line = min(position['line'], len(starts) - 1)
    return min(starts[line] + position['character'], len(data))

def change(uri, changes):
    for c in changes:
        if 'range' in c:
            data = TEXTS[uri].encode()
            start, end = offset(data, c['range']['start']), offset(data, c['range']['end'])
            TEXTS[uri] = (data[:start] + c['text'].encode() + data[end:]).decode()
            if LOG: LOG.write('ranges\n'); LOG.flush()
        else:
            TEXTS[uri] = c['text']
            if LOG: LOG.write('whole\n'); LOG.flush()

def pulled(params):
    uri = params['textDocument']['uri']
    items = [{'range': r, 'severity': 3, 'source': 'stand-in', 'message': 'ugh found'} for r in places(uri, 'ugh')]
    return {'kind': 'full', 'resultId': str(len(items)), 'items': items}

def blocks(params):
    found, text = [], lines(params['textDocument']['uri'])
    for n, line in enumerate(text):
        if re.match(rb'\s*def ', line):
            indent = len(line) - len(line.lstrip())
            last = n
            for k in range(n + 1, len(text)):
                if text[k].strip() and len(text[k]) - len(text[k].lstrip()) <= indent:
                    break
                if text[k].strip():
                    last = k
            if last > n:
                found.append({'startLine': n, 'endLine': last})
    return found

def selection(params):
    uri, out = params['textDocument']['uri'], []
    for p in params['positions']:
        line = lines(uri)[p['line']]
        whole = {'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': len(lines(uri)) - 1, 'character': 0}}}
        own = {'range': {'start': {'line': p['line'], 'character': 0}, 'end': {'line': p['line'], 'character': len(line)}}, 'parent': whole}
        word = own
        for m in re.finditer(rb'\w+', line):
            if m.start() <= p['character'] <= m.end():
                word = {'range': {'start': {'line': p['line'], 'character': m.start()}, 'end': {'line': p['line'], 'character': m.end()}}, 'parent': own}
        out.append(word)
    return out

def lenses(params):
    uri = params['textDocument']['uri']
    return [{'range': item(uri, n, m.start(1), m.group(1).decode())['range'], 'data': m.group(1).decode()}
            for n, line in enumerate(lines(uri)) for m in [re.match(rb'\s*def (\w+)', line)] if m]

def resolve_lens(lens):
    name = lens['data']
    calls = sum(len(re.findall(rb'\b' + name.encode() + rb'\(', l)) for t in TEXTS.values() for l in t.encode().split(b'\n'))
    return dict(lens, command={'title': f'{calls} calls', 'command': 'stand-in.count', 'arguments': [name]})

def run(params):
    if LOG:
        LOG.write(f"ran {params['arguments'][0]}\n"); LOG.flush()
    return None

ROOT = [None]

def progress(kind, **value):
    send({'jsonrpc': '2.0', 'method': '$/progress', 'params': {'token': 'index', 'value': dict(kind=kind, **value)}})

INDEXING = [True]

def implementations(params):
    uri, word = params['textDocument']['uri'], word_at(params) or ''
    return [{'uri': uri, 'range': item(uri, n, m.start(1), word)['range']}
            for n, line in enumerate(lines(uri)) for m in [re.match(rb'\s*def (' + re.escape(word.encode()) + rb')\b', line)] if m]

def type_definition(params):
    uri = params['textDocument']['uri']
    for n, line in enumerate(lines(uri)):
        m = re.match(rb'class (\w+)', line)
        if m:
            return {'uri': uri, 'range': item(uri, n, m.start(1), m.group(1).decode())['range']}
    return None

def organise(uri):
    text = lines(uri)
    top = 0
    while top < len(text) and text[top].startswith(b'import '):
        top += 1
    ordered = sorted(text[:top])
    if ordered == text[:top]:
        return []
    new = b'\n'.join(ordered).decode() + '\n'
    return [{'title': 'Organise imports', 'kind': 'source.organizeImports',
             'edit': {'changes': {uri: [{'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': top, 'character': 0}}, 'newText': new}]}}}]

def code_actions(params):
    uri = params['textDocument']['uri']
    if 'source.organizeImports' in (params.get('context') or {}).get('only', []):
        return organise(uri)
    return [{'title': 'Make bad good', 'kind': 'quickfix',
             'edit': {'changes': {uri: [{'range': r, 'newText': 'good'} for r in places(uri, 'bad')]}}}]

ANSWERS = {
    'textDocument/completion': completion,
    'textDocument/definition': definition,
    'textDocument/references': lambda p: [{'uri': p['textDocument']['uri'], 'range': r} for r in places(p['textDocument']['uri'], word_at(p) or '')],
    'textDocument/hover': describe,
    'textDocument/signatureHelp': signature,
    'textDocument/prepareCallHierarchy': prepare_calls,
    'callHierarchy/incomingCalls': incoming,
    'callHierarchy/outgoingCalls': outgoing,
    'workspace/symbol': symbols,
    'textDocument/semanticTokens/full': tokens,
    'textDocument/documentSymbol': document_symbols,
    'textDocument/implementation': implementations,
    'textDocument/diagnostic': pulled,
    'textDocument/foldingRange': blocks,
    'textDocument/selectionRange': selection,
    'textDocument/codeLens': lenses,
    'codeLens/resolve': resolve_lens,
    'workspace/executeCommand': run,
    'completionItem/resolve': lambda item: dict(item, documentation={'kind': 'markdown', 'value': f"The word `{item['label']}`."}),
    'textDocument/typeDefinition': type_definition,
    'textDocument/documentHighlight': lambda p: [{'range': r, 'kind': 1} for r in places(p['textDocument']['uri'], word_at(p) or '')],
    'textDocument/rename': lambda p: {'changes': {p['textDocument']['uri']: [{'range': r, 'newText': p['newName']}
                                                                              for r in places(p['textDocument']['uri'], word_at(p) or '')]}},
    'textDocument/formatting': layout,
    'textDocument/codeAction': code_actions,
    'textDocument/inlayHint': lambda p: [{'position': {'line': n, 'character': m.end(1)}, 'label': ': int'}
                                         for n, line in enumerate(lines(p['textDocument']['uri']))
                                         for m in [re.match(rb'(\w+) = ', line)] if m],
}

while True:
    message = read()
    if message is None:
        break
    method = message.get('method')
    if LOG and method:
        LOG.write(method + '\n'); LOG.flush()
    params = message.get('params') or {}
    if method == 'initialize':
        ROOT[0] = params.get('rootUri')
        send({'jsonrpc': '2.0', 'id': message['id'], 'result': {'capabilities': {
            'positionEncoding': 'utf-8', 'textDocumentSync': 2, 'hoverProvider': True, 'definitionProvider': True,
            'referencesProvider': True, 'renameProvider': True, 'documentFormattingProvider': True,
            'completionProvider': {'resolveProvider': True}, 'codeActionProvider': True, 'inlayHintProvider': True,
            'signatureHelpProvider': {'triggerCharacters': ['(']}, 'documentRangeFormattingProvider': True,
            'callHierarchyProvider': True, 'workspaceSymbolProvider': True,
            'documentSymbolProvider': True, 'documentHighlightProvider': True,
            'implementationProvider': True, 'typeDefinitionProvider': True,
            'diagnosticProvider': {'interFileDependencies': False, 'workspaceDiagnostics': False},
            'foldingRangeProvider': True, 'selectionRangeProvider': True,
            'codeLensProvider': {'resolveProvider': True}, 'executeCommandProvider': {'commands': ['stand-in.count']},
            'semanticTokensProvider': {'legend': {'tokenTypes': ['variable', 'function'], 'tokenModifiers': []}, 'full': True}}}})
    elif method is None and 'id' in message:
        if LOG and message['id'] == 'unknown':
            LOG.write(f"answered unknown {message.get('error', {}).get('code')}\n"); LOG.flush()
        if LOG and message['id'] == 'edit':
            LOG.write(f"applied {str(message.get('result', {}).get('applied')).lower()}\n"); LOG.flush()
    elif method == 'textDocument/didOpen':
        TEXTS[params['textDocument']['uri']] = params['textDocument']['text']
        VERSIONS[params['textDocument']['uri']] = params['textDocument'].get('version')
        diagnostics(params['textDocument']['uri'])
        if 'editme' in TEXTS[params['textDocument']['uri']]:
            ask_to_edit(params['textDocument']['uri'])
        if 'stall' in TEXTS[params['textDocument']['uri']]:
            time.sleep(60)
    elif method == 'initialized':
        progress('begin', title='Indexing', percentage=40)
        if ROOT[0]:
            send({'jsonrpc': '2.0', 'method': 'textDocument/publishDiagnostics', 'params': {
                'uri': ROOT[0].rstrip('/') + '/elsewhere.py',
                'diagnostics': [{'range': {'start': {'line': 1, 'character': 4}, 'end': {'line': 1, 'character': 8}},
                                 'severity': 2, 'source': 'stand-in', 'message': 'elsewhere found'}]}})
        send({'jsonrpc': '2.0', 'id': 'unknown', 'method': 'tuatara/no-such-question', 'params': {}})
        send({'jsonrpc': '2.0', 'id': 'watch', 'method': 'client/registerCapability',
              'params': {'registrations': [{'id': 'py', 'method': 'workspace/didChangeWatchedFiles',
                                            'registerOptions': {'watchers': [{'globPattern': '**/*.py'}]}}]}})
    elif method == 'workspace/didChangeWatchedFiles':
        if LOG:
            for c in params['changes']:
                LOG.write(f"watched {c['uri'].rsplit('/', 1)[-1]} {c['type']}\n")
            LOG.flush()
    elif method == 'textDocument/didChange':
        if INDEXING:
            INDEXING.clear()
            progress('end')
        change(params['textDocument']['uri'], params['contentChanges'])
        VERSIONS[params['textDocument']['uri']] = params['textDocument'].get('version')
        diagnostics(params['textDocument']['uri'])
    elif method == 'textDocument/didSave':
        check_saved(params['textDocument']['uri'])
    elif method in ANSWERS:
        send({'jsonrpc': '2.0', 'id': message['id'], 'result': ANSWERS[method](params)})
    elif method == 'shutdown':
        send({'jsonrpc': '2.0', 'id': message['id'], 'result': None})
    elif method == 'exit':
        break
    elif 'id' in message and method:
        send({'jsonrpc': '2.0', 'id': message['id'], 'result': None})
