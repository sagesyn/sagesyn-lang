import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  // Get the server path from configuration or use default
  const config = vscode.workspace.getConfiguration('sag');
  let serverPath = config.get<string>('server.path') || '';

  if (!serverPath) {
    // Try to find sag-lsp in PATH or use bundled version
    serverPath = 'sag-lsp';
  }

  const serverOptions: ServerOptions = {
    run: {
      command: serverPath,
      transport: TransportKind.stdio,
    },
    debug: {
      command: serverPath,
      transport: TransportKind.stdio,
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'sag' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.sag'),
    },
    outputChannelName: 'Sage Agent Language Server',
  };

  client = new LanguageClient(
    'sag-language-server',
    'Sage Agent Language Server',
    serverOptions,
    clientOptions
  );

  // Start the client
  client.start();

  context.subscriptions.push({
    dispose: () => {
      if (client) {
        client.stop();
      }
    },
  });

  // Show notification
  vscode.window.showInformationMessage('Sage Agent Language extension activated');
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
