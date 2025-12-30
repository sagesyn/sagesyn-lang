import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | null = null;
let statusBarItem: vscode.StatusBarItem;

export async function activate(context: vscode.ExtensionContext) {
  // Create status bar item
  statusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    100
  );
  statusBarItem.name = 'Sage Agent';
  context.subscriptions.push(statusBarItem);

  // Get the server path from configuration or use default
  const config = vscode.workspace.getConfiguration('sag');
  let serverPath = config.get<string>('server.path') || '';

  if (!serverPath) {
    // Try to find sag-lsp in PATH
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

  // Try to start the LSP client
  try {
    await client.start();

    // LSP started successfully
    statusBarItem.text = '$(check) Sage LSP';
    statusBarItem.tooltip = 'Sage Agent Language Server is running';
    statusBarItem.show();

    vscode.window.showInformationMessage(
      'Sage Agent Language extension activated with full LSP support!'
    );
  } catch (error) {
    // LSP failed to start - graceful degradation
    const message = error instanceof Error ? error.message : String(error);

    // Set client to null so deactivate doesn't try to stop it
    client = null;

    // Update status bar to show LSP is not available
    statusBarItem.text = '$(warning) Sage (syntax only)';
    statusBarItem.tooltip = 'LSP not available - click for more info';
    statusBarItem.command = 'sag.showLspInfo';
    statusBarItem.show();

    if (message.includes('ENOENT') || message.includes('not found')) {
      // Binary not found - common case
      const selection = await vscode.window.showWarningMessage(
        'Sage Language Server (sag-lsp) not found. Syntax highlighting is available, but advanced features (diagnostics, completion, hover) require installing sag-lsp.',
        'How to Install',
        'Dismiss'
      );

      if (selection === 'How to Install') {
        vscode.env.openExternal(
          vscode.Uri.parse('https://github.com/sagesyn/sagesyn-lang#installation')
        );
      }
    } else {
      // Other error
      vscode.window.showWarningMessage(
        `Sage Language Server failed to start: ${message}. Syntax highlighting is still available.`
      );
    }
  }

  // Register command to show LSP info
  context.subscriptions.push(
    vscode.commands.registerCommand('sag.showLspInfo', () => {
      const hasLsp = client !== null;
      if (hasLsp) {
        vscode.window.showInformationMessage(
          'Sage Language Server is running with full LSP support.'
        );
      } else {
        vscode.window.showInformationMessage(
          'Sage Language Server is not running. Syntax highlighting is available. Install sag-lsp for advanced features.',
          'How to Install'
        ).then(selection => {
          if (selection === 'How to Install') {
            vscode.env.openExternal(
              vscode.Uri.parse('https://github.com/sagesyn/sagesyn-lang#installation')
            );
          }
        });
      }
    })
  );

  // Register dispose handler
  context.subscriptions.push({
    dispose: () => {
      if (client) {
        client.stop();
      }
    },
  });
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
