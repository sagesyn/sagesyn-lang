import * as vscode from 'vscode';
import { execFile } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | null = null;
let statusBarItem: vscode.StatusBarItem;

/**
 * Check if a command exists in PATH using execFile (safer than exec)
 */
function commandExists(command: string): Promise<string | null> {
  return new Promise((resolve) => {
    // If it's an absolute path, check if file exists and is executable
    if (path.isAbsolute(command)) {
      fs.access(command, fs.constants.X_OK, (err) => {
        resolve(err ? null : command);
      });
      return;
    }

    // Use 'which' on Unix, 'where' on Windows
    const isWindows = process.platform === 'win32';
    const checkCmd = isWindows ? 'where' : 'which';

    // Use execFile with command as argument (safer than exec)
    execFile(checkCmd, [command], (error, stdout) => {
      if (error || !stdout.trim()) {
        resolve(null);
      } else {
        resolve(stdout.trim().split('\n')[0]);
      }
    });
  });
}

/**
 * Show syntax-only mode (no LSP)
 */
function activateSyntaxOnlyMode(context: vscode.ExtensionContext, reason: string) {
  statusBarItem.text = '$(warning) Sage (syntax only)';
  statusBarItem.tooltip = `LSP not available: ${reason}\nClick for more info`;
  statusBarItem.command = 'sag.showLspInfo';
  statusBarItem.show();

  // Register command to show LSP info
  context.subscriptions.push(
    vscode.commands.registerCommand('sag.showLspInfo', async () => {
      const selection = await vscode.window.showInformationMessage(
        'Sage Language Server is not running. Syntax highlighting is available. Install sag-lsp for advanced features like diagnostics, completion, and hover.',
        'How to Install'
      );
      if (selection === 'How to Install') {
        vscode.env.openExternal(
          vscode.Uri.parse('https://github.com/sagesyn/sagesyn-lang#installation')
        );
      }
    })
  );
}

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
  let serverPath = config.get<string>('server.path') || 'sag-lsp';

  // Check if the binary exists BEFORE trying to start the client
  const resolvedPath = await commandExists(serverPath);

  if (!resolvedPath) {
    // Binary not found - activate syntax-only mode without showing error
    activateSyntaxOnlyMode(context, 'sag-lsp not found in PATH');

    // Show a one-time warning
    const dontShowAgainKey = 'sag.dontShowLspWarning';
    const dontShowAgain = context.globalState.get<boolean>(dontShowAgainKey);

    if (!dontShowAgain) {
      const selection = await vscode.window.showWarningMessage(
        'Sage Language Server (sag-lsp) not found. Syntax highlighting works, but install sag-lsp for advanced features.',
        'How to Install',
        "Don't Show Again"
      );

      if (selection === 'How to Install') {
        vscode.env.openExternal(
          vscode.Uri.parse('https://github.com/sagesyn/sagesyn-lang#installation')
        );
      } else if (selection === "Don't Show Again") {
        context.globalState.update(dontShowAgainKey, true);
      }
    }

    return; // Don't try to start LSP
  }

  // Binary exists - start the LSP client
  const serverOptions: ServerOptions = {
    run: {
      command: resolvedPath,
      transport: TransportKind.stdio,
    },
    debug: {
      command: resolvedPath,
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

  try {
    await client.start();

    // LSP started successfully
    statusBarItem.text = '$(check) Sage LSP';
    statusBarItem.tooltip = 'Sage Agent Language Server is running';
    statusBarItem.command = 'sag.showLspInfo';
    statusBarItem.show();

    // Register command to show LSP info
    context.subscriptions.push(
      vscode.commands.registerCommand('sag.showLspInfo', () => {
        vscode.window.showInformationMessage(
          'Sage Language Server is running with full LSP support.'
        );
      })
    );
  } catch (error) {
    // LSP failed to start for some other reason
    const message = error instanceof Error ? error.message : String(error);
    client = null;

    activateSyntaxOnlyMode(context, message);

    vscode.window.showWarningMessage(
      `Sage Language Server failed to start: ${message}. Syntax highlighting is still available.`
    );
  }

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
