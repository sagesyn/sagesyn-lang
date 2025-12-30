import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient | null = null;
let statusBarItem: vscode.StatusBarItem;

/**
 * Get the platform identifier for bundled binaries
 */
function getPlatformId(): string {
  const platform = os.platform();
  const arch = os.arch();

  if (platform === 'linux') {
    return arch === 'arm64' ? 'linux-arm64' : 'linux-x64';
  } else if (platform === 'darwin') {
    return arch === 'arm64' ? 'darwin-arm64' : 'darwin-x64';
  } else if (platform === 'win32') {
    return 'win32-x64';
  }

  return `${platform}-${arch}`;
}

/**
 * Get the binary name for the current platform
 */
function getBinaryName(): string {
  return os.platform() === 'win32' ? 'sag-lsp.exe' : 'sag-lsp';
}

/**
 * Find the sag-lsp binary - first check bundled, then PATH
 */
function findServerBinary(context: vscode.ExtensionContext): string | null {
  // 1. Check user-configured path first
  const config = vscode.workspace.getConfiguration('sag');
  const configuredPath = config.get<string>('server.path');
  if (configuredPath && fs.existsSync(configuredPath)) {
    return configuredPath;
  }

  // 2. Check bundled binary in extension
  const platformId = getPlatformId();
  const binaryName = getBinaryName();
  const bundledPath = path.join(context.extensionPath, 'bin', platformId, binaryName);

  if (fs.existsSync(bundledPath)) {
    // Make sure it's executable on Unix
    if (os.platform() !== 'win32') {
      try {
        fs.chmodSync(bundledPath, 0o755);
      } catch {
        // Ignore chmod errors
      }
    }
    return bundledPath;
  }

  // 3. Binary not found
  return null;
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
        'Sage Language Server is not running. Syntax highlighting is available. The bundled LSP binary may not be available for your platform.',
        'Report Issue'
      );
      if (selection === 'Report Issue') {
        vscode.env.openExternal(
          vscode.Uri.parse('https://github.com/sagesyn/sagesyn-lang/issues')
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

  // Find the server binary
  const serverPath = findServerBinary(context);

  if (!serverPath) {
    // Binary not found - activate syntax-only mode
    const platformId = getPlatformId();
    activateSyntaxOnlyMode(context, `No binary for ${platformId}`);

    vscode.window.showWarningMessage(
      `Sage Language Server binary not found for your platform (${platformId}). Syntax highlighting is available, but LSP features are disabled.`,
      'Report Issue'
    ).then(selection => {
      if (selection === 'Report Issue') {
        vscode.env.openExternal(
          vscode.Uri.parse('https://github.com/sagesyn/sagesyn-lang/issues')
        );
      }
    });

    return;
  }

  // Binary exists - start the LSP client
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
    // LSP failed to start
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
