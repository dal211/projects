import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './components/App.jsx'
import './index.css'

// Global error handler to catch crash and show on screen
window.onerror = function (message, source, lineno, colno, error) {
    const errorDiv = document.createElement('div');
    errorDiv.style.position = 'fixed';
    errorDiv.style.top = '0';
    errorDiv.style.left = '0';
    errorDiv.style.width = '100%';
    errorDiv.style.backgroundColor = '#990000';
    errorDiv.style.color = 'white';
    errorDiv.style.padding = '20px';
    errorDiv.style.zIndex = '9999';
    errorDiv.style.fontFamily = 'monospace';
    errorDiv.style.whiteSpace = 'pre-wrap';
    errorDiv.innerHTML = `<h3>Application Error</h3>
    <p><strong>Message:</strong> ${message}</p>
    <p><strong>Source:</strong> ${source}:${lineno}:${colno}</p>
    <p><strong>Stack:</strong> ${error ? error.stack : 'No stack trace'}</p>`;
    document.body.appendChild(errorDiv);
};

ReactDOM.createRoot(document.getElementById('root')).render(
    <React.StrictMode>
        <App />
    </React.StrictMode>,
)
