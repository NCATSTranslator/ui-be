import express from 'express';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const app = express();
const PORT = process.env.PORT || 8386;

// Middleware
app.use(express.json());

// Serve static files from the built frontend
app.use(express.static(path.join(__dirname, '../build')));

// Utility function to load mock data
function loadMockData(filePath) {
  try {
    const fullPath = path.join(__dirname, 'mock-data', filePath);
    if (fs.existsSync(fullPath)) {
      const data = fs.readFileSync(fullPath, 'utf8');
      return JSON.parse(data);
    } else {
      console.warn(`Mock data file not found: ${fullPath}`);
      return null;
    }
  } catch (error) {
    console.error(`Error loading mock data from ${filePath}:`, error);
    return null;
  }
}

// API v1 endpoints

// Configuration
app.get('/api/v1/config', (req, res) => {
  const data = loadMockData('api/v1/config.json');
  if (data) {
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Session status
app.get('/api/v1/session/status', (req, res) => {
  const data = loadMockData('api/v1/session/status.json');
  if (data) {
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Query status
app.get('/api/v1/query/:qid/status', (req, res) => {
  const { qid } = req.params;
  let data = loadMockData(`api/v1/query/${qid}/status.json`);

  if (data) {
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Query results
app.get('/api/v1/query/:qid/result', (req, res) => {
  const { qid } = req.params;
  let data = loadMockData(`api/v1/query/${qid}/result.json`);

  if (data) {
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Current user
app.get('/api/v1/users/me', (req, res) => {
  const data = loadMockData('api/v1/users/me.json');
  if (data) {
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User preferences
app.get('/api/v1/users/me/preferences', (req, res) => {
  const data = loadMockData('api/v1/users/me/preferences.json');
  if (data) {
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User queries
app.get('/api/v1/users/me/queries', (req, res) => {
  const { include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/queries.json');

  if (data && Array.isArray(data)) {
    // Filter based on include_deleted parameter
    if (include_deleted !== 'true') {
      data = data.filter(query => !query.data.deleted);
    }
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User projects
app.get('/api/v1/users/me/projects', (req, res) => {
  const { include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/projects.json');

  if (data && Array.isArray(data)) {
    // Filter based on include_deleted parameter
    if (include_deleted !== 'true') {
      data = data.filter(project => !project.deleted);
    }
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User bookmarks
app.get('/api/v1/users/me/bookmarks', (req, res) => {
  const { include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/bookmarks.json');

  if (data && Array.isArray(data)) {
    // Filter based on include_deleted parameter
    if (include_deleted !== 'true') {
      data = data.filter(bookmark => !bookmark.deleted);
    }
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User tags
app.get('/api/v1/users/me/tags', (req, res) => {
  const { include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/tags.json');

  if (data && Array.isArray(data)) {
    // Filter based on include_deleted parameter
    if (include_deleted !== 'true') {
      data = data.filter(tag => !tag.deleted);
    }
    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User saves
app.get('/api/v1/users/me/saves', (req, res) => {
  const { include_deleted, type } = req.query;
  let data = loadMockData('api/v1/users/me/saves.json');

  if (data && Array.isArray(data)) {
    // Filter based on include_deleted parameter
    if (include_deleted !== 'true') {
      data = data.filter(save => !save.deleted);
    }

    // Filter by type if specified
    if (type) {
      data = data.filter(save => save.save_type === type);
    }

    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User save by ID
app.get('/api/v1/users/me/saves/:save_id', (req, res) => {
  const { save_id } = req.params;
  const { include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/saves.json');

  if (data && Array.isArray(data)) {
    let save = data.find(s => s.id == save_id);

    if (save && (include_deleted === 'true' || !save.deleted)) {
      res.json(save);
    } else {
      res.status(404).json({ error: 'Save not found' });
    }
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User workspaces
app.get('/api/v1/users/me/workspaces', (req, res) => {
  const { include_data, include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/workspaces.json');

  if (data && Array.isArray(data)) {
    // Filter based on include_deleted parameter
    if (include_deleted !== 'true') {
      data = data.filter(workspace => !workspace.deleted);
    }

    // Remove data field if include_data is false
    if (include_data !== 'true') {
      data = data.map(workspace => {
        const { data: workspaceData, ...rest } = workspace;
        return rest;
      });
    }

    res.json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User workspace by ID
app.get('/api/v1/users/me/workspaces/:ws_id', (req, res) => {
  const { ws_id } = req.params;
  const { include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/workspaces.json');

  if (data && Array.isArray(data)) {
    let workspace = data.find(w => w.id === ws_id);

    if (workspace && (include_deleted === 'true' || !workspace.deleted)) {
      res.json(workspace);
    } else {
      res.status(404).json({ error: 'Workspace not found' });
    }
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Catch-all handler for SPA routing - serve index.html for non-API routes
app.get('*', (req, res) => {
  // If it's an API route that doesn't exist, return 404 JSON
  if (req.path.startsWith('/api/')) {
    res.status(404).json({ error: 'API endpoint not found' });
  } else {
    // For all other routes, serve the React app's index.html
    res.sendFile(path.join(__dirname, '../build/index.html'));
  }
});

// Start server
app.listen(PORT, () => {
  console.log(`Mock server running on http://localhost:${PORT}`);
  console.log('Frontend app available at: http://localhost:' + PORT);
  console.log('Available API endpoints:');
  console.log('  GET /api/v1/config');
  console.log('  GET /api/v1/session/status');
  console.log('  GET /api/v1/query/:qid/status');
  console.log('  GET /api/v1/query/:qid/result');
  console.log('  GET /api/v1/users/me');
  console.log('  GET /api/v1/users/me/preferences');
  console.log('  GET /api/v1/users/me/queries');
  console.log('  GET /api/v1/users/me/projects');
  console.log('  GET /api/v1/users/me/bookmarks');
  console.log('  GET /api/v1/users/me/saves');
  console.log('  GET /api/v1/users/me/saves/:save_id');
  console.log('  GET /api/v1/users/me/workspaces');
  console.log('  GET /api/v1/users/me/workspaces/:ws_id');
});
