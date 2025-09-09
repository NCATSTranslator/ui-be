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

// Utility function to write mock data
function writeMockData(filePath, data) {
  try {
    const fullPath = path.join(__dirname, 'mock-data', filePath);
    const dirPath = path.dirname(fullPath);

    // Create directory if it doesn't exist
    if (!fs.existsSync(dirPath)) {
      fs.mkdirSync(dirPath, { recursive: true });
    }

    const jsonData = JSON.stringify(data, null, 2);
    fs.writeFileSync(fullPath, jsonData, 'utf8');
    console.log(`Mock data written to: ${fullPath}`);
    return true;
  } catch (error) {
    console.error(`Error writing mock data to ${filePath}:`, error);
    return false;
  }
}

// API v1 endpoints

// Configuration
app.get('/api/v1/config', (req, res) => {
  const data = loadMockData('api/v1/config.json');
  if (data) {
    res.status(200).json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Session status
app.get('/api/v1/session/status', (req, res) => {
  const data = loadMockData('api/v1/session/status.json');
  if (data) {
    res.status(200).json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Query status
app.get('/api/v1/query/:qid/status', (req, res) => {
  const { qid } = req.params;
  let data = loadMockData(`api/v1/query/${qid}/status.json`);

  if (data) {
    res.status(200).json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Query results
app.get('/api/v1/query/:qid/result', (req, res) => {
  const { qid } = req.params;
  let data = loadMockData(`api/v1/query/${qid}/result.json`);

  if (data) {
    res.status(200).json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Current user
app.get('/api/v1/users/me', (req, res) => {
  const data = loadMockData('api/v1/users/me.json');
  if (data) {
    res.status(200).json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// User preferences
app.get('/api/v1/users/me/preferences', (req, res) => {
  const data = loadMockData('api/v1/users/me/preferences.json');
  if (data) {
    res.status(200).json(data);
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
    res.status(200).json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Trash user queries
app.put('/api/v1/users/me/queries/trash', (req, res) => {
  const sids = req.body.map(parseInt);
  console.log('PUT /api/v1/users/me/queries/trash - Received data:', JSON.stringify(sids, null, 2));

  if (!Array.isArray(sids)) return res.status(400).json({ error: 'data must be an array' });
  for (let i = 0; i < sids.length; i++) {
    const sid = sids[i];
    if (!sid) return res.status(400).json({ error: 'data must be an array of save IDs' });
  }

  let data = loadMockData('api/v1/users/me/queries.json');
  if (data && Array.isArray(data)) {
    // Mark queries as deleted
    data.forEach(query => {
      if (sids.includes(query.sid)) {
        query.data.deleted = true;
        query.time_updated = new Date().toISOString();
      }
    });

    // Write updated data back to file
    writeMockData('api/v1/users/me/queries.json', data);
  }

  res.status(200).send();
});

// Restore user queries
app.put('/api/v1/users/me/queries/restore', (req, res) => {
  const sids = req.body.map(parseInt);
  console.log('PUT /api/v1/users/me/queries/restore - Received data:', JSON.stringify(sids, null, 2));

  if (!Array.isArray(sids)) {
    return res.status(400).json({ error: 'sids must be an array' });
  }
  for (let i = 0; i < sids.length; i++) {
    const sid = sids[i];
    if (!sid) return res.status(400).json({ error: 'data must be an array of save IDs' });
  }

  let data = loadMockData('api/v1/users/me/queries.json');
  if (data && Array.isArray(data)) {
    data.forEach(query => {
      if (sids.includes(query.sid)) {
        query.data.deleted = false;
        query.time_updated = new Date().toISOString();
      }
    });

    writeMockData('api/v1/users/me/queries.json', data);
  }

  res.status(200).send();
});

// Permanently delete user queries
app.delete('/api/v1/users/me/queries', (req, res) => {
  const sids = req.body.map(parseInt);
  console.log('DELETE /api/v1/users/me/queries - Received data:', JSON.stringify(sids, null, 2));

  if (!Array.isArray(sids)) return res.status(400).json({ error: 'data must be an array' });
  for (let i = 0; i < sids.length; i++) {
    const sid = sids[i];
    if (!sid) return res.status(400).json({ error: 'data must be an array of save IDs' });
  }

  let data = loadMockData('api/v1/users/me/queries.json');
  if (data && Array.isArray(data)) {
    // Permanently remove queries from array
    data = data.filter(query => !sids.includes(query.sid));

    // Write updated data back to file
    writeMockData('api/v1/users/me/queries.json', data);
  }

  res.status(204).send();
});


// User projects
app.get('/api/v1/users/me/projects', (req, res) => {
  const { include_deleted } = req.query;
  let data = loadMockData('api/v1/users/me/projects.json');

  if (data && Array.isArray(data)) {
    if (include_deleted !== 'true') {
      data = data.filter(project => !project.deleted);
    }
    res.status(200).json(data);
  } else {
    res.status(500).json({ error: 'Mock data not available' });
  }
});

// Update user projects
app.put('/api/v1/users/me/projects', (req, res) => {
  const projects = req.body;
  console.log('PUT /api/v1/users/me/projects - Received data:', JSON.stringify(projects, null, 2));

  if (!Array.isArray(projects)) {
    return res.status(400).json({ error: 'Request body must be an array of projects' });
  }

  let data = loadMockData('api/v1/users/me/projects.json');
  const updatedProjects = [];

  if (data && Array.isArray(data)) {
    projects.forEach(updateProject => {
      const index = data.findIndex(project => parseInt(project.id) === updateProject.id);
      if (index !== -1) {
        // Update existing project
        data[index].data.title = updateProject.title;
        data[index].data.pks = updateProject.pks;
        data[index].time_updated = new Date().toISOString();
        updatedProjects.push(data[index]);
      }
    });

    // Write updated data back to file
    writeMockData('api/v1/users/me/projects.json', data);
  }

  res.status(200).json(updatedProjects);
});

// Trash user projects
app.put('/api/v1/users/me/projects/trash', (req, res) => {
  const project_ids = req.body.map(parseInt);
  console.log('PUT /api/v1/users/me/projects/trash - Received data:', JSON.stringify(project_ids, null, 2));
  if (!Array.isArray(project_ids)) {
    return res.status(400).json({ error: 'project_ids must be an array' });
  }

  let data = loadMockData('api/v1/users/me/projects.json');
  if (data && Array.isArray(data)) {
    // Mark projects as deleted
    data.forEach(project => {
      if (project_ids.includes(project.id)) {
        project.deleted = true;
        project.time_updated = new Date().toISOString();
      }
    });

    // Write updated data back to file
    writeMockData('api/v1/users/me/projects.json', data);
  }

  res.status(200).send();
});

// Restore user projects
app.put('/api/v1/users/me/projects/restore', (req, res) => {
  const project_ids = req.body.map(parseInt);
  console.log('PUT /api/v1/users/me/projects/restore - Received data:', JSON.stringify(project_ids, null, 2));

  if (!Array.isArray(project_ids)) {
    return res.status(400).json({ error: 'project_ids must be an array' });
  }

  let data = loadMockData('api/v1/users/me/projects.json');
  if (data && Array.isArray(data)) {
    // Mark projects as restored (not deleted)
    data.forEach(project => {
      if (project_ids.includes(project.id)) {
        project.deleted = false;
        project.time_updated = new Date().toISOString();
      }
    });

    // Write updated data back to file
    writeMockData('api/v1/users/me/projects.json', data);
  }

  res.status(200).send();
});

// Permanently delete user projects
app.delete('/api/v1/users/me/projects', (req, res) => {
  const project_ids = req.body.map(parseInt);
  console.log('DELETE /api/v1/users/me/projects - Received data:', JSON.stringify(project_ids, null, 2));
  
  if (!Array.isArray(project_ids)) {
    return res.status(400).json({ error: 'project_ids must be an array' });
  }

  let data = loadMockData('api/v1/users/me/projects.json');
  if (data && Array.isArray(data)) {
    // Permanently remove projects from array
    data = data.filter(project => !project_ids.includes(project.id));

    // Write updated data back to file
    writeMockData('api/v1/users/me/projects.json', data);
  }

  res.status(204).send();
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
    res.status(200).json(data);
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
    res.status(200).json(data);
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

    res.status(200).json(data);
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
      res.status(200).json(save);
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

    res.status(200).json(data);
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
      res.status(200).json(workspace);
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
  console.log('  DELETE /api/v1/users/me/queries');
  console.log('  PUT /api/v1/users/me/queries/trash');
  console.log('  PUT /api/v1/users/me/queries/restore');
  console.log('  GET /api/v1/users/me/projects');
  console.log('  PUT /api/v1/users/me/projects');
  console.log('  DELETE /api/v1/users/me/projects');
  console.log('  PUT /api/v1/users/me/projects/trash');
  console.log('  PUT /api/v1/users/me/projects/restore');
  console.log('  GET /api/v1/users/me/bookmarks');
  console.log('  GET /api/v1/users/me/saves');
  console.log('  GET /api/v1/users/me/saves/:save_id');
  console.log('  GET /api/v1/users/me/workspaces');
  console.log('  GET /api/v1/users/me/workspaces/:ws_id');
});
