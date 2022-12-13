import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pino } from 'pino-http';

const __root = path.dirname(url.fileURLToPath(import.meta.url));
const app = express();

app.use(pino());
app.use(express.static('../build'));

app.get('/creative_query', (req, res) =>
  {
  });

app.get('/creative_status', (req, res) =>
  {
  });

app.get('/creative_result', (req, res) =>
  {
  });

app.get('*', (req, res) =>
  {
    res.sendFile(path.join(__root, '../build/index.html'));
  });


app.listen(8000);
