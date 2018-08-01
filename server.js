const express = require('express');
const app = express();
const path = require('path');

app.use('/js', express.static(`${__dirname}/build/static/js`));
app.use('/css', express.static(`${__dirname}/build/static/css`));

app.get('*', (req, res) => {
  res.sendFile(path.join(`${__dirname}/build/index.html`));
});

app.listen(3000);
