import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'), {
  api_url: process.env.ELM_APP_API_URL,
});

app.ports.title.subscribe(title => {
  document.title = title
})

registerServiceWorker();
