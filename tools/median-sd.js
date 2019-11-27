const fs = require('fs');

fs.readFile('tools/time.txt', 'utf-8', (err, data) => {
   const times = data
      .split('\n')
      .filter(line => line.startsWith('Elapsed time:'))
      .map(line => Number(line.match(/\d\.\d+/)[0]))
      .slice(3);

   const average = times.reduce((sum, x) => sum + x, 0) / times.length;
   const dev = times.reduce((sum, x) => sum + Math.pow(x - average, 2), 0) / times.length;
   const sd = Math.sqrt(dev);
   const wcet = Math.max(...times);
   const best = Math.min(...times);

   console.log({
      wcet: wcet.toFixed(6),
      average: average.toFixed(6),
      best: best.toFixed(6),
      sd,
      relativeSd: sd / average,
   });
});
