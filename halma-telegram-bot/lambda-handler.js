const spawn = require('child_process').spawn;

exports.handler = function (event, context, callback) {
  process.env['PATH'] = process.env['PATH'] + ':' + process.env['LAMBDA_TASK_ROOT'];
  const executable = './halma-telegram-bot-serverless';
  const main = spawn(executable, { stdio: ['pipe', process.stdout, process.stderr] });

  main.on('error', function (err) {
    console.error('error: ' + err);
    callback(err, err);
  });

  main.on('exit', function (code) {
    if (code !== 0) {
      callback(new Error(
        "'" + executable + "' exited with non-zero exit code. Please check stderr for details."
      ));
    } else {
      callback(null, {
        statusCode: 200,
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ "ok": true, "description": "update has been processed" })
      });
    }
  });

  main.stdin.write(event.body);
  main.stdin.end();
};
