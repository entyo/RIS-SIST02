// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

const path = require("path");

module.exports = {
  mode: "development",
  entry: "./src/App.fsproj",
  devtool: "source-map",
  output: {
    path: path.join(__dirname, "./public"),
    filename: "bundle.js"
  },
  devServer: {
    contentBase: "./public",
    port: process.env.PORT || 8080
  },
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        exclude: "/node_modules/",
        use: {
          loader: "fable-loader",
          options: {
            babel: {
              presets: [
                [
                  "@babel/preset-env",
                  {
                    modules: false
                  }
                ]
              ]
            }
          }
        }
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"]
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: "file-loader",
            options: {
              name: "[name].[ext]",
              outputPath: "fonts/"
            }
          }
        ]
      }
    ]
  }
};
