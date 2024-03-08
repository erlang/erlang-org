const { resolve } = require('path')
module.exports = {
    mode: 'production',
    //mode: 'development',
    entry: {
        "doc-search": './assets/doc-search.tsx'
    },
    devtool: 'source-map',
    output: {
        filename: '[name].bundle.js',
        path: resolve(__dirname, './assets/js/')
    },
    resolve: {
        extensions: ['.tsx', '.ts', '.js'],
    },
    optimization: {
        minimize: true
    },
    performance: {
        hints: false,
        maxEntrypointSize: 512000,
        maxAssetSize: 512000
    },
    module: {
        rules: [{
                test: /\.css$/i,
                use: ["style-loader", "css-loader"],
            },
            {
                test: /\.tsx?$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            }
        ],
    }
}
