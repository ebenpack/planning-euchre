const appConfig = require('./client/src/App/Config.js').config
const HtmlWebpackPlugin = require('html-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const WriteFilePlugin = require('write-file-webpack-plugin');
const path = require('path')
const webpack = require('webpack')
const isProd = process.env.NODE_ENV === 'production'

const entries = [
    path.join(__dirname, 'client/src/entry.js'),
    path.join(__dirname, 'client/base.css'),
    path.join(__dirname, 'client/index.css'),
]

const plugins = [
    new MiniCssExtractPlugin(),
    new webpack.DefinePlugin({
        'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
    }),
    new HtmlWebpackPlugin({
        filename: path.resolve(__dirname, 'static/index.html'),
        template: path.resolve(__dirname, 'client/index.ejs'),
    }),
    new WriteFilePlugin({
        test: /\.html$/,
    }),
]

if (isProd) {
    plugins.push(
        new webpack.LoaderOptionsPlugin({
            minimize: true,
            debug: false
        })
    )
}

let pursOptions = {
    src: [
        path.join('client', 'src', '**', '*.purs'),
        path.join('bower_components', 'purescript-*', 'src', '**', '*.purs')
    ]
}

pursOptions = isProd ?
    {
        bundle: true,
        bundleOutput: 'static/dist/bundle.js',
        ...pursOptions
    } : {
        psc: 'psa',
        pscIde: true,
        pscArgs: {
            sourceMaps: true
        },
        ...pursOptions
    };

module.exports = {
    mode: isProd ? 'production' : 'development',
    devtool: isProd ? undefined : 'source-map',
    entry: entries,
    context: path.join(__dirname, 'client'),
    target: 'web',
    output: {
        path: path.join(__dirname, 'static', 'dist'),
        filename: '[name].[hash].js',
        publicPath: appConfig.public_path
    },
    module: {
        rules: [
            {
                test: /\.purs$/,
                use: [{
                    loader: 'purs-loader',
                    options: pursOptions,
                }],
                exclude: /node_modules/
            },
            {
                test: /\.css$/,
                exclude: /node_modules/,
                use: [
                    MiniCssExtractPlugin.loader,
                    {
                        loader: 'css-loader',
                    }
                ],
            },
        ],
    },
    plugins: plugins,
    resolveLoader: {
        modules: [
            path.join(__dirname, 'node_modules')
        ]
    },
    resolve: {
        alias: {
            'react': 'preact-compat',
            'react-dom': 'preact-compat',
            'create-react-class': 'preact-compat/lib/create-react-class'
        },
        modules: [
            'node_modules',
            'bower_components'
        ],
        extensions: ['.js', '.purs']
    },
    performance: { hints: false },
    stats: {
        hash: false,
        timings: false,
        version: false,
        assets: false,
        errors: true,
        colors: false,
        chunks: false,
        children: false,
        cached: false,
        modules: false,
        chunkModules: false
    }
}
