import webpack from "webpack";

export const onCreateWebpackConfig = ({ actions }: any) => {
  actions.setWebpackConfig({
    experiments: {
      asyncWebAssembly: true,
      topLevelAwait: true,
    },
    plugins: [
      new webpack.ProvidePlugin({
        Buffer: ["buffer", "Buffer"],
      }),
    ],
  });
};
