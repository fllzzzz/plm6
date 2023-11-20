import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
// 插件：JSX
import vueJsx from '@vitejs/plugin-vue-jsx'
// 插件：按需加载组件样式
import AutoImport from 'unplugin-auto-import/vite'
import Components from 'unplugin-vue-components/vite'
import { ElementPlusResolver } from 'unplugin-vue-components/resolvers'
// 插件：SVG
import { createSvgIconsPlugin } from 'vite-plugin-svg-icons'
import { splitVendorChunkPlugin } from 'vite'
// mock
// import { viteMockServe } from 'vite-plugin-mock'

const { resolve } = require('path')

// https://vitejs.dev/config/
export default ({ command }) => {
  return defineConfig({
    brotliSize: false, // 关闭计算打包时间
    server: {
      open: true,
      // host: '172.16.1.9',
      host: '0.0.0.0',
      port: 681, // 本地服务端口
      strictPort: false // 设为 true 时若端口已被占用则会直接退出，而不是尝试下一个可用端口
      // proxy: {
      //   '/server': {
      //     target: 'http://172.16.3.23:8088', // 代理地址（可处理跨域问题）
      //     ws: false, // 如果要代理 websockets，配置这个参数
      //     secure: false, // 如果是https接口，需要配置这个参数
      //     changeOrigin: true, // 是否跨域
      //     rewrite: path => path.replace(/^\/server/, '')
      //   }
      // }
    },
    plugins: [
      vue(),
      // JSX
      vueJsx(),
      AutoImport({
        resolvers: [ElementPlusResolver()]
      }),
      Components({
        resolvers: [ElementPlusResolver()]
      }),
      // SVG插件
      createSvgIconsPlugin({
        // 配置路径：svg存放地址
        iconDirs: [resolve(__dirname, 'src/icons/svg')],
        symbolId: 'icon-[dir]-[name]'
      }),
      splitVendorChunkPlugin()
    ],
    resolve: {
      alias: [
        { find: '@', replacement: resolve(__dirname, 'src') },
        { find: '@crud', replacement: resolve(__dirname, 'src/components/Crud') },
        { find: '@comp', replacement: resolve(__dirname, 'src/components') },
        { find: '@comp-common', replacement: resolve(__dirname, 'src/components-system/common') },
        { find: '@comp-base', replacement: resolve(__dirname, 'src/components-system/base') },
        { find: '@comp-wms', replacement: resolve(__dirname, 'src/components-system/wms') },
        { find: '@comp-cls', replacement: resolve(__dirname, 'src/components-system/classification') },
        { find: '@comp-mes', replacement: resolve(__dirname, 'src/components-system/mes') },
        { find: '@comp-bridge', replacement: resolve(__dirname, 'src/components-system/bridge') },
        { find: '@comp-label', replacement: resolve(__dirname, 'src/components-system/label') },
        { find: '@compos', replacement: resolve(__dirname, 'src/composables') },
        { find: '@enum', replacement: resolve(__dirname, 'src/utils/enum') },
        { find: '@enum-ms', replacement: resolve(__dirname, 'src/utils/enum/modules') },
        { find: '@data-type', replacement: resolve(__dirname, 'src/utils/data-type') }
      ],
      // 忽略后缀名的配置选项, 添加 .vue 选项时要记得原本默认忽略的选项也要手动写入
      extensions: ['.mjs', '.js', '.ts', '.jsx', '.tsx', '.json', '.vue']
    },
    build: {
      assetsDir: 'static/img/',
      rollupOptions: {
        output: {
          chunkFileNames: 'static/js/[name]-[hash].js',
          entryFileNames: 'static/js/[name]-[hash].js',
          assetFileNames: 'static/[ext]/[name]-[hash].[ext]'
        }
      }
    }

    // optimizeDeps: {
    //   include: ['axios','jquery','lodash']
    // },
  })
}
