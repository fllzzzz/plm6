import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'
// 插件：JSX
import vueJsx from '@vitejs/plugin-vue-jsx'
// 插件：按需加载组件样式
import styleImport from 'vite-plugin-style-import'
// 插件：SVG
import viteSvgIcons from 'vite-plugin-svg-icons'
// mock
import { viteMockServe } from 'vite-plugin-mock'

import viteCompression from 'vite-plugin-compression'

const { resolve } = require('path')

// https://vitejs.dev/config/
export default ({ command }) => {
  return defineConfig({
    brotliSize: false, // 关闭计算打包时间
    server: {
      open: true,
      // host: '172.16.1.9',
      host: '0.0.0.0',
      port: 668, // 本地服务端口
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
      // 按需加载组件
      styleImport({
        libs: [{
          libraryName: 'element-plus',
          resolveStyle: (name) => {
            name = name.slice(3) // 官网是用splice的（数组方法）。
            if (name === 'sub-menu') {
              return `element-plus/packages/theme-chalk/src/submenu.scss`
            } else {
              return `element-plus/packages/theme-chalk/src/${name}.scss`
            }
          }
          // resolveComponent: (name) => {
          //   return `element-plus/lib/${name}`
          // }
        }]
      }),
      // SVG插件
      viteSvgIcons({
        // 配置路径：svg存放地址
        iconDirs: [resolve(__dirname, 'src/icons/svg')],
        symbolId: 'icon-[dir]-[name]'
      }),
      // MOCK插件
      viteMockServe({
        supportTs: false, // 打开后，可以读取 ts 文件模块。 请注意，打开后将无法监视.js 文件。 默认：true
        mockPath: 'src/api-mock/',
        localEnabled: command === 'serve', // 情景配置 是否为开发模式  serve 或 build
        prodEnabled: command !== 'serve' && import.meta.env && import.meta.env.PROD_MOCK,
        injectCode: `
          import { setupProdMockServer } from '/src/plugins/mock-prod-server';
          setupProdMockServer();
        `
      }),
      // gzip静态资源压缩
      viteCompression({
        verbose: true,
        disable: false,
        threshold: 1024 * 20, // 启用压缩的文件大小限制，20kb以上压缩
        deleteOriginFile: true, // 压缩后是否删除原文件
        algorithm: 'gzip',
        ext: '.gz'
      })
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
          assetFileNames: 'static/[ext]/[name]-[hash].[ext]',
          // 合并js
          manualChunks(moduleName) {
            if (moduleName.includes('node_modules')) {
              // 按 node_modules 目录拆分
              return moduleName.toString().split('node_modules/')[1].split('/')[0].toString()
            } else if (moduleName.includes('views')) {
              // 按 views 目录拆分
              return moduleName.toString().split('views/')[1].split('/')[0].toString()
            } else if (moduleName.includes('utils')) {
              // 打印模板单独放一个文件
              if (moduleName.includes('utils/print')) {
                return 'print'
              }
              // 其他放在一起
              return 'utils'
            } else if (moduleName.includes('api')) {
              return 'api'
            } else {
              return 'index'
            }
          }
        }
      }
    }

    // optimizeDeps: {
    //   include: ['axios','jquery','lodash']
    // },
  })
}
