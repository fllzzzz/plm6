/*
 * 后端返回文件的url,前端创建a标签来下载
 *
 *  1. 解决了若文件为图片或浏览器支持的格式类型,点击下载会直接打开文件的问题,
 *  2. 下载文件时,浏览器会有闪动的问题
 *
 *  页面内使用
 *  1. 引入指令 import downLoad from '@/directive/down-load-url'
 *  2. 注册指令 directives:{downLoad}
 *  3. 使用,在要下载按钮上以指令的形式使用 例如: <el-button v-downLoad="url">下载</el-button>
 */

import downLoad from './downLoad'

const install = function (Vue) {
  Vue.directive('downLoadUrl', downLoad)
}

downLoad.install = install

export default downLoad
