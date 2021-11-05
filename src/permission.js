import router, { addRoutes } from './router'
import store from './store'
import NProgress from 'nprogress' // Progress 进度条
import 'nprogress/nprogress.css' // Progress 进度条样式
import { ElMessage } from 'element-plus'
import { getToken, getRequestUrl } from '@/utils/storage'
// import { fetchMenus } from '@/api/user' // 获取菜单
import configRouter from '@/router/modules/config'
import projectRouter from '@/router/modules/project'

import { validRequestUrl } from '@/utils/validate' // 请求路径验证规则

NProgress.configure({ showSpinner: false }) // 进度条配置：不显示Loading图标

// 页面白名单
const whiteList = ['/login', '/auth-redirect']

// 全局路由守卫
router.beforeEach(async (to, from, next) => {
  // 触发加载进度条
  NProgress.start()

  // 设置页面标题（用于浏览器标签名称展示）
  document.title = getPageTitle(to.meta.title)

  // 从缓存中获取请求地址及用户token
  const requestUrl = getRequestUrl()
  const hasToken = getToken()

  // 白名单
  if (whiteList.includes(to.path)) {
    next()
  }

  // TODO:请求地址未填写，则回到登录页面填写请求地址
  if (!requestUrl || !validRequestUrl(requestUrl)) {
    try {
      // TODO: 重置请求地址,修改名称
      await store.dispatch('user/resetRequestUrl')

      if (to.path !== '/login') {
        // TODO: reload
        // location.reload()// 为了重新实例化vue-router对象 避免bug
        next('/login')
      }
    } catch (error) {
      console.log(error)
    }
  }

  // if (!hasToken) {
  //   // 其他无权访问的页面将重定向到登录页面。
  //   next(`/login?redirect=${to.path}`)
  //   // NProgress.done()
  // }

  if (hasToken) {
    /**
     * 确定用户是否已通过getInfo获得其权限角色
     * 服务端处理：权限变更后使token失效 -> 避免权限变更未及时拉取新的权限
     */
    const hasRoles = store.getters.roles && store.getters.roles.length > 0
    if (!hasRoles) {
      try {
        // TODO: 提示用户正在加载权限？
        // TODO: 封装首次加载需要缓存的信息
        await store.dispatch('user/getInfo')
        // 基础配置信息
        await store.dispatch('config/fetchConfigInfo')
        // 科目信息
        // await store.dispatch('interfaceCache/fetchSubjectSimple')
        if (!store.getters.roles || store.getters.roles.length === 0) {
          ElMessage({
            message: '您没有权限，请联系管理员为您分配权限后再次登录',
            type: 'info',
            duration: 3 * 1000
          })
        }
      } catch (error) {
        console.log('permission：获取信息失败', error)
        store.dispatch('user/logout')
        next('/login')
      }
    }
    // 设置当前模块
    store.dispatch('user/setCurrentMenu', to)
    // 是否加载菜单
    if (!store.getters.loadedMenus) {
      const res = await loadMenus(next, to)
      if (res) {
        next({ ...to, replace: true })
      } else {
        next()
      }
    } else {
      next()
    }
  } else {
    next()
  }

  // NProgress.done()
})

router.afterEach(() => {
  // 跳转结束，关闭进度条
  NProgress.done()
})

// 加载菜单
const loadMenus = async (next, to) => {
  let success = false
  try {
    // 菜单：content
    // const { content = [] } = await fetchMenus()
    const content = [configRouter, projectRouter]
    await store.dispatch('permission/generateRoutes', content)
    const asyncRoutes = await store.dispatch('permission/setRoutes', to.path)
    addRoutes(asyncRoutes)
    // 设置为已加载
    await store.dispatch('user/updateLoadedMenus')
    success = true
    // next({ ...to, replace: true })
  } catch (error) {
    console.log('loadMenus', error)
    // next()
  }
  return success
}

// 页面标题
const getPageTitle = (pageTitle) => {
  const title = store.getters.title
  if (pageTitle) {
    return `${pageTitle} - ${title}`
  }
  return `${title}`
}
