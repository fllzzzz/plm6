import { constantRoutes } from '@/router/index'
import systemModule from '@/router/modules/system'
import Layout from '@/layout/index.vue'
import BlankLayout from '@/layout/components/AppMain.vue'
import { showProjectSearch } from '@/settings/system'
import { repairStartSymbol } from '@/utils'
import checkPermission from '@/utils/system/check-permission'
import { arr2obj } from '@/utils/convert/type'
import { deepClone } from '@data-type/index'
import { resolvePath } from '@/utils/resolve-path'
import { projectTypeEnum } from '@/utils/enum/modules/contract'

const state = {
  routes: [], // 当前菜单
  allRoutes: [], // 所有菜单
  treeRoutes: {} // 根据模块id划分菜单
  // addRoutes: [] // 添加的菜单
}

const mutations = {
  SET_ROUTES: (state, routes) => {
    // state.addRoutes = routes
    // state.routes = constantRoutes.concat(routes)
    state.routes = routes
  },
  SET_ALL_ROUTES: (state, routes) => {
    // state.allRoutes = constantRoutes.concat(routes)
    state.allRoutes = constantRoutes.concat(routes)
  },
  SET_TREE_ROUTES: (state, routes) => {
    state.treeRoutes = routes
  }
}

const actions = {
  generateRoutes({ commit, state, rootGetters }, routes = []) {
    return new Promise(resolve => {
      if (checkPermission(['admin'])) {
        // 如果是超级管理员【admin】加入系统管理模块
        routes.push(systemModule)
      }
      const accessedRoutes = filterAsyncRoutes(commit, routes)
      const menus = rootGetters.menus
      setMenusRedirect(menus, state.treeRoutes)
      resolve(accessedRoutes)
    })
  },
  setRoutes({ commit, state, rootGetters }, routePath) {
    return new Promise((resolve, reject) => {
      // let moduleId
      let moduleRoutes = []
      // 获取菜单
      const menus = rootGetters.menus
      const menusMap = arr2obj(menus, 'id')
      // TODO: 待删除
      // for (const item of state.allRoutes) {
      //   if (item.routePath === routePath) {
      //     moduleId = item.moduleId
      //   }
      // }
      // if (moduleId) {
      //   // if(moduleId == ) TODO: 相同moduleId不需要重新设置
      //   // moduleRoutes = JSON.parse(JSON.stringify(state.treeRoutes[moduleId])) TODO: 无法使用当前方法深拷贝对象
      //   moduleRoutes = deepClone(state.treeRoutes[moduleId])
      // }
      for (const moduleId in state.treeRoutes) {
        if (state.treeRoutes[moduleId]) {
          const menu = menusMap[moduleId]
          if (menu && menu.redirect !== menu.realRedirect) {
            moduleRoutes.push({ path: menu.redirect, redirect: menu.realRedirect, hidden: true })
          }
          let _routes = deepClone(state.treeRoutes[moduleId])
          // 建刚mes 处理菜单隐藏 ；全局项目类型为桥梁时不做处理
          if (menu && menu.id === 2 && rootGetters.currentProjectType && _routes && _routes.length && !(rootGetters.currentProjectType & projectTypeEnum.BRIDGE.V)) {
            _routes = filterRoutesByProjectType(_routes, rootGetters.currentProjectType)
          }
          moduleRoutes = moduleRoutes.concat(_routes)
        }
      }
      moduleRoutes.push({ path: '/:catchAll(.*)', redirect: '/404', hidden: true })
      commit('SET_ROUTES', moduleRoutes)
      resolve(moduleRoutes)
    })
  }
}

const filterRoutesByProjectType = (routes, projectType) => {
  const routesArr = routes.filter(route => {
    if (route.children && route.children.length) {
      route.children = filterRoutesByProjectType(route.children, projectType)
    }
    // 根据projectType 筛选
    if (route.meta && route.meta.projectType && !(route.meta.projectType & projectType)) {
      return false
    }
    // 筛选后 无children 也过滤；例：route.meta.title = 标准用量统计
    if (typeof route.children === 'object' && route.children.length === 0) {
      return false
    }
    return route
  })
  return routesArr
}

// 过滤异步路由
export const filterAsyncRoutes = (commit, routes, moduleId, basePath, hasLayout = false, treeRoutes = {}, allRoutes = []) => { // 遍历后台传来的路由字符串，转换为组件对象, 含递归
  const routesArr = routes.filter(route => {
    if (!moduleId) {
      if (route.children && route.children.length) {
        treeRoutes[route.id] = filterAsyncRoutes(commit, route.children, route.id, basePath, hasLayout, treeRoutes, allRoutes)
      }
    } else {
      // if (route.component) {
      if (route.meta) {
        route.meta.moduleId = moduleId
      }
      let _hasLayout = hasLayout
      if (route.component === 'Layout' || !route.component) { // Layout组件特殊处理
        // 3级菜单处理
        if (!_hasLayout || route.component === 'Layout') {
          route.component = Layout
          _hasLayout = true
        } else {
          route.component = BlankLayout
        }
        // 未设置redirect，则将其设置为noRedirect
        if (!route.redirect) {
          route.redirect = 'noRedirect'
        } else {
          route.redirect = repairStartSymbol(route.redirect, '/')
        }
        route.path = basePath ? route.path : repairStartSymbol(route.path, '/')
        // TODO: 处理非一级菜单 routePath开头带斜杠的问题
        route.routePath = basePath ? resolvePath(basePath, route.path) : route.path
      } else {
        // TODO: redirect 情况似乎未处理
        // TODO: 处理 routePath开头带斜杠的问题
        const component = route.component
        route.component = resolveComponent(component)

        route.routePath = resolvePath(basePath, route.path)

        for (const item of showProjectSearch) {
          // TODO: 处理当菜单填写与setting不一致时 例如有无component最后的index
          if (item.component === component) {
            route.meta.needProject = item.required
            route.meta.projectType = item.type
          }
        }
      }
      // }
      if (route.children && route.children.length) {
        route.children = filterAsyncRoutes(commit, route.children, moduleId, route.routePath, _hasLayout, treeRoutes, allRoutes)
      } else if (route.redirect) {
        return false
      } else {
        delete route.children
      }
      allRoutes.push(route)
    }
    return true
  })
  routes = routesArr
  if (!moduleId) {
    commit('SET_ALL_ROUTES', allRoutes)
    commit('SET_TREE_ROUTES', treeRoutes)
  }
  return routesArr
}

const setMenusRedirect = (menus, treeRoutes) => {
  // 遍历菜单
  menus.forEach(menu => {
    // 遍历路由
    for (const moduleId in treeRoutes) {
      //
      if (+moduleId === +menu.id) {
        const _path = getPath(menu.redirect, treeRoutes[moduleId])
        if (_path) {
          menu.realRedirect = menu.redirect
        } else {
          const _firstRoutePath = getFirstRoutePath(treeRoutes[moduleId])
          if (_firstRoutePath) {
            menu.realRedirect = _firstRoutePath
          } else {
            menu.realRedirect = '/404'
          }
        }
        break
      }
    }
  })
}

const getFirstRoutePath = (treeRoute) => {
  let _path
  if (!treeRoute) {
    return _path
  }
  for (const route of treeRoute) {
    if (!route.redirect) {
      return route.routePath
    }
    if (route.redirect && route.children && route.children.length > 0) {
      _path = getFirstRoutePath(route.children)
      if (_path) {
        break
      }
    }
  }
  return _path
}

// 获取当前路由树第一条转发路径下，最后一级转发地址
const getPath = (path, treeRoute) => {
  let _path
  // 路由不存在，直接返回path
  if (!treeRoute) {
    return _path
  }
  // 遍历路由
  for (const route of treeRoute) {
    // 当路由地址与传入地址相同时
    if (route.routePath === path) {
      if (!route.redirect) {
        return route.routePath
      }
      if (route.redirect && route.children && route.children.length > 0) {
        _path = getPath(route.redirect, route.children)
        if (_path) {
          break
        }
      }
    }
  }
  return _path
}

// 引入component
// 如果用globEager引入，则使用importPage.default返回。glob异步
const pages = import.meta.glob('../../views/**/*.vue')
// const pages = import.meta.globEager('../../views/**/*.vue')

export const resolveComponent = (name) => {
  const importPage = pages[`../../views${name}.vue`]

  if (!importPage) {
    throw new Error(`无法找到 ${name}。 确定这是后缀为.vue的文件么?`)
  }

  return importPage
  // return importPage.default
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
