import { addRoutes, resetRouter } from '@/router'
import { login, logout as logoutApi, getInfo, fetchMenus } from '@/api/user'
import { repairStartSymbol } from '@/utils'
import { getToken, setToken, removeToken, getRequestUrl, setRequestUrl as storeSetRequestUrl, removeRequestUrl } from '@/utils/storage' // get token from cookie
import { projectTypeEnum } from '@enum-ms/contract'
import { installProjectTypeEnum, deliveryInstallTypeEnum } from '@enum-ms/project'
import { contractSaleTypeEnum } from '@enum-ms/mes'

import checkPermission from '@/utils/system/check-permission'

const state = {
  requestUrl: getRequestUrl(), // 请求地址
  token: getToken(),
  user: {
    id: '', // 用户标识
    name: '', // 用户姓名
    username: '', // 登录账号
    email: '', // 邮箱
    dept: '', // 部门
    job: '', // 岗位
    avatar: '', // 头像
    introduction: '', // 介绍
    companyName: '', // 公司名称
    phone: '', // 电话
    sex: '', // 性别
    inventoryNotifyPerm: false // 库存预警权限
  },
  productMenu: null, // 产品菜单
  menus: [], // 菜单
  roles: [], // 权限
  currentMenu: null, // 当前主模块
  // 首次加载页面，是否已经加载菜单
  loadedMenus: false,
  projectTypeEnumArr: [projectTypeEnum.STEEL, projectTypeEnum.BRIDGE, projectTypeEnum.CARBARN, projectTypeEnum.ENCLOSURE],
  installTypeEnumArr: [installProjectTypeEnum.ARTIFACT, installProjectTypeEnum.ENCLOSURE, installProjectTypeEnum.AUXILIARY, installProjectTypeEnum.STRAIGHTENER],
  deliveryInstallEnumArr: [deliveryInstallTypeEnum.ARTIFACT, deliveryInstallTypeEnum.ENCLOSURE],
  contractSaleTypeEnumArr: [contractSaleTypeEnum.STRUCTURE, contractSaleTypeEnum.ENCLOSURE, contractSaleTypeEnum.MACHINE_PART, contractSaleTypeEnum.AUXILIARY_MATERIAL],
  flag: 0,
  hasIntelligent: 0,
  hasBridgeMenu: 0
}

const mutations = {
  SET_TOKEN: (state, token) => {
    state.token = token
  },
  SET_MENUS: (state, menus) => {
    state.menus = menus
  },
  SET_ROLES: (state, roles) => {
    state.roles = roles
  },
  SET_PRODUCT_MENU: (state, productMenu) => {
    state.productMenu = productMenu
  },
  SET_CURRENT_MENU: (state, currentMenu) => {
    state.currentMenu = currentMenu
  },
  SET_LOAD_MENUS: (state, loadedMenus) => {
    state.loadedMenus = loadedMenus
  },
  SET_USER: (state, { id, name = '', email = '', username = '', job = '', dept = '', avatar = '', introduction = '', companyName = '', phone = '', inventoryNotifyPerm = false, sex }) => {
    state.user = { id, name, email, username, job, dept, avatar, introduction, companyName, phone, sex, inventoryNotifyPerm }
  },
  SET_REQUEST_URL: (state, requestUrl) => {
    state.requestUrl = requestUrl
  },
  SET_PROJECT_TYPE_ENUM_ARR: (state, projectTypeEnumArr) => {
    state.projectTypeEnumArr = projectTypeEnumArr
  },
  SET_INSTALL_TYPE_ENUM_ARR: (state, installTypeEnumArr) => {
    state.installTypeEnumArr = installTypeEnumArr
  },
  SET_DELIVERY_INSTALL_ARR: (state, deliveryInstallEnumArr) => {
    state.deliveryInstallEnumArr = deliveryInstallEnumArr
  },
  SET_CONTRACT_SALE_TYPE_ENUM_ARR: (state, contractSaleTypeEnumArr) => {
    state.contractSaleTypeEnumArr = contractSaleTypeEnumArr
  },
  SET_FLAG: (state, flag) => {
    state.flag = flag
  },
  SET_HAS_INTELLIGENT: (state, hasIntelligent) => {
    state.hasIntelligent = hasIntelligent
  },
  SET_HAS_BRIDGE_MENU: (state, hasBridgeMenu) => {
    state.hasBridgeMenu = hasBridgeMenu
  }
}

const actions = {
  // 用户登录
  login({ commit }, userInfo) {
    const rememberMe = userInfo.rememberMe
    const { username, password } = userInfo
    return new Promise((resolve, reject) => {
      login({ username: username.trim(), password: password }).then(res => {
        if (res.token) {
          setToken(res.token, rememberMe)
          commit('SET_TOKEN', res.token)
          // actions.setInfo(res, commit)
          resolve()
        } else {
          reject()
        }
      }).catch(error => {
        reject(error)
      })
    })
  },

  // 获取用户信息
  getInfo({ commit, state }) {
    return new Promise((resolve, reject) => {
      getInfo(state.token).then(res => {
        actions.setInfo(res, commit)
        resolve(res)
      }).catch(error => {
        reject(error)
      })
    })
  },

  // 设置用户信息
  setInfo(res, commit) {
    // dept
    const { permissions = [], roles = [], productMenu, enclosureType, boolSmartLine, boolBridge } = res
    let { menus = [] } = res

    // 如果没有任何权限，则赋予一个默认的权限，避免请求死循环
    if (permissions.length === 0 && roles.length === 0) {
      commit('SET_ROLES', ['ROLE_SYSTEM_DEFAULT'])
    } else {
      commit('SET_ROLES', permissions.concat(roles))
    }
    if (menus) {
      menus.forEach(m => {
        if (!m.iframe) {
          m.redirect = m.redirect ? repairStartSymbol(m.redirect, '/') : null
        }
      })
    } else {
      menus = []
    }
    if (checkPermission(['admin'])) {
      // 如果是超级管理员【admin】加入系统管理模块
      menus.push({ name: '系统管理', id: -2, icon: 'module-system', redirect: '/system' })
    }
    const arr = enclosureType ? [projectTypeEnum.STEEL, projectTypeEnum.CARBARN, projectTypeEnum.BRIDGE] : [projectTypeEnum.STEEL, projectTypeEnum.CARBARN, projectTypeEnum.ENCLOSURE, projectTypeEnum.BRIDGE]
    const installArr = enclosureType ? [installProjectTypeEnum.ARTIFACT, installProjectTypeEnum.AUXILIARY] : [installProjectTypeEnum.ARTIFACT, installProjectTypeEnum.ENCLOSURE, installProjectTypeEnum.AUXILIARY, installProjectTypeEnum.STRAIGHTENER]
    const deliveryArr = enclosureType ? [deliveryInstallTypeEnum.ARTIFACT] : [deliveryInstallTypeEnum.ARTIFACT, deliveryInstallTypeEnum.ENCLOSURE]
    const contractSaleTypeArr = enclosureType ? [contractSaleTypeEnum.STRUCTURE, contractSaleTypeEnum.MACHINE_PART, contractSaleTypeEnum.AUXILIARY_MATERIAL] : [contractSaleTypeEnum.STRUCTURE, contractSaleTypeEnum.ENCLOSURE, contractSaleTypeEnum.MACHINE_PART, contractSaleTypeEnum.AUXILIARY_MATERIAL]
    console.log(contractSaleTypeArr)
    commit('SET_INSTALL_TYPE_ENUM_ARR', installArr)
    commit('SET_PROJECT_TYPE_ENUM_ARR', arr)
    commit('SET_DELIVERY_INSTALL_ARR', deliveryArr)
    commit('SET_CONTRACT_SALE_TYPE_ENUM_ARR', contractSaleTypeArr)
    commit('SET_FLAG', enclosureType)
    commit('SET_HAS_INTELLIGENT', boolSmartLine)
    commit('SET_HAS_BRIDGE_MENU', boolBridge)
    commit('SET_MENUS', menus)
    commit('SET_PRODUCT_MENU', productMenu)
    commit('SET_USER', res)
    // TODO: 头像 后台需要给一个默认头像的地址
    // commit('SET_AVATAR', avatar || 'https://wpimg.wallstcn.com/f778738c-e4f8-4870-b634-56703b4acafe.gif')
    // 第一次加载菜单时用到， 具体见 src 目录下的 permission.js
    commit('SET_LOAD_MENUS', false)
  },

  // 用户退出登录
  async logout({ commit, state, dispatch }) {
    try {
      if (state.token) {
        logoutApi(state.token)
      }
      await dispatch('resetToken')
    } catch (error) {
      console.log('退出登录', error)
    } finally {
      dispatch('tagsView/delAllViews', null, { root: true })
    }
  },

  // 更改加载状态
  updateLoadedMenus({ commit }) {
    return new Promise((resolve, reject) => {
      commit('SET_LOAD_MENUS', true)
      resolve()
    })
  },

  // 移除token
  resetToken({ commit }) {
    return new Promise(resolve => {
      commit('SET_TOKEN', '')
      commit('SET_ROLES', [])
      commit('SET_USER', {})
      removeToken()
      // resetRouter()
      resolve()
    })
  },

  // 移除请求路径
  resetRequestUrl({ commit, dispatch }) {
    return new Promise(resolve => {
      commit('SET_REQUEST_URL', '')
      removeRequestUrl('requestUrl')
      dispatch('logout')
      resolve()
    })
  },

  // 设置请求路径
  setRequestUrl({ commit, dispatch }, requestUrl) {
    return new Promise(resolve => {
      // TODO:可用正则
      let index = 0
      const urlReverseArr = [...requestUrl].reverse()
      // 去掉路径后面最后的斜杠
      for (const i in urlReverseArr) {
        if (urlReverseArr[i] !== '/') {
          index = i
          break
        }
      }
      requestUrl = requestUrl.substr(0, urlReverseArr.length - index)
      commit('SET_REQUEST_URL', requestUrl)
      storeSetRequestUrl(requestUrl)
      dispatch('api/setAPI', {}, { root: true })
      resolve()
    })
  },

  // 若用户权限发生变化，token不发生变化，可调用此方法更新用户权限
  async changeRoles({ commit, dispatch }, role) {
    const { content = [] } = await fetchMenus()
    resetRouter()
    // generate accessible routes map based on roles
    const accessRoutes = await dispatch('permission/generateRoutes', content, { root: true })
    // dynamically add accessible routes
    addRoutes(accessRoutes)
    // reset visited views and cached views
    dispatch('tagsView/delAllViews', null, { root: true })
  },

  /**
   * 设置当前访问菜单
   * @param {object} toRouter 要前往的路由对象
   */
  async setCurrentMenu({ commit }, toRouter) {
    // 存在moduleId 和 菜单
    const flag = toRouter && toRouter.meta && toRouter.meta.moduleId && state.menus
    if (flag) {
      let currentMenu = null
      // 遍历菜单
      for (const item of state.menus) {
        if (item.id === toRouter.meta.moduleId) {
          currentMenu = item
          break
        }
      }
      commit('SET_CURRENT_MENU', currentMenu)
    }
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
