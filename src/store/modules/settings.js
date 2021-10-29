import storage from '@/utils/storage'
import { showSettings, tagsView, fixedHeader, showSidebarLogo, theme, tableBorder, tableStripe, tablePageSize } from '@/settings/user'
import { title, logo, sidebarLogo, projectNameShowConfig } from '@/settings/system'

// 获取缓存的配置信息
const stateStorage = storage.get('setting') || {}
const state = {
  // logo
  logo: stateStorage.logo || logo,
  // 菜单栏顶部logo
  sidebarLogo: stateStorage.sidebarLogo || sidebarLogo,
  // 项目标题
  title: stateStorage.title || title,
  // 显示个人风格设置
  showSettings: stateStorage.showSettings || showSettings,
  // 主题
  theme: stateStorage.theme || theme,
  // 表格边框显示
  tableBorder: stateStorage.tableBorder || tableBorder,
  // 表格斑马线显示
  tableStripe: stateStorage.tableStripe || tableStripe,
  // 表格分页每页默认数量
  tablePageSize: stateStorage.tablePageSize || tablePageSize,
  // 显示标签页
  tagsView: stateStorage.tagsView || tagsView,
  // 吸顶
  fixedHeader: stateStorage.fixedHeader || fixedHeader,
  // 显示菜单栏顶部logo
  showSidebarLogo: stateStorage.showSidebarLogo || showSidebarLogo,
  // 项目名称显示配置
  projectNameShowConfig: stateStorage.projectNameShowConfig || projectNameShowConfig
}

// 将配置缓存到本地
storage.set('setting', state)

const mutations = {
  CHANGE_SETTING: (state, map) => {
    map.forEach((value, key) => {
      if (Object.prototype.hasOwnProperty.call(state, key)) {
        state[key] = value
      }
    })
    storage.set('setting', state)
  }
}

const actions = {
  /**
   * 更改设置
   * @param {Map} data
   */
  changeSetting({ commit }, data) {
    commit('CHANGE_SETTING', data)
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
