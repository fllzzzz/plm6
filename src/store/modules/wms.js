import { getWmsConfig } from '@/api/config/wms/base'

import { MAT_BASE_UNIT } from '@/settings/config'

// TODO: 加入接口数据缓存有效时间，避免页面长时间未刷新
const state = {
  baseUnit: MAT_BASE_UNIT, // 基础单位
  inboundSteelCfg: {}, // 入库钢材配置
  inboundFillWayCfg: {}, //  入库配置
  outboundCfg: {}, // 辅材出库方式
  loaded: {
    // 接口是否加载
    config: false
  }
}

const mutations = {
  SET_MAT_BASE_UNIT(state, unit) {
    state.matBaseUnit = unit
  },
  SET_LOADED(state, { key, loaded = true }) {
    state.loaded[key] = loaded
  },
  SET_INBOUND_STEEL_CFG(state, config) {
    state.inboundSteelCfg = config
  },
  SET_INBOUND_FILL_WAY_CFG(state, config) {
    state.inboundFillWayCfg = config
  },
  SET_OUTBOUND_CFG(state, config) {
    state.outboundCfg = config
  }
}

// 加载配置文件
const actions = {
  fetchConfigInfo() {
    console.log('TODO：加载配置文件')
  },
  // wms 基础配置
  async fetchWmsConfig({ commit }) {
    const { inbound, outbound } = await getWmsConfig()
    commit('SET_INBOUND_STEEL_CFG', inbound.steel)
    commit('SET_INBOUND_FILL_WAY_CFG', inbound.fillWay)
    commit('SET_OUTBOUND_CFG', outbound)
    commit('SET_LOADED', { key: 'config' })
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
