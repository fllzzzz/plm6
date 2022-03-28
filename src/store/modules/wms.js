import { getWmsConfig } from '@/api/config/wms/base'
import { getNotPrintedMaterialNumber } from '@/api/wms/material-label-print/index'

import { MAT_BASE_UNIT } from '@/settings/config'

// TODO: 加入接口数据缓存有效时间，避免页面长时间未刷新
const state = {
  baseUnit: MAT_BASE_UNIT, // 基础单位
  inboundSteelCfg: {}, // 入库钢材配置
  inboundFillWayCfg: {}, //  入库配置
  outboundCfg: {}, // 出库基础配置
  rejectCfg: {}, // 退货基础配置
  partyABorrowReturnCfg: {}, // 甲供借用归还配置
  notPrintedMaterialNumber: { totalMaterial: 0, inboundMaterial: 0, outboundMaterial: 0, transferMaterial: 0, returnMaterial: 0 }, // 未打印物料数量
  loaded: {
    // 接口是否加载
    config: false,
    // 未打印的数量
    notPrintedMaterialNumber: false
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
  },
  SET_REJECT_CFG(state, config) {
    state.rejectCfg = config
  },
  SET_PARTY_A_BORROW_RETURN_CFG(state, config) {
    state.partyABorrowReturnCfg = config
  },
  SET_NOT_PRINTED_MATERIAL_NUMBER(state, number) {
    state.notPrintedMaterialNumber = number
  }
}

// 加载配置文件
const actions = {
  fetchConfigInfo() {
    console.log('TODO：加载配置文件')
  },
  // wms 基础配置
  async fetchWmsConfig({ commit }) {
    const { inbound = {}, outbound = {}, reject = {}, partyABorrowReturn = {}} = await getWmsConfig()
    commit('SET_INBOUND_STEEL_CFG', inbound.steel)
    commit('SET_INBOUND_FILL_WAY_CFG', inbound.fillWay)
    commit('SET_OUTBOUND_CFG', outbound)
    commit('SET_REJECT_CFG', reject)
    commit('SET_PARTY_A_BORROW_RETURN_CFG', partyABorrowReturn)
    commit('SET_LOADED', { key: 'config' })
  },
  async fetchNotPrintedMaterialNumber({ commit }) {
    const {
      totalMaterial = 0,
      inboundMaterial = 0,
      outboundMaterial = 0,
      transferMaterial = 0,
      returnMaterial = 0
    } = await getNotPrintedMaterialNumber()
    const number = {
      totalMaterial,
      inboundMaterial,
      outboundMaterial,
      transferMaterial,
      returnMaterial
    }
    commit('SET_NOT_PRINTED_MATERIAL_NUMBER', number)
    commit('SET_LOADED', { key: 'notPrintedMaterialNumber' })
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
