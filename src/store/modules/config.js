import { getMatClsTree, get as getClassificationTree } from '@/api/config/classification-manage/classification-config'
import { getAll as getDicts } from '@/api/system/dict-detail'
import { getAllUnit } from '@/api/config/main/unit-config'
import { getFactoriesAllSimple } from '@/api/mes/common'
import { getWorkshopsAllSimple } from '@/api/mes/common'
import { getProcessAllSimple } from '@/api/mes/common'
import { getUserAllSimple } from '@/api/common'
import { unitTypeEnum } from '@enum-ms/common'
import useFormatTree from '@compos/classification/use-format-tree'

const state = {
  clsTree: [], // 科目树
  matClsTree: [], // 物料科目树
  dict: {}, // 字典值
  unit: { ALL: [], GROUP: [] }, // 单位列表 ALL，WEIGHT...
  factories: [], // 工厂
  workshops: [], // 车间
  process: [], // 工序
  users: [], // 人员列表
  loaded: { // 接口是否加载
    factories: false,
    workshops: false,
    process: false,
    users: false,
    unit: false,
    matClsTree: false,
    clsTree: false
  }
}

const mutations = {
  SET_LOADED(state, { key, loaded }) {
    state.loaded[key] = loaded
  },
  SET_MAT_CLS_TREE(state, tree) {
    state.matClsTree = tree
  },
  SET_CLS_TREE(state, tree) {
    state.clsTree = tree
  },
  SET_UNIT(state, unit) {
    state.unit = unit
  },
  SET_FACTORIES(state, factories) {
    state.factories = factories
  },
  SET_WORKSHOPS(state, workshops) {
    state.workshops = workshops
  },
  SET_PROCESS(state, process) {
    state.process = process
  },
  SET_USERS(state, users) {
    state.users = users
  }
}

// 加载配置文件
const actions = {
  fetchConfigInfo() {
    console.log('TODO：加载配置文件')
  },
  // 加载分类
  async fetchMatClsTree({ commit }) {
    const res = await getMatClsTree()
    const tree = useFormatTree(res)
    commit('SET_MAT_CLS_TREE', tree)
    commit('SET_LOADED', { key: 'matClsTree', loaded: true })
    return tree
  },
  async fetchClassificationTree({ commit }) {
    const res = await getClassificationTree()
    const tree = useFormatTree(res)
    commit('SET_CLS_TREE', tree)
    commit('SET_LOADED', { key: 'clsTree', loaded: true })
    return tree
  },
  // 加载字典值
  async fetchDict({ state }, names = []) {
    for (const name of names) {
      const { content = [] } = await getDicts(name)
      const dict = state.dict
      dict[name] = [...content]
      dict.dict[name] = {}
      dict.label[name] = {}
      content.forEach(v => {
        dict.dict[name][v.value] = v
        dict.label[name][v.value] = v.label
      })
    }
  },
  // 加载单位
  async fetchUnit({ commit }) {
    const res = await getAllUnit() || []
    // 单位分为分类列表与全单位列表
    const unit = { ALL: [], GROUP: [], KS: new Map() }
    Object.keys(unitTypeEnum.ENUM).forEach(key => {
      unit[key] = []
    })
    res.forEach(v => {
      const n = {
        id: v.id,
        name: v.name,
        type: v.type,
        symbol: v.symbol
      }
      unit.ALL.push(n)
      unit.KS.set(v.name, v.symbol || v.name)
      unit[unitTypeEnum.VK[v.type]].push(n)
    })
    Object.keys(unitTypeEnum.ENUM).forEach(key => {
      unit.GROUP.push({
        name: unitTypeEnum[key].L,
        type: key,
        options: unit[key]
      })
    })
    // 可以通过名称获取
    unit.symbol = (name) => unit.KS.get(name)
    commit('SET_UNIT', unit)
    commit('SET_LOADED', { key: 'unit', loaded: true })
  },
  async fetchFactories({ commit }) {
    const { content = [] } = await getFactoriesAllSimple()
    commit('SET_FACTORIES', content)
    commit('SET_LOADED', { key: 'factories', loaded: true })
    return content
  },
  async fetchWorkshops({ commit }) {
    const { content = [] } = await getWorkshopsAllSimple()
    commit('SET_WORKSHOPS', content)
    commit('SET_LOADED', { key: 'workshops', loaded: true })
    return content
  },
  async fetchProcess({ commit }) {
    const { content = [] } = await getProcessAllSimple()
    commit('SET_PROCESS', content)
    commit('SET_LOADED', { key: 'process', loaded: true })
    return content
  },
  async fetchUsers({ commit }) {
    const { content = [] } = await getUserAllSimple()
    commit('SET_USERS', content)
    commit('SET_LOADED', { key: 'users', loaded: true })
    return content
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
