import { getMatClsTree, get as getClassificationTree } from '@/api/config/classification-manage/classification-config'
import { getAll as getDicts } from '@/api/system/dict-detail'
import { getAllUnit } from '@/api/config/main/unit-config'
import { getFactoriesAllSimple } from '@/api/mes/common'
import { getFinalMatClsById, getUserTree, getRegionalCascade } from '@/api/common'
import { getWorkshopsAllSimple } from '@/api/mes/common'
import { getAllFactoryWorkshopLines } from '@/api/mes/common'
import { getProcessAllSimple } from '@/api/mes/common'
import { getUserAllSimple } from '@/api/common'
import { getDeptAllSimple } from '@/api/common'
import { getSuppliersBrief } from '@/api/common'

import { unitTypeEnum } from '@enum-ms/common'
import { materialClassificationEnum } from '@enum-ms/classification'
import { setEmptyArr2Undefined } from '@/utils/data-type/tree'
import { isBlank } from '@/utils/data-type'
import { arr2obj } from '@/utils/convert/type'
import { formatClsTree } from '@/utils/system/classification'

// TODO: 加入接口数据缓存有效时间，避免页面长时间未刷新
const state = {
  clsTree: [], // 科目树
  matClsTree: [], // 物料科目树
  rawMatClsTree: [], // 普通物料科目树（不含制成品）
  classifySpec: { specKV: {}}, // 科目规格
  dict: {}, // 字典值
  unit: { ALL: [], GROUP: [] }, // 单位列表 ALL，WEIGHT...
  factories: [], // 工厂
  factoryKV: {}, // 工厂id:value 格式
  workshops: [], // 车间
  productLines: [], // 生产线
  process: [], // 工序
  users: [], // 人员列表
  dept: [], // 部门列表
  userDeptTree: [], // 人员部门树
  regional: [], // 地区
  suppliers: [], // 供应商列表
  supplierKV: {}, // 供应商id:value 格式
  loaded: {
    // 接口是否加载
    factories: false,
    workshops: false,
    productLines: false,
    process: false,
    users: false,
    dept: false,
    unit: false,
    userDeptTree: false,
    matClsTree: false,
    clsTree: false,
    suppliers: false
  }
}

const mutations = {
  SET_LOADED(state, { key, loaded = true }) {
    state.loaded[key] = loaded
  },
  SET_MAT_CLS_TREE(state, tree) {
    state.matClsTree = tree
    state.rawMatClsTree = tree.filter(t => ![materialClassificationEnum.STRUC_MANUFACTURED.V, materialClassificationEnum.ENCL_MANUFACTURED.V].includes(t.basicClass))
  },
  SET_CLS_TREE(state, tree) {
    state.clsTree = tree
  },
  SET_UNIT(state, unit) {
    state.unit = unit
  },
  SET_FACTORIES(state, factories) {
    state.factories = factories
    // 生产工厂kv
    state.factoryKV = {}
    factories.forEach((v) => {
      state.factoryKV[v.id] = v
    })
  },
  SET_WORKSHOPS(state, workshops) {
    state.workshops = workshops
  },
  SET_PRODUCT_LINES(state, productLines) {
    state.productLines = productLines
  },
  SET_PROCESS(state, process) {
    state.process = process
  },
  SET_USERS(state, users) {
    state.users = users
  },
  SET_SUPPLIERS(state, suppliers) {
    state.suppliers = suppliers
    // 供应商kv
    state.supplierKV = {}
    suppliers.forEach((v) => {
      state.supplierKV[v.id] = v
    })
  },
  SET_DEPT(state, dept) {
    state.dept = dept
  },
  SET_USER_DEPT_TREE(state, tree) {
    state.userDeptTree = tree
  },
  SET_REGIONAL(state, regional) {
    state.regional = regional
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
    const tree = formatClsTree(res)
    commit('SET_MAT_CLS_TREE', tree)
    commit('SET_LOADED', { key: 'matClsTree' })
    return tree
  },
  async fetchClassificationTree({ commit }) {
    const res = await getClassificationTree()
    const tree = formatClsTree(res)
    commit('SET_CLS_TREE', tree)
    commit('SET_LOADED', { key: 'clsTree' })
    return tree
  },
  // 加载字典值
  async fetchDict({ state }, names = []) {
    const allInterFace = []
    const dict = state.dict
    if (!('dict' in dict)) {
      dict.dict = {}
    }
    if (!('label' in dict)) {
      dict.label = {}
    }
    for (const name of names) {
      const ps = getDicts(name).then((res) => {
        const content = res.content || []
        dict[name] = [...content]
        dict.dict[name] = {}
        dict.label[name] = {}
        content.forEach((v) => {
          dict.dict[name][v.value] = v
          dict.label[name][v.value] = v.label
        })
      })
      allInterFace.push(ps)
    }
    await Promise.all(allInterFace)
  },
  // 加载单位
  async fetchUnit({ commit }) {
    const res = (await getAllUnit()) || []
    // 单位分为分类列表与全单位列表
    const unit = { ALL: [], GROUP: [], KS: new Map() }
    Object.keys(unitTypeEnum.ENUM).forEach((key) => {
      unit[key] = []
    })
    res.forEach((v) => {
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
    Object.keys(unitTypeEnum.ENUM).forEach((key) => {
      unit.GROUP.push({
        name: unitTypeEnum[key].L,
        type: key,
        options: unit[key]
      })
    })
    // 可以通过名称获取
    unit.symbol = (name) => unit.KS.get(name)
    commit('SET_UNIT', unit)
    commit('SET_LOADED', { key: 'unit' })
  },
  async fetchFactories({ commit }) {
    const { content = [] } = await getFactoriesAllSimple()
    commit('SET_FACTORIES', content)
    commit('SET_LOADED', { key: 'factories' })
    return content
  },
  async fetchWorkshops({ commit }) {
    const { content = [] } = await getWorkshopsAllSimple()
    commit('SET_WORKSHOPS', content)
    commit('SET_LOADED', { key: 'workshops' })
    return content
  },
  async fetchProductLines({ commit }) {
    const { content = [] } = await getAllFactoryWorkshopLines()
    commit('SET_PRODUCT_LINES', content)
    commit('SET_LOADED', { key: 'productLines', loaded: true })
    return content
  },
  async fetchProcess({ commit }) {
    const { content = [] } = await getProcessAllSimple()
    commit('SET_PROCESS', content)
    commit('SET_LOADED', { key: 'process' })
    return content
  },
  async fetchUsers({ commit }) {
    const { content = [] } = await getUserAllSimple()
    commit('SET_USERS', content)
    commit('SET_LOADED', { key: 'users' })
    return content
  },
  async fetchSuppliers({ commit }) {
    const { content = [] } = await getSuppliersBrief()
    commit('SET_SUPPLIERS', content)
    commit('SET_LOADED', { key: 'suppliers' })
    return content
  },
  async fetchDept({ commit }) {
    const dept = await getDeptAllSimple()
    setEmptyArr2Undefined(dept)
    commit('SET_DEPT', dept)
    commit('SET_LOADED', { key: 'dept' })
    return dept
  },
  async fetchUserDeptTree({ commit }) {
    const content = await getUserTree()
    const tree = content
    setEmptyArr2Undefined(tree)
    commit('SET_USER_DEPT_TREE', tree)
    commit('SET_LOADED', { key: 'userDeptTree' })
    return tree
  },
  async fetchRegional({ commit }) {
    const regional = await getRegionalCascade() || []
    setEmptyArr2Undefined(regional)
    commit('SET_REGIONAL', regional)
    commit('SET_LOADED', { key: 'regional' })
    return regional
  },
  async fetchMarClsSpec({ state }, classifyIds = []) {
    const allInterFace = []
    const classifySpec = state.classifySpec
    for (const id of classifyIds) {
      const ps = getFinalMatClsById(id).then((res) => {
        const clsSimple = {
          id: res.id,
          name: res.name,
          fullName: res.fullName,
          serialNumber: res.serialNumber, // 编码
          measureUnit: res.measureUnit, // 计量单位
          accountingUnit: res.accountingUnit, // 核算单位
          accountingPrecision: res.accountingPrecision, // 核算单位小数精度
          measurePrecision: res.measurePrecision, // 计量单位小数精度
          outboundUnit: res.outboundUnit, // 出库方式
          basicClass: res.basicClass
        }
        const matCls = {
          ...clsSimple,
          specConfig: res.specConfig.map((sc, ci) => {
            return {
              id: sc.id,
              name: sc.name,
              index: ci,
              list: sc.list.map((v, i) => {
                return {
                  index: i,
                  code: v.code,
                  name: v.name
                }
              })
            }
          }),
          specKV: {}
        }
        classifySpec[id].specList = getSpecList(clsSimple, matCls.specConfig)
        Object.assign(matCls.specKV, arr2obj(classifySpec[id].specList, 'sn'))
        Object.assign(classifySpec[id], matCls)
        Object.assign(classifySpec.specKV, matCls.specKV)
      })
      allInterFace.push(ps)
    }
    await Promise.all(allInterFace)
  }
}

// 获取规格列表（将后端的规格转换为各种常用格式）
function getSpecList(classify, specConfig) {
  if (isBlank(specConfig)) return []
  const specLengthArr = []
  const arrLength = specConfig.reduce((res, cur) => {
    specLengthArr.push(cur.list.length)
    return res * cur.list.length
  }, 1)

  // 创建规格列表
  const arr = new Array(arrLength)
  for (let i = 0; i < arr.length; i++) {
    arr[i] = {
      classify,
      index: new Array(specLengthArr.length),
      arr: new Array(specLengthArr.length)
    }
  }
  // 遍历方式：按顺序将每一个【规格配置】的所有【小规格】推入数组来获得结果
  let kl = arrLength // 单个规格需要遍历几次，起始为数组长度
  let prevLength = 1 // 一套【规格配置】需要重复遍历几次
  for (let i = 0; i < specLengthArr.length; i++) {
    kl = kl / specLengthArr[i] // 除当前【规格配置】的长度，等于单个规格需要遍历的次数
    for (let p = 0; p < prevLength; p++) {
      for (let j = 0; j < specLengthArr[i]; j++) {
        const spec = specConfig[i].list[j] // 当前【小规格】
        for (let k = 0; k < kl; k++) {
          const currentIndex = p * specLengthArr[i] * kl + j * kl + k
          arr[currentIndex].index[i] = spec.index
          arr[currentIndex].arr[i] = spec.name
        }
      }
    }
    prevLength = prevLength * specLengthArr[i]
  }
  arr.forEach((v) => {
    v.spec = v.arr.join('*') // 规格
    // 使用object，以Kay-value的形式存储，不使用map，因为本地缓存无法转换Map
    v.specKV = {}
    v.specNameKV = {}
    v.specArrKV = []
    v.arr.forEach((c, i) => {
      v.specArrKV.push({
        key: specConfig[i].id,
        value: c
      })
      v.specKV[specConfig[i].id] = c
      v.specNameKV[specConfig[i].name] = c
    })
    // TODO:map可删除
    v.specMap = new Map(v.arr.map((c, i) => [specConfig[i].id, c])) // id - value
    v.specNameMap = new Map(v.arr.map((c, i) => [specConfig[i].name, c])) // name - value
    v.sn = v.classify.id + '_' + v.index.join('_') // 唯一编号
  })
  return arr
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
