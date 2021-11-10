import { getMatClsTree, get as getClassificationTree } from '@/api/config/classification-manage/classification-config'
import { getAll as getDicts } from '@/api/system/dict-detail'
import { getAllUnit } from '@/api/config/main/unit-config'
import { getFactoriesAllSimple } from '@/api/mes/common'
import { getFinalMatClsById } from '@/api/common'
import { unitTypeEnum } from '@enum-ms/common'
import useFormatTree from '@compos/classification/use-format-tree'
import { isBlank } from '@/utils/data-type'
import { arr2obj } from '@/utils/convert/type'

// TODO: 加入接口数据缓存有效时间，避免页面长时间未刷新
const state = {
  clsTree: [], // 科目树
  matClsTree: [], // 物料科目树
  classifySpec: {}, // 科目规格
  dict: {}, // 字典值
  unit: { ALL: [], GROUP: [] }, // 单位列表 ALL，WEIGHT...
  factories: [], // 工厂
  loaded: { // 接口是否加载
    factories: false,
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
  async fetchMarClsSpec({ state }, classifyIds = []) {
    const allInterFace = []
    const classifySpec = state.classifySpec
    for (const id of classifyIds) {
      const ps = getFinalMatClsById(id).then(res => {
        const clsSimple = {
          id: res.id,
          name: res.name,
          fullName: res.fullName
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
          specMap: {}
        }
        classifySpec[id].specList = getSpecList(clsSimple, matCls.specConfig)
        Object.assign(matCls.specMap, arr2obj(classifySpec[id].specList, 'sn'))
        Object.assign(classifySpec[id], matCls)
      })
      allInterFace.push(ps)
    }
    await Promise.all(allInterFace)
  }
}

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
          const currentIndex = p * specLengthArr[i] + j * kl + k
          arr[currentIndex].index[i] = spec.index
          arr[currentIndex].arr[i] = spec.name
        }
      }
    }
    prevLength = prevLength * specLengthArr[i]
  }
  arr.forEach(v => {
    v.spec = v.arr.join('*') // 规格
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
