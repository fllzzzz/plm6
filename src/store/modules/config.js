import { getMatClsTree, get as getClassificationTree } from '@/api/config/classification-manage/classification-config'
import { getAll as getDicts } from '@/api/system/dict-detail'
import useFormatTree from '@compos/classification/use-format-tree'

const state = {
  clsTree: [], // 科目树
  matClsTree: [], // 物料科目树
  dict: {} // 字典值
}

const mutations = {
  SET_MAT_CLS_TREE(state, tree) {
    state.matClsTree = tree
  },
  SET_CLS_TREE(state, tree) {
    state.clsTree = tree
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
    return tree
  },
  async fetchClassificationTree({ commit }) {
    const res = await getClassificationTree()
    const tree = useFormatTree(res)
    commit('SET_CLS_TREE', tree)
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
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
