import { getMatClsTree, get as getClassificationTree } from '@/api/config/classification-manage/classification-config'
import useFormatTree from '@compos/classification/use-format-tree'

const state = {
  clsTree: [],
  matClsTree: []
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
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
