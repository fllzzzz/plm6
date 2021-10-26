import { getMatClsTree } from '@/api/config/classification-manage/classification-config'
import useFormatTree from '@compos/classification/use-format-tree'
const state = {
  matClsTree: []
}

const mutations = {
  SET_MAT_CLS_TREE(state, tree) {
    state.matClsTree = tree
  }
}

// 加载配置文件
const actions = {
  fetchConfigInfo() {
    console.log('TODO：加载配置文件')
  },
  // 加载分类
  async fetchMatClsTree({ commit }) {
    const { content = [] } = await getMatClsTree()
    const tree = useFormatTree(content)
    commit('SET_MAT_CLS_TREE', tree)
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
