import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取人员部门列表
const userDeptTree = () => {
  const store = useStore()
  // 未加载则拉取
  if (!store.state.config.loaded.userDeptTree) {
    store.dispatch('config/fetchUserDeptTree')
  }
  return {
    userDeptTree: computed(() => store.state.config.userDeptTree),
    loaded: computed(() => store.state.config.loaded.userDeptTree)
  }
}

export default userDeptTree
