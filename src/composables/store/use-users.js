import { computed } from 'vue'
import { useStore } from 'vuex'

// 获取人员列表
const useUsers = () => {
  const store = useStore()
  // 未加载则拉取
  if (!store.state.config.loaded.users) {
    store.dispatch('config/fetchUsers')
  }
  return {
    users: computed(() => store.state.config.users),
    loaded: computed(() => store.state.config.loaded.users)
  }
}

export default useUsers
