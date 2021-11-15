import { computed, watch } from 'vue'
import { useStore } from 'vuex'

// 获取人员列表
const useUsers = (loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.config.loaded.users)
  // 未加载则拉取
  if (!store.state.config.loaded.users) {
    store.dispatch('config/fetchUsers')
  }

  // 加载成功回调
  if (loadedCallBack) {
    const monitor = watch(
      loaded,
      (flag) => {
        if (flag) {
          setTimeout(() => {
            loadedCallBack()
            monitor()
          }, 0)
        }
      },
      { immediate: true }
    )
  }

  return {
    users: computed(() => store.state.config.users),
    loaded
  }
}

export default useUsers
