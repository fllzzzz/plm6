import { computed, watch } from 'vue'
import { useStore } from 'vuex'
import { businessTypeEnum } from '@enum-ms/contract'

// 获取可签证项目
const useUserVisaProjects = (params = { businessType: businessTypeEnum.MACHINING.V }, loadedCallBack) => {
  const store = useStore()
  const loaded = computed(() => store.state.project.visaLoaded)
  // 未加载则拉取
  if (!loaded.value) {
    store.dispatch('project/fetchUserVisaProjects', params)
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
    loaded,
    visaProjects: computed(() => store.state.project.userVisaProjects)
  }
}

export default useUserVisaProjects
