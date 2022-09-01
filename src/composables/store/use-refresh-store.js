import store from '@/store'
import { isNotBlank } from '@/utils/data-type'

/**
 * 更新缓存
 * @param {array/string} keys 需要更新的缓存
 * @param {boolean} reset 重置加载状态（默认不重置）
 * @returns
 */
export default function useRefreshStore(keys = [], reset = false) {
  if (isNotBlank(keys)) {
    if (typeof keys === 'string') keys = [keys]
    keys.forEach((key) => {
      switch (key) {
        case 'classification': // 物料科目树
          if (reset) store.commit('SET_LOADED', { key: 'matClsTree', loaded: false })
          store.dispatch('config/fetchMatClsTree')
          break
        case 'classificationTree': // 科目树
          if (reset) store.commit('SET_LOADED', { key: 'clsTree', loaded: false })
          store.dispatch('config/fetchClassificationTree')
          break
        case 'wmsConfig': // wms基本配置
          store.dispatch('wms/fetchWmsConfig')
          break
      }
    })
  } else {
    // 更新所有缓存？
  }
}
