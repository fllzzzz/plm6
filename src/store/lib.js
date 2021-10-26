// 只能在setup中使用或调用
import { computed } from 'vue'
import { useStore } from 'vuex'
import { isNotBlank } from '@data-type/index'

const mapGetters = (getters) => {
  const store = useStore()
  if (isNotBlank(getters)) {
    if (getters instanceof Array) {
      return Object.fromEntries(
        getters.map(
          getter => [getter, computed(() => store.getters[getter])]
        )
      )
    }
    if (typeof getters === 'string') {
      return Object.fromEntries(
        [[getters, computed(() => store.getters[getters])]]
      )
    }
  }

  return Object.fromEntries(
    Object.keys(store.getters).map(
      getter => [getter, computed(() => store.getters[getter])]
    )
  )
}

const mapMutations = () => {
  const store = useStore()
  return Object.fromEntries(
    Object.keys(store._mutations).map(
      mutation => [mutation, value => store.commit(mutation, value)]
    )
  )
}

export { mapGetters, mapMutations }
