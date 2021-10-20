const state = {
  // 可取消接口的token
  axiosCancelTokens: {}
}

const mutations = {
  SET_AXIOS_CANCEL_TOKENS: (state, { cancelKey, source }) => {
    state.axiosCancelTokens[cancelKey] = source
  }
}

const actions = {
  setAxiosCancelTokens({ commit }, { cancelKey, source }) {
    commit('SET_AXIOS_CANCEL_TOKENS', { cancelKey, source })
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
