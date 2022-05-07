import { apiVersion } from '@/settings/api'
import { validURI } from '@/utils/validate'
import { getRequestUrl } from '@/utils/storage'

const state = {
  // // 修改头像
  // updateAvatarApi: lStore.get('requestUrl') + '/api/user/updateAvatar',
  // // 文件上传
  fileUploadApi: getRequestUrl() + '/api/common/attachment',
  // 上传公司logo
  fileUploadCompanyLogoApi: getRequestUrl() + '/api/config/company/logo',
  // // 材料清单上传
  // materialListUploadApi: lStore.get('requestUrl') + '/api/material/listUpload',
  // // 材料变更清单上传
  // materialChangeListUploadApi: lStore.get('requestUrl') + '/api/material/changeListUpload',
  // baseUrl，
  baseApi: {
    // contract: `/api/contract/v${apiVersion.contract}/`,
    // user: `/api/user/v${apiVersion.user}/`,
    // wms: `/api/wms/v${apiVersion.wms}/`,
    // common: `/api/common/v${apiVersion.common}/`,
    // config: `/api/config/v${apiVersion.config}/`
    contract: `/api/`,
    plan: `/api/plan/`,
    user: `/api/`,
    wms: `/api/wms/`,
    common: `/api/common/`,
    config: `/api/config/`,
    project: `/api/project/`,
    mes: `/api/mes/building/`,
    system: `/api/`,
    cost: `/api/`,
    scm: `/api/scm/`
  }
}

const mutations = {
  SET_BASE_API: (state, {
    baseUrl,
    apiVersion
  }) => {
    state.baseApi = {
      user: `/api/user/v${apiVersion.user}`,
      contract: `/api/contract/v${apiVersion.contract}`,
      project: `/api/project/v${apiVersion.common}`,
      wms: `/api/wms/v${apiVersion.wms}`,
      scm: `/api/wms/v${apiVersion.scm}`,
      common: `/api/common/v${apiVersion.common}`
    }
  }
}

const actions = {
  /**
   * TODO:设置访问api
   * 具体如何实现需要看服务端
   * @param {*} param0
   */
  setAPI({
    commit,
    state
  }) {
    return new Promise((resolve, reject) => {
      const requestUrl = getRequestUrl()
      if (requestUrl || validURI(requestUrl)) {
        commit('SET_BASE_API', {
          baseUrl: getRequestUrl(),
          apiVersion: apiVersion
        })
        resolve()
      } else {
        reject()
      }
    })
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
