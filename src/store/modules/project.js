import { addRoutes, resetRouter } from '@/router'
import storage from '@/utils/storage'
import { projectsToCascade } from '@/utils/project'
import { isNotBlank, isBlank } from '@data-type/index'
import EO from '@enum'
import { projectTypeEnum } from '@enum-ms/contract'

const allProjectTypes = EO.getBitsSum(projectTypeEnum)
const projectTypeEnumArr = EO.toArr(projectTypeEnum)

const state = {
  // 项目id
  id: storage.get('projectId'),
  // 当前路由项目类型
  routeProjectType: storage.get('routeProjectType'),
  // 当前项目类型
  projectType: storage.get('projectType') || allProjectTypes,
  // 项目列表(当前项目类型的项目列表)
  projects: [],
  // 项目级联列表(当前项目类型的级联列表)
  projectsCascade: [],
  // 项目列表Map（key:项目类型,val:项目列表）
  projectsMap: {},
  // 项目级联列表Map（key:项目类型,val:项目级联列表）
  projectsCascadeMap: {}
}

const mutations = {
  SET_PROJECT_ID: (state, id) => {
    state.id = id
    storage.set('projectId', id)
  },
  SET_PROJECT_TYPE: (state, type) => {
    state.projectType = type
    storage.set('projectType', type)
  },
  SET_ROUTE_PROJECT_TYPE: (state, type) => {
    state.routeProjectType = type
    storage.set('routeProjectType', type)
  },
  SET_PROJECTS: (state, projects) => {
    state.projects = projects
  },
  SET_PROJECTS_MAP: (state, map) => {
    state.projectsMap = map
  },
  SET_PROJECTS_CASCADE_MAP: (state, map) => {
    state.projectsCascadeMap = map
  },
  SET_PROJECTS_CASCADE: (state, cascade) => {
    state.projectsCascade = cascade
  }
}

const actions = {
  setProjectId({ commit }, id) {
    commit('SET_PROJECT_ID', id)
  },
  setRouteProjectMeta({ commit }, meta) {
    const _projectType = meta && isNotBlank(meta.projectType) ? meta.projectType : undefined
    commit('SET_ROUTE_PROJECT_TYPE', _projectType)
  },
  setProjectsCascade({ commit }, projects) {
    commit('SET_PROJECTS_CASCADE', projects)
  },
  setProjects({ dispatch, commit, state }, projects) {
    const projectsMap = {}
    const projectsCascadeMap = {}
    projectTypeEnumArr && projectTypeEnumArr.forEach(type => {
      projectsMap[type.V] = projects.filter(p => p.projectType === type.V) || []
      projectsCascadeMap[type.V] = projectsToCascade(projectsMap[type.V]) || []
    })
    projectsMap[0] = projects || []
    projectsCascadeMap[0] = projectsToCascade(projects) || []
    commit('SET_PROJECTS_MAP', projectsMap)
    commit('SET_PROJECTS_CASCADE_MAP', projectsCascadeMap)
    // 根据项目类型选择项目级联
    dispatch('changeProjectType', state.projectType)
  },
  // 项目类型变更
  changeProjectType({ dispatch, commit, state, rootGetters }, type) {
    const _type = type || allProjectTypes
    const _cascade = state.projectsCascadeMap[_type]
    const _projects = state.projectsMap[_type]
    const ids = _projects && _projects.map(i => i.id) || []
    if (isBlank(ids) || ids.indexOf(state.id) === -1 || isBlank(type)) {
      commit('SET_PROJECT_ID', undefined)
    }
    commit('SET_PROJECT_TYPE', _type)
    commit('SET_PROJECTS', _projects)
    commit('SET_PROJECTS_CASCADE', _cascade)
    // 登录进来 未加载过路由不进行处理 TODO:
    if (!rootGetters.permission_routes) return
    resetRouter()
    dispatch('permission/setRoutes', null, { root: true }).then(asyncRoutes => {
      addRoutes(asyncRoutes)
    })
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
