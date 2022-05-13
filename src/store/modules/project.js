import { getUserProjects, getUserVisaProjects } from '@/api/contract/project'
import { addRoutes, resetRouter } from '@/router'
import EO from '@enum'
import { projectTypeEnum, projectStatusEnum, TechnologyTypeAllEnum } from '@enum-ms/contract'
import storage from '@/utils/storage'
import { projectsToCascade, projectNameFormatter } from '@/utils/project'
import { isNotBlank, isBlank } from '@data-type/index'
import { allPT } from '@/settings/config'

const projectTypeEnumArr = EO.toArr(projectTypeEnum)

const state = {
  // 项目id
  id: storage.get('projectId'),
  // 当前项目
  curProject: storage.get('curProject'),
  // 当前项目内容（位运算结果）
  curProContentBit: storage.get('curProContentBit'),
  // 当前路由项目类型
  routeProjectType: storage.get('routeProjectType'),
  // 当前项目类型
  projectType: storage.get('projectType') || allPT,
  // 用户项目列表(当前项目类型的项目列表)
  userProjects: [],
  // 用户项目KV K:id, V:项目详情
  userProjectKV: {},
  // 用户项目列表（当前项目类型的项目列表，且处于进行中的状态）
  userProcessProjects: [],
  // 用户项目级联列表(当前项目类型的级联列表)
  userProjectsCascade: [],
  // 用户项目列表Map（key:项目类型,val:项目列表）
  userProjectsMap: {},
  // 用户项目级联列表Map（key:项目类型,val:项目级联列表）
  userProjectsCascadeMap: {},
  // 加载状态
  loaded: false,
  // 用户可签证的项目列表
  userVisaProjects: [],
  // 可签证项目加载状态
  visaLoaded: false,
  // 显示所有
  navbarShowAll: storage.get('navbarShowAll') || false
  // currentProject: storage.get('currentProject') || {}
}

const mutations = {
  SET_LOADED(state, loaded) {
    state.loaded = loaded
  },
  SET_VISA_LOADED(state, loaded) {
    state.visaLoaded = loaded
  },
  SET_PROJECT_ID: (state, id) => {
    state.id = id
    state.curProject = state.userProjectKV[id]
    // 计算项目内容的位运算结果
    state.curProContentBit = state.curProject?.projectContentList.reduce((res, cur) => {
      cur.bit = cur.no || cur?.childrenList?.reduce((cRes, cCur) => {
        return cRes | cCur.no
      }, 0)
      return res | cur.bit
    }, 0)
    // 有围护内容需手动加上折边件
    if (state.curProContentBit > TechnologyTypeAllEnum.STRUCTURE.V) {
      state.curProContentBit = state.curProContentBit | TechnologyTypeAllEnum.BENDING.V
    }
    storage.set('projectId', id)
    storage.set('curProject', state.curProject)
    storage.set('curProContentBit', state.curProContentBit)
  },
  // SET_CURRENT_PROJECT: (state, project) => {
  //   state.currentProject = project
  //   storage.set('currentProject', project)
  // },
  SET_PROJECT_TYPE: (state, type) => {
    state.projectType = type
    storage.set('projectType', type)
  },
  SET_ROUTE_PROJECT_TYPE: (state, type) => {
    state.routeProjectType = type
    storage.set('routeProjectType', type)
  },
  SET_USER_PROJECTS: (state, projects = []) => {
    state.userProjects = projects
    state.userProcessProjects = projects.filter(v => v.status === projectStatusEnum.PROCESS.V)
    state.userProjectKV = {}
    projects.forEach(p => {
      state.userProjectKV[p.id] = p
    })
  },
  SET_USER_PROJECTS_MAP: (state, map) => {
    state.userProjectsMap = map
  },
  SET_USER_PROJECTS_CASCADE_MAP: (state, map) => {
    state.userProjectsCascadeMap = map
  },
  SET_USER_PROJECTS_CASCADE: (state, cascade) => {
    state.userProjectsCascade = cascade
  },
  SET_USER_VISA_PROJECTS: (state, projects) => {
    state.userVisaProjects = projects
  },
  // 设置showAll
  SET_NAVBAR_SHOW_ALL: (state, showAll) => {
    state.navbarShowAll = showAll
    storage.set('navbarShowAll', showAll)
  }
}

const actions = {
  setProjectId({ commit }, id) {
    commit('SET_PROJECT_ID', id)
  },
  // setCurrentProject({ commit }, project) {
  //   commit('SET_CURRENT_PROJECT', project)
  // },
  setRouteProjectByMeta({ commit }, meta) {
    const _projectType = meta && isNotBlank(meta.projectType) ? meta.projectType : undefined
    commit('SET_ROUTE_PROJECT_TYPE', _projectType)
  },
  async fetchUserProjects({ dispatch, commit, state }) {
    commit('SET_LOADED', false)
    const { content: projects = [] } = await getUserProjects()
    const projectsMap = {}
    const projectsCascadeMap = {}

    projectTypeEnumArr && projectTypeEnumArr.forEach(type => {
      projectsMap[type.V] = projects.filter(p => p.projectType === type.V) || []
      projectsCascadeMap[type.V] = projectsToCascade(projectsMap[type.V]) || []
    })
    projectsMap[allPT] = projects || []
    projectsCascadeMap[allPT] = projectsToCascade(projects) || []
    commit('SET_USER_PROJECTS_MAP', projectsMap)
    commit('SET_USER_PROJECTS_CASCADE_MAP', projectsCascadeMap)
    // 根据项目类型选择项目级联
    dispatch('changeProjectType', state.projectType)
  },
  // 项目类型变更
  changeProjectType({ dispatch, commit, state, rootGetters }, type) {
    // TODO: 出现保存为projectType 字符串数组的情况
    if (typeof type !== 'number' || type === null || type === undefined) throw Error('type为null、undefined或number类型')
    const _type = type || allPT
    const _cascade = state.userProjectsCascadeMap[_type]
    const _projects = state.userProjectsMap[_type]
    const ids = _projects && _projects.map(i => i.id) || []
    if (isBlank(ids) || ids.indexOf(state.id) === -1 || isBlank(type)) {
      commit('SET_PROJECT_ID', undefined)
      // commit('SET_CURRENT_PROJECT', {})
    }
    commit('SET_PROJECT_TYPE', _type)
    commit('SET_USER_PROJECTS', _projects)
    commit('SET_USER_PROJECTS_CASCADE', _cascade)
    commit('SET_LOADED', true)
    // 登录进来 未加载过路由不进行处理 TODO:
    if (!rootGetters.permission_routes) return
    resetRouter()
    dispatch('permission/setRoutes', null, { root: true }).then(asyncRoutes => {
      addRoutes(asyncRoutes)
    })
  },
  // 获取可签证项目
  async fetchUserVisaProjects({ commit, state }, params) {
    commit('SET_VISA_LOADED', false)
    const { content: projects = [] } = await getUserVisaProjects(params)
    projects.forEach(p => {
      p.fullName = projectNameFormatter(p, null, false)
    })
    commit('SET_USER_VISA_PROJECTS', projects)
    commit('SET_VISA_LOADED', true)
  }
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
