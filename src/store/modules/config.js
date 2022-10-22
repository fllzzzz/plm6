import { getMatClsTree, get as getClassificationTree } from '@/api/config/classification-manage/classification-config'
import { getFinalMatClsById } from '@/api/config/classification-manage/common'
import { getAll as getDicts } from '@/api/system/dict-detail'
import { getAllUnit } from '@/api/config/main/unit-config'
import { getFactoriesAllSimple } from '@/api/mes/common'
import { getUserTree, getRegionalCascade } from '@/api/common'
import { getWorkshopsAllSimple } from '@/api/mes/common'
import { getAllFactoryWorkshopLines } from '@/api/mes/common'
import { getLinesAllSimple } from '@/api/mes/common'
import { getProcessAllSimple } from '@/api/mes/common'
import { getUserAllSimple } from '@/api/common'
import { getDeptAllSimple } from '@/api/common'
import { getSuppliersBrief } from '@/api/common'
import { getTaxRateBrief } from '@/api/config/wms/tax-rate'
import { getCompanyConfig, getLogoConfig } from '@/api/config/main/system-config'
import { getUnclosedRequisitionsBrief } from '@/api/wms/requisitions'
import { getPurchasingPurchaseOrderBrief, getPurchaseOrder } from '@/api/supply-chain/purchase-order'
import { getWarehouseBrief } from '@/api/config/wms/warehouse'
import { getSteelClassifyConfBrief } from '@/api/config/system-config/steel-classic'

import { unitTypeEnum } from '@enum-ms/common'
import { matClsEnum } from '@enum-ms/classification'
import { setEmptyArr2Undefined, tree2list, tree2listForLeaf } from '@/utils/data-type/tree'
import { isBlank, isNotBlank } from '@/utils/data-type'
import { arr2obj } from '@/utils/convert/type'
import { formatClsTree } from '@/utils/system/classification'
import { monomerAll } from '@/api/plan/monomer'
import { get as getChangeReasonConfig } from '@/api/config/system-config/change-reason'
import { get as getSubcontractType } from '@/api/config/project-config/subcontract-config'
import { get as getQualityProblemType } from '@/api/config/project-config/quality-problem-config'
import { get as getVisaReason } from '@/api/config/project-config/visa-reason-config'
import moment from 'moment'

/**
 * TODO: 后期设计配置变更，增加接口加载状态：未加载，加载中，加载完成，加载失败
 * 接口处于“加载中时”等待加载回调，而非调用接口后被自动取消。
 * 相关其他文件 composable/store
 */
// TODO: 加入接口数据缓存有效时间，避免页面长时间未刷新
const state = {
  clsTree: [], // 科目树
  matClsTree: [], // 物料科目树
  rawMatClsTree: [], // 普通物料科目树（不含制成品）
  manufClsTree: [], // 制成品科目树
  matClsList: [], // 材料科目树列表
  rawMatClsList: [], // 原材料科目树列表
  manufClsList: [], // 制成品科目树列表
  rawMatClsKV: {}, // 原材料科目树 kv k：id，v：科目信息
  matClsLeafList: [], // 材料科目树列表 - 只包含叶子节点
  rawMatClsLeafList: [], // 原材料科目树列表 - 只包含叶子节点
  manufClsLeafList: [], // 制成品科目树列表 - 只包含叶子节点

  classifySpec: { specKV: {}}, // 科目规格
  dict: {}, // 字典值
  // 公司信息
  company: {
    companyName: '', // 名称
    companyNo: '', // 编号
    telephone: '', // 电话
    website: '', // 网址
    logo: '' // logo
  },
  unit: { ALL: [], GROUP: [], MAP: new Map(), KS: new Map() }, // 单位列表 ALL，WEIGHT...
  factories: [], // 工厂
  factoryKV: {}, // 工厂id:value 格式
  warehouse: [], // 存储仓库
  workshops: [], // 车间
  productLines: [], // 工厂-车间-生产线
  onlyProductLines: [], // 生产线
  process: [], // 工序
  users: [], // 人员列表
  dept: [], // 部门列表
  userDeptTree: [], // 人员部门树
  regional: [], // 地区
  suppliers: [], // 供应商列表
  supplierKV: {}, // 供应商id:value 格式
  taxRateKV: {}, // 税率列表KV  key:基础分类，value：税率列表
  unclosedRequisitions: [], // 未关闭的申购单
  unclosedPurchaseOrder: [], // 采购中（未完成）的采购合同
  purchaseOrders: [], // 采购合同列表
  purchaseOrderKV: {}, // 采购合同id:value 格式
  monomers: {}, // 单体
  changeReasonConfig: [],
  steelClassifyConf: [], // 钢材材料分类配置
  steelClassifyConfICKV: {}, // 钢材材料分类配置 key: id, value: boundFinalClassifyIds
  subcontractType: [],
  qualityProblemType: [],
  visaReason: [],
  loaded: {
    // 接口是否加载
    company: false,
    factories: false,
    warehouse: false,
    workshops: false,
    productLines: false,
    onlyProductLines: false,
    process: false,
    users: false,
    dept: false,
    unit: false,
    userDeptTree: false,
    matClsTree: false,
    clsTree: false,
    suppliers: false,
    taxRate: false,
    unclosedRequisitions: false,
    unclosedPurchaseOrder: false,
    purchaseOrders: false,
    changeReasonConfig: false,
    steelClassifyConf: false,
    subcontractType: false,
    qualityProblemType: false,
    visaReason: false
  }
}

const mutations = {
  SET_LOADED(state, { key, loaded = true }) {
    state.loaded[key] = loaded
  },
  SET_COMPANY: (state, company = {}) => {
    state.company = Object.assign(state.company, company)
  },
  SET_TAX_RATE(state, list = []) {
    state.taxRateKV = {}
    list.forEach((v) => {
      state.taxRateKV[v.classification] = v.taxRateList
    })
  },
  SET_MAT_CLS_TREE(state, tree = []) {
    state.matClsTree = tree
    state.rawMatClsTree = tree.filter((t) => ![matClsEnum.STRUC_MANUFACTURED.V, matClsEnum.ENCL_MANUFACTURED.V].includes(t.basicClass))
    state.manufClsTree = tree.filter((t) => [matClsEnum.STRUC_MANUFACTURED.V, matClsEnum.ENCL_MANUFACTURED.V].includes(t.basicClass))

    const list = tree2list(tree)
    state.matClsList = list
    state.rawMatClsList = list.filter((t) => ![matClsEnum.STRUC_MANUFACTURED.V, matClsEnum.ENCL_MANUFACTURED.V].includes(t.basicClass))
    state.manufClsList = list.filter((t) => [matClsEnum.STRUC_MANUFACTURED.V, matClsEnum.ENCL_MANUFACTURED.V].includes(t.basicClass))
    state.rawMatClsKV = arr2obj(list, 'id')

    const leafList = tree2listForLeaf(tree)
    state.matClsLeafList = leafList
    state.rawMatClsLeafList = leafList.filter(
      (t) => ![matClsEnum.STRUC_MANUFACTURED.V, matClsEnum.ENCL_MANUFACTURED.V].includes(t.basicClass)
    )
    state.manufClsLeafList = leafList.filter((t) =>
      [matClsEnum.STRUC_MANUFACTURED.V, matClsEnum.ENCL_MANUFACTURED.V].includes(t.basicClass)
    )
  },
  SET_CLS_TREE(state, tree = []) {
    state.clsTree = tree
  },
  SET_UNIT(state, unit) {
    state.unit = unit
  },
  SET_FACTORIES(state, factories = []) {
    state.factories = factories
    // 生产工厂kv
    state.factoryKV = {}
    factories.forEach((v) => {
      state.factoryKV[v.id] = v
    })
  },
  SET_WORKSHOPS(state, workshops) {
    state.workshops = workshops
  },
  SET_WAREHOUSE(state, warehouse) {
    state.warehouse = warehouse
  },
  SET_PRODUCT_LINES(state, productLines) {
    state.productLines = productLines
  },
  SET_ONLY_PRODUCT_LINES(state, onlyProductLines) {
    state.onlyProductLines = onlyProductLines
  },
  SET_PROCESS(state, process) {
    state.process = process
  },
  SET_USERS(state, users) {
    state.users = users
  },
  SET_SUPPLIERS(state, suppliers) {
    state.suppliers = suppliers
    // 供应商kv
    state.supplierKV = {}
    suppliers.forEach((v) => {
      state.supplierKV[v.id] = v
    })
  },
  SET_DEPT(state, dept) {
    state.dept = dept
  },
  SET_USER_DEPT_TREE(state, tree) {
    state.userDeptTree = tree
  },
  SET_REGIONAL(state, regional) {
    state.regional = regional
  },
  SET_UNCLOSED_REQUISITIONS(state, requisitions) {
    state.unclosedRequisitions = requisitions
  },
  SET_UNCLOSED_PURCHASE_ORDER(state, order) {
    state.unclosedPurchaseOrder = order
  },
  SET_PURCHASE_ORDERS(state, purchaseOrders) {
    state.purchaseOrders = purchaseOrders
    state.purchaseOrderKV = {}
    purchaseOrders.forEach((v) => {
      state.purchaseOrderKV[v.id] = v
    })
  },
  SET_CHANGE_REASON_CONFIG(state, changeReasonConfig) {
    state.changeReasonConfig = changeReasonConfig
  },
  SET_STEEL_CLASSIFY_CONF(state, list) {
    state.steelClassifyConf = list
    state.steelClassifyConfKV = {}
    state.steelClassifyConfICKV = {}
    list.forEach((row) => {
      state.steelClassifyConfKV[row.id] = row
      state.steelClassifyConfICKV[row.id] = row.boundFinalClassifyIds
    })
  },
  SET_SUBCONTRACT_TYPE(state, subcontractType) {
    state.subcontractType = subcontractType
  },
  SET_QUALITY_PROBLEM_TYPE(state, qualityProblemType) {
    state.qualityProblemType = qualityProblemType
  },
  SET_VISA_REASON(state, visaReason) {
    state.visaReason = visaReason
  }
}

// 加载配置文件
const actions = {
  fetchConfigInfo() {
    console.log('TODO：加载配置文件')
  },
  // 加载公司信息
  async fetchCompany({ commit }) {
    // 基本信息
    const res = (await getCompanyConfig()) || {}
    commit('SET_COMPANY', res)
    // logo
    const { content = [] } = await getLogoConfig()
    const index = content.findIndex((v) => v.isDefault)
    if (index !== -1) {
      commit('SET_COMPANY', { logo: content[index]?.path })
    }
  },
  // 设置公司信息
  setCompany({ commit }, data = {}) {
    commit('SET_COMPANY', data)
  },
  // 加载税率列表
  async fetchTaxRate({ commit }) {
    const { content = [] } = await getTaxRateBrief()
    commit('SET_TAX_RATE', content)
    commit('SET_LOADED', { key: 'taxRate' })
    return content
  },
  // 物料分类树
  async fetchMatClsTree({ commit }) {
    const res = await getMatClsTree()
    const tree = formatClsTree(res)
    commit('SET_MAT_CLS_TREE', tree)
    commit('SET_LOADED', { key: 'matClsTree' })
    return tree
  },
  // 分类树
  async fetchClassificationTree({ commit }) {
    const res = await getClassificationTree()
    const tree = formatClsTree(res)
    commit('SET_CLS_TREE', tree)
    commit('SET_LOADED', { key: 'clsTree' })
    return tree
  },
  // 加载字典值
  async fetchDict({ state }, names = []) {
    const allInterFace = []
    const dict = state.dict
    if (!('dict' in dict)) {
      dict.dict = {}
    }
    if (!('label' in dict)) {
      dict.label = {}
    }
    for (const name of names) {
      const ps = getDicts(name).then((res) => {
        const content = res.content || []
        dict[name] = [...content]
        dict.dict[name] = {}
        dict.label[name] = {}
        content.forEach((v) => {
          dict.dict[name][v.value] = v
          dict.label[name][v.value] = v.label
        })
      })
      allInterFace.push(ps)
    }
    await Promise.all(allInterFace)
  },
  // 加载单位
  async fetchUnit({ commit }) {
    const res = (await getAllUnit()) || []
    // 单位分为分类列表与全单位列表
    const unit = { ALL: [], GROUP: [], MAP: new Map(), KS: new Map() }
    Object.keys(unitTypeEnum.ENUM).forEach((key) => {
      unit[key] = []
    })
    res.forEach((v) => {
      const n = {
        id: v.id,
        name: v.name,
        type: v.type,
        symbol: v.symbol,
        enabled: v.enabled
      }
      unit.ALL.push(n)
      unit.MAP.set(n.name, n)
      if (n.symbol) {
        // TODO: 多符号问题处理
        // 如果存在符号，则符号也设置进map
        unit.MAP.set(n.symbol, n)
      }
      unit.KS.set(n.name, n.symbol || n.name)
      unit[unitTypeEnum.VK[n.type]].push(n)
    })
    Object.keys(unitTypeEnum.ENUM).forEach((key) => {
      unit.GROUP.push({
        name: unitTypeEnum[key].L,
        type: key,
        options: unit[key]
      })
    })
    // 可以通过名称获取
    unit.symbol = (name) => unit.KS.get(name)
    commit('SET_UNIT', unit)
    commit('SET_LOADED', { key: 'unit' })
  },
  // 工厂
  async fetchFactories({ commit }) {
    const { content = [] } = await getFactoriesAllSimple()
    commit('SET_FACTORIES', content)
    commit('SET_LOADED', { key: 'factories' })
    return content
  },
  // 仓库
  async fetchWarehouse({ commit }) {
    const content = (await getWarehouseBrief()) || []
    commit('SET_WAREHOUSE', content)
    commit('SET_LOADED', { key: 'warehouse' })
    return content
  },
  async fetchWorkshops({ commit }) {
    const { content = [] } = await getWorkshopsAllSimple()
    commit('SET_WORKSHOPS', content)
    commit('SET_LOADED', { key: 'workshops' })
    return content
  },
  // 生产线
  async fetchProductLines({ commit }) {
    const { content = [] } = await getAllFactoryWorkshopLines()
    commit('SET_PRODUCT_LINES', content)
    commit('SET_LOADED', { key: 'productLines', loaded: true })
    return content
  },
  // 生产线
  async fetchOnlyProductLines({ commit }) {
    const { content = [] } = await getLinesAllSimple()
    commit('SET_ONLY_PRODUCT_LINES', content)
    commit('SET_LOADED', { key: 'onlyProductLines', loaded: true })
    return content
  },
  // 工序
  async fetchProcess({ commit }) {
    const content = await getProcessAllSimple() || []
    commit('SET_PROCESS', content)
    commit('SET_LOADED', { key: 'process' })
    return content
  },
  // 用户
  async fetchUsers({ commit }) {
    const { content = [] } = await getUserAllSimple()
    commit('SET_USERS', content)
    commit('SET_LOADED', { key: 'users' })
    return content
  },
  // 供应商
  async fetchSuppliers({ commit }) {
    const { content = [] } = await getSuppliersBrief()
    commit('SET_SUPPLIERS', content)
    commit('SET_LOADED', { key: 'suppliers' })
    return content
  },
  // 部门
  async fetchDept({ commit }) {
    const dept = await getDeptAllSimple()
    setEmptyArr2Undefined(dept)
    commit('SET_DEPT', dept)
    commit('SET_LOADED', { key: 'dept' })
    return dept
  },
  // 用户部门树
  async fetchUserDeptTree({ commit }) {
    const content = await getUserTree()
    const tree = content
    setEmptyArr2Undefined(tree)
    commit('SET_USER_DEPT_TREE', tree)
    commit('SET_LOADED', { key: 'userDeptTree' })
    return tree
  },
  // 省市区
  async fetchRegional({ commit }) {
    const regional = (await getRegionalCascade()) || []
    setEmptyArr2Undefined(regional)
    commit('SET_REGIONAL', regional)
    commit('SET_LOADED', { key: 'regional' })
    return regional
  },
  // 加载未关闭的申购单
  async fetchUnclosedRequisitions({ commit }) {
    const { content = [] } = await getUnclosedRequisitionsBrief()
    commit('SET_UNCLOSED_REQUISITIONS', content)
    commit('SET_LOADED', { key: 'unclosedRequisitions' })
    return content
  },
  // 加载未关闭的申购单
  async fetchUnclosedPurchaseOrder({ commit }) {
    const { content = [] } = await getPurchasingPurchaseOrderBrief()
    content.forEach((v) => {
      if (v.projects) v.projectIds = v.projects.map((v) => v.id)
    })
    commit('SET_UNCLOSED_PURCHASE_ORDER', content)
    commit('SET_LOADED', { key: 'unclosedPurchaseOrder' })
    return content
  },
  // 加载全部采购合同
  async fetchPurchaseOrder({ commit }) {
    const { content = [] } = await getPurchaseOrder()
    // 倒叙
    commit('SET_PURCHASE_ORDERS', content.reverse().map(v => {
      // 创建年份
      v.createYear = moment(v.createTime).format('YYYY')
      return v
    }))
    commit('SET_LOADED', { key: 'purchaseOrders' })
    return content
  },
  async fetchSteelClassifyConf({ commit }) {
    const { content = [] } = await getSteelClassifyConfBrief()
    commit('SET_STEEL_CLASSIFY_CONF', content)
    commit('SET_LOADED', { key: 'steelClassifyConf' })
    return content
  },
  // 原材料规格
  async fetchMarClsSpec({ state }, classifyIds = []) {
    const allInterFace = []
    const classifySpec = state.classifySpec
    for (const id of classifyIds) {
      const ps = getFinalMatClsById(id).then((res) => {
        const clsBrief = {
          // 简要信息
          id: id, // 科目id
          name: res.name, // 名称
          serialNumber: res.serialNumber, // 编码
          measureUnit: res.measureUnit, // 计量单位
          accountingUnit: res.accountingUnit, // 核算单位
          accountingPrecision: res.accountingPrecision || 0, // 核算单位小数精度
          measurePrecision: res.measurePrecision || 0, // 计量单位小数精度
          outboundUnitType: res.outboundUnitType, // 出库方式
          basicClass: res.basicClass, // 基础分类
          hasUnitConfig: !!res.accountingUnit
        }
        clsBrief.fullPathId = res.fullPathId // 全路径id
        clsBrief.fullPathName = res.fullName.split('>') // 全称全路径 数组
        clsBrief.fullName = clsBrief.fullPathName.join(' > ') // 全称
        clsBrief.name = clsBrief.fullPathName[clsBrief.fullPathName.length - 1]
        clsBrief.parentPathName = clsBrief.fullPathName.slice(0, -1) // 路径
        clsBrief.parentFullName = clsBrief.parentPathName.join(' > ') // 全称

        // 型材处理
        if (res.basicClass === matClsEnum.SECTION_STEEL.V) {
          const nationalStandard = res.nationalStandard || []
          nationalStandard.forEach((std) => {
            const matCls = {
              ...clsBrief,
              specConfig: [],
              specKV: {}
            }
            if (isNotBlank(res.specConfig)) {
              matCls.specConfig = res.specConfig.map((sc, ci) => {
                return {
                  id: sc.id,
                  name: sc.name,
                  index: ci + 1,
                  list: sc.list.map((v, i) => {
                    const spec = {
                      index: i,
                      code: v.code,
                      name: v.name
                    }
                    return spec
                  })
                }
              })
            }

            matCls.specConfig.unshift({
              id: `standard_${std.id}`,
              name: std.name,
              index: 0,
              list: std.list.map((v, i) => {
                const spec = {
                  index: i,
                  code: v.code || v.id, // 型材国标编码，没有code使用id
                  name: v.name,
                  // 型材国标加入单位净重
                  unitWeight: v.unitWeight,
                  boolStandard: true // 是否国标
                }
                return spec
              })
            })

            const key = `${id}_${std.name}`
            if (std.boolDefault) {
              // classifySpec[id] 已在compos中创建对象
              classifySpec[key] = classifySpec[id]
            } else {
              classifySpec[key] = {}
            }
            classifySpec[key].fullSpecKV = new Map()
            classifySpec[key].specList = getSpecList(clsBrief, matCls.specConfig)
            // fullSpecMap：可通过classifyId及“全规格（例：M27*60）”，获取规格的详细信息
            classifySpec[key].fullSpecMap = new Map(classifySpec[key].specList.map((v) => [v.spec, v]))
            Object.assign(matCls.specKV, arr2obj(classifySpec[key].specList, 'sn'))
            Object.assign(classifySpec[key], matCls)
            Object.assign(classifySpec.specKV, matCls.specKV)
          })
        } else {
          const matCls = {
            ...clsBrief,
            specConfig: [],
            specKV: {}
          }
          if (isNotBlank(res.specConfig)) {
            matCls.specConfig = res.specConfig.map((sc, ci) => {
              return {
                id: sc.id,
                name: sc.name,
                index: ci,
                list: sc.list.map((v, i) => {
                  const spec = {
                    index: i,
                    code: v.code,
                    name: v.name
                  }
                  return spec
                })
              }
            })
          }
          classifySpec[id].fullSpecKV = new Map()
          classifySpec[id].specList = getSpecList(clsBrief, matCls.specConfig)
          // fullSpecMap：可通过classifyId及“全规格（例：M27*60）”，获取规格的详细信息
          classifySpec[id].fullSpecMap = new Map(classifySpec[id].specList.map((v) => [v.spec, v]))
          Object.assign(matCls.specKV, arr2obj(classifySpec[id].specList, 'sn'))
          Object.assign(classifySpec[id], matCls)
          Object.assign(classifySpec.specKV, matCls.specKV)
        }
      })
      allInterFace.push(ps)
    }
    await Promise.all(allInterFace)
  },
  // 单体
  async fetchMonomer({ state }, projectId) {
    const monomers = state.monomers
    const { content = [] } = await monomerAll(projectId)
    monomers[projectId] = content
  },
  // 变更原因
  async fetchChangeReasonConfig({ commit }) {
    const { content = [] } = await getChangeReasonConfig()
    commit('SET_CHANGE_REASON_CONFIG', content)
    commit('SET_LOADED', { key: 'changeReasonConfig' })
    return content
  },
  // 分包类别
  async fetchSubcontractType({ commit }) {
    const { content = [] } = await getSubcontractType()
    commit('SET_SUBCONTRACT_TYPE', content)
    commit('SET_LOADED', { key: 'subcontractType' })
    return content
  },
  // 质安问题分类
  async fetchQualityProblemType({ commit }) {
    const { content = [] } = await getQualityProblemType()
    commit('SET_QUALITY_PROBLEM_TYPE', content)
    commit('SET_LOADED', { key: 'qualityProblemType' })
    return content
  },
  // 签证原因
  async fetchVisaReason({ commit }) {
    const { content = [] } = await getVisaReason()
    commit('SET_VISA_REASON', content)
    commit('SET_LOADED', { key: 'visaReason' })
    return content
  }
}

// 获取规格列表（将后端的规格转换为各种常用格式）
function getSpecList(classify, specConfig) {
  if (isBlank(specConfig)) {
    // 为空则插入无规格选项
    return [
      {
        classify,
        index: [0],
        specificationLabels: '无规格',
        arr: [],
        spec: '',
        specKV: {},
        specNameKV: {},
        specArrKV: [],
        serialNumber: classify.serialNumber,
        // sn: classify.id + '_' + '-1'
        sn: classify.serialNumber
      }
    ]
  }
  const specLengthArr = []
  const arrLength = specConfig.reduce((res, cur) => {
    specLengthArr.push(cur.list.length)
    return res * cur.list.length
  }, 1)

  // 创建规格列表
  const arr = new Array(arrLength)
  for (let i = 0; i < arr.length; i++) {
    arr[i] = {
      classify,
      index: new Array(specLengthArr.length),
      arr: new Array(specLengthArr.length),
      code: new Array(specLengthArr.length)
    }
  }
  // 遍历方式：按顺序将每一个【规格配置】的所有【小规格】推入数组来获得结果
  let kl = arrLength // 单个规格需要遍历几次，起始为数组长度
  let prevLength = 1 // 一套【规格配置】需要重复遍历几次
  for (let i = 0; i < specLengthArr.length; i++) {
    kl = kl / specLengthArr[i] // 除当前【规格配置】的长度，等于单个规格需要遍历的次数
    for (let p = 0; p < prevLength; p++) {
      for (let j = 0; j < specLengthArr[i]; j++) {
        const spec = specConfig[i].list[j] // 当前【小规格】
        for (let k = 0; k < kl; k++) {
          const currentIndex = p * specLengthArr[i] * kl + j * kl + k
          arr[currentIndex].index[i] = spec.index
          arr[currentIndex].arr[i] = spec.name
          arr[currentIndex].code[i] = spec.code
          if (classify.basicClass === matClsEnum.SECTION_STEEL.V && spec.boolStandard) {
            arr[currentIndex].unitWeight = spec.unitWeight
            arr[currentIndex].boolStandard = spec.boolStandard
          }
        }
      }
    }
    prevLength = prevLength * specLengthArr[i]
  }
  arr.forEach((v) => {
    // 唯一编号
    v.serialNumber = classify.serialNumber + '-' + v.code.join('') || void 0
    v.spec = v.arr.join(' * ') // 规格
    // 使用object，以Kay-value的形式存储，不使用map，因为本地缓存无法转换Map
    v.specKV = {} // 例：key: 材质id ， val: 'Q235B'
    v.specificationLabels = specConfig.map((v) => v.name).join(' * ')
    v.specNameKV = {} // 例：key: 材质 ， val: 'Q235B'
    v.specArrKV = []
    v.arr.forEach((c, i) => {
      v.specArrKV.push({
        key: specConfig[i].id,
        value: c
      })
      v.specKV[specConfig[i].id] = c
      v.specNameKV[specConfig[i].name] = c
    })
    // TODO:map可删除
    v.specMap = new Map(v.arr.map((c, i) => [specConfig[i].id, c])) // id - value
    v.specNameMap = new Map(v.arr.map((c, i) => [specConfig[i].name, c])) // name - value
    v.sn = v.serialNumber // 唯一编号
    // v.sn = v.classify.id + '_' + v.index.join('_') // 唯一编号
    // TODO:之所以使用index作为唯一标识，而不使用规格的code最后拼接的serialNumber, 是因为一开始未设计code
  })
  return arr
}

export default {
  namespaced: true,
  state,
  mutations,
  actions
}
