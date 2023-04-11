<template>
  <div v-permission="permission" class="steel-inbound-application-container">
    <common-wrapper
      :basic-class="STEEL_ENUM"
      :current-basic-class="steelBasicClassKV[currentBasicClass]?.V"
      :total-value="totalWeight"
      :total-amount="totalAmount"
      :show-total-amount="!boolPartyA && fillableAmount"
      :contract-unit="order?.meteUnit"
      :contract-value="order?.mete"
      :showContractTotal="!boolPartyA && !!form.purchaseId"
      :validate="validate"
      :edit="props.edit"
      unit="kg"
      :total-name="!boolPartyA ? '本次实收' : '总量合计'"
      @purchase-order-change="handleOrderInfoChange"
    >
      <div class="filter-container">
        <div class="filter-left-box">
          <el-radio-group v-model="currentBasicClass" size="small" class="filter-item">
            <el-radio-button v-for="item in steelBasicClassKV" :key="item.K" :label="item.K" :disabled="disabledBasicClass[item.K]">
              {{ item.L }}{{ getNum(item.K) ? `(${getNum(item.K)})` : '' }}
            </el-radio-button>
          </el-radio-group>
        </div>
        <div class="filter-right-box">
          <common-button
            v-if="weightAssignable"
            class="filter-item"
            type="warning"
            @click="automaticAssignWeight"
            :disabled="!currentBasicClass"
          >
            按过磅重量自动分配（钢卷不参与分配)
          </common-button>
          <el-tooltip :disabled="addable" effect="light" content="请先选择采购合同编号" placement="left-start">
            <span>
              <common-button
                v-if="boolPartyA"
                class="filter-item"
                type="success"
                @click="materialSelectVisible = true"
                :disabled="!addable"
              >
                添加物料
              </common-button>
            </span>
          </el-tooltip>
        </div>
      </div>
      <el-form ref="formRef" :model="form">
        <component
          ref="steelRef"
          :max-height="tableMaxHeight"
          :style="maxHeightStyle"
          :is="comp"
          :bool-party-a="boolPartyA"
          :fillableAmount="fillableAmount"
        />
      </el-form>
    </common-wrapper>
    <common-drawer
      ref="drawerRef"
      v-model="materialSelectVisible"
      title="物料选择"
      :show-close="true"
      :size="900"
      custom-class="material-table-spec-select"
    >
      <template #content>
        <material-table-spec-select
          v-if="addable"
          ref="matSpecRef"
          v-model="list"
          :visible="materialSelectVisible"
          :row-init-fn="rowInit"
          :max-height="specSelectMaxHeight"
          :basic-class="steelBasicClassKV[currentBasicClass].V"
          :table-width="350"
          auto-selected
          expand-query
        />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { steelInboundApplication } from '@/api/wms/material-inbound/raw-material/application'
import { edit as editInboundApplication } from '@/api/wms/material-inbound/raw-material/record'
import { steelInboundApplicationPM as permission } from '@/page-permission/wms'

import { toPrecision } from '@/utils/data-type'
import { createUniqueString } from '@/utils/data-type/string'
import { defineProps, defineEmits, ref, computed, watch, provide, nextTick, reactive } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { weightMeasurementModeEnum } from '@/utils/enum/modules/finance'
import { orderSupplyTypeEnum } from '@/utils/enum/modules/wms'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
// import useWmsConfig from '@/composables/store/use-wms-config'
import commonWrapper from '@/views/wms/material-inbound/raw-material/application/components/common-wrapper.vue'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import steelPlateTable from './module/steel-plate-table.vue'
import sectionSteelTable from './module/section-steel-table.vue'
import steelCoilTable from './module/steel-coil-table.vue'
import { ElMessage, ElRadioGroup } from 'element-plus'
import { isBlank, isNotBlank, toFixed } from '@/utils/data-type'
import { steelInboundFormFormat } from '@/utils/wms/measurement-calc'

const emit = defineEmits(['success'])

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  detail: {
    type: Object
  }
})

// 基础分类
const steelBasicClassKV = {
  steelPlateList: { K: 'steelPlateList', L: '钢板', V: matClsEnum.STEEL_PLATE.V, TABLE: steelPlateTable },
  sectionSteelList: { K: 'sectionSteelList', L: '型材', V: matClsEnum.SECTION_STEEL.V, TABLE: sectionSteelTable },
  steelCoilList: { K: 'steelCoilList', L: '钢卷', V: matClsEnum.STEEL_COIL.V, TABLE: steelCoilTable }
}

const defaultForm = {
  purchaseId: null, // 采购合同id
  loadingWeight: null, // 装载重量
  licensePlate: null, // 车牌号
  shipmentNumber: null, // 物流单号
  logistics: {}, // 物流信息
  list: [], // 钢材列表，提交时合并
  steelPlateList: [], // 钢板列表
  sectionSteelList: [], // 型材列表
  steelCoilList: [] // 钢卷列表
}

const steelRef = ref() // 表格ref
const matSpecRef = ref() // 规格列表ref
const formRef = ref() // form表单ref
const drawerRef = ref()
const order = ref() // 订单信息
const orderLoaded = ref(false) // 订单加载状态
const disabledBasicClass = ref({}) // 禁用的基础分类
const materialSelectVisible = ref(false) // 显示物料选择
const currentBasicClass = ref() // 当前基础分类
const list = ref([]) // 当前操作的表格list
const boolPartyA = ref(false) // 是否“甲供”

// 钢材三个组件的ref列表
const steelRefList = reactive({
  steelPlateList: null,
  sectionSteelList: null,
  steelCoilList: null
})

const { baseUnit } = useMatBaseUnit() // 当前分类基础单位

// const { inboundFillWayCfg } = useWmsConfig()
// 显示金额相关信息（由采购填写的信息）
const fillableAmount = computed(
  () => false
  // inboundFillWayCfg.value ? inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.APPLICATION.V : false
)

const addable = computed(() => !!(currentBasicClass.value && order.value)) // 可添加的状态（选择了采购合同编号）

function getNum(key) {
  if (boolPartyA.value) {
    return form[key]?.length || 0
  } else {
    return form[key]?.filter((v) => form.selectObj[v.mergeId]?.isSelected)?.length || 0
  }
}

// 列表汇总数据
const formList = computed(() => {
  const list = []
  if (isNotBlank(form.steelPlateList)) {
    form.steelPlateList.forEach((v) => {
      if (boolPartyA.value || form.selectObj[v.mergeId]?.isSelected) {
        list.push(v)
      }
    })
  }
  if (isNotBlank(form.sectionSteelList)) {
    form.sectionSteelList.forEach((v) => {
      if (boolPartyA.value || form.selectObj[v.mergeId]?.isSelected) {
        list.push(v)
      }
    })
  }
  if (isNotBlank(form.steelCoilList)) {
    form.steelCoilList.forEach((v) => {
      if (boolPartyA.value || form.selectObj[v.mergeId]?.isSelected) {
        list.push(v)
      }
    })
  }
  return list
})

// 总价
const totalAmount = computed(() => {
  let amount = 0
  if (!boolPartyA.value && fillableAmount.value) {
    formList.value.forEach((v) => {
      if (isNotBlank(v.amount)) {
        amount += +v.amount
      }
    })
  }
  return toFixed(amount, 2)
})

// 总重
const totalWeight = computed(() => {
  let weight = 0
  formList.value.forEach((v) => {
    if (isNotBlank(v.weighingTotalWeight)) {
      weight += +v.weighingTotalWeight
    }
  })
  return toFixed(weight, 2)
})

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

// 使用草稿时，为数据设置监听
const setFormCallback = (form) => {
  const trigger = {
    steelPlateList: null,
    sectionSteelList: null,
    steelCoilList: null
  }
  const initSelectedTrigger = {
    steelPlateList: null,
    sectionSteelList: null,
    steelCoilList: null
  }
  const list = ['steelPlateList', 'sectionSteelList', 'steelCoilList']
  let hasDefaultSelect = false // 是否有默认选中，优先选中有数据的类型
  list.forEach((key) => {
    if (isNotBlank(form[key])) {
      if (!hasDefaultSelect) {
        hasDefaultSelect = true
        // 等待订单加载后选中
        const orderTrigger = watch(
          orderLoaded,
          (loaded) => {
            if (loaded) {
              nextTick(() => {
                currentBasicClass.value = key
                orderTrigger()
              })
            }
          },
          { immediate: true }
        )
      }

      form[key] = form[key].map((v) => reactive(v))
      trigger[key] = watch(
        steelRefList,
        (ref) => {
          if (ref[key]) {
            // 初始化数据监听，执行一次后取消当前监听
            form[key].forEach((v) => {
              ref[key].rowWatch(v)
              if (!boolPartyA.value && form.selectObj?.[v.mergeId]?.isSelected) {
                ref[key].toggleRowSelection(v, true)
              }
            })
            // 初始化选中数据，执行一次后取消当前监听
            initSelectedTrigger[key] = watch(
              matSpecRef,
              () => {
                if (matSpecRef.value) {
                  matSpecRef.value.initSelected(
                    form[key].map((v) => {
                      return { sn: v.sn, classifyId: v.classifyId }
                    })
                  )
                  nextTick(() => {
                    initSelectedTrigger[key]()
                  })
                }
              },
              { immediate: true }
            )
            nextTick(() => {
              trigger[key]()
            })
          }
        },
        { immediate: true, deep: true }
      )
    }
  })
  fixMaxHeight()
}

const { cu, form, FORM } = useForm(
  {
    title: '钢材入库',
    formStore: !props.edit,
    formStoreKey: 'WMS_INBOUND_APPLICATION_STEEL',
    permission: permission,
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? editInboundApplication : steelInboundApplication
  },
  formRef,
  props.detail
)

// 物料选择组件高度
const { maxHeight: specSelectMaxHeight } = useMaxHeight(
  {
    mainBox: '.material-table-spec-select',
    extraBox: '.el-drawer__header',
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

let tableHeightConfig = {}
if (props.edit) {
  // 修改时，是drawer弹窗
  tableHeightConfig = {
    mainBox: '.raw-mat-inbound-application-record-form',
    extraBox: ['.el-drawer__header', '.filter-container', '.inbound-application-header', '.inbound-application-footer'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 300,
    extraHeight: 20
  }
} else {
  // 非修改时
  tableHeightConfig = {
    mainBox: '.steel-inbound-application-container',
    extraBox: ['.filter-container', '.inbound-application-header', '.inbound-application-footer'],
    navbar: true,
    minHeight: 300,
    extraHeight: 20
  }
}
const { fixMaxHeight, maxHeight: tableMaxHeight, maxHeightStyle } = useMaxHeight(tableHeightConfig)

// 当前使用的组件
const comp = computed(() => {
  return currentBasicClass.value ? steelBasicClassKV[currentBasicClass.value].TABLE : steelBasicClassKV.steelPlateList.TABLE
})

// 可自动分配重量
const weightAssignable = computed(() => {
  const modeFlag = order.value && order.value.weightMeasurementMode === weightMeasurementModeEnum.OVERWEIGHT.V
  const isSpOrSs = !disabledBasicClass.value.steelPlateList || !disabledBasicClass.value.sectionSteelList
  return modeFlag && isSpOrSs
})

// 监听切换钢材类型，为list赋值
watch(
  currentBasicClass,
  (k) => {
    list.value = form[k]
    if (k) {
      nextTick(() => {
        // nextTick 后 steelRef.value 才会发生变化
        if (!steelRefList[k]) steelRefList[k] = steelRef.value
        steelRef.value?.setSelect()
      })
    }
  },
  { immediate: true }
)

// 监听list变更,为对应的钢材清单赋值，监听地址即可
watch(list, (val) => {
  form[currentBasicClass.value] = val
})

// 用于与车的过磅重量比较
watch(
  () => totalWeight.value,
  (val) => {
    cu.props.totalWeight = val
  }
)

// 初始化
init()

FORM.HOOK.beforeToEdit = async (crud, form) => {
  if (!props.edit) return
  // 采购合同id
  form.purchaseId = form.purchaseOrder?.id
  if (!form.logistics) form.logistics = {}
  // 修改的情况下，数据预处理
  await steelInboundFormFormat(form)
  // 设置监听等
  setFormCallback(form)
}

// 提交后清除校验结果
FORM.HOOK.afterSubmit = () => {
  if (props.edit) {
    emit('success')
  }
  init()
}

// 表单校验
function validate() {
  if (boolPartyA.value && isBlank(form.steelPlateList) && isBlank(form.sectionSteelList) && isBlank(form.steelCoilList)) {
    ElMessage.warning('请填写数据')
    return false
  }
  if (!boolPartyA.value && isBlank(formList.value)) {
    ElMessage.warning('请选择数据')
    return false
  }
  const tableValidateRes = validateTable()
  if (tableValidateRes) {
    const _list = []
    formList.value.forEach((v) => {
      if (v.applyPurchase?.length) {
        v.applyPurchase.forEach((a) => {
          if (a.quantity) {
            const _weight = toPrecision((a.quantity / v.quantity) * v.weighingTotalWeight, baseUnit.value[v.basicClass].weight.precision)
            _list.push({
              ...v,
              quantity: a.quantity,
              projectId: a.project?.id,
              uid: createUniqueString(),
              mete: _weight,
              weight: _weight
            })
          }
        })
      } else {
        _list.push({
          ...v,
          uid: createUniqueString(),
          mete: v.weighingTotalWeight,
          weight: v.weighingTotalWeight
        })
      }
    })
    form.list = _list
  }
  // 进入仓库级价格填写页面
  return tableValidateRes
}

// 表格校验
function validateTable() {
  return Object.keys(steelRefList).every((k) => (steelRefList[k] ? steelRefList[k].validate() : true))
}

// 行数据添加时初始化
function rowInit(row) {
  return steelRef.value.rowInit(row)
}

// 过磅重量自动分配(钢卷不参与重量分配)
function automaticAssignWeight() {
  if (!form.loadingWeight) {
    ElMessage.warning('请先输入过磅重量')
    return
  }
  let spList = []
  let ssList = []
  let spAndSsTheoryTotalWeight = 0
  // 遍历钢板与型材列表 计算这两种钢材的总理论重量
  if (isNotBlank(form.steelPlateList)) {
    spList = form.steelPlateList.filter((v) => {
      spAndSsTheoryTotalWeight += v.theoryTotalWeight ? v.theoryTotalWeight : 0
      return v.theoryTotalWeight
    })
  }
  if (isNotBlank(form.sectionSteelList)) {
    ssList = form.sectionSteelList.filter((v) => {
      spAndSsTheoryTotalWeight += v.theoryTotalWeight ? v.theoryTotalWeight : 0
      return v.theoryTotalWeight
    })
  }
  // 可计算钢板及型材数据为空
  if (isBlank(spList) && isBlank(ssList)) {
    ElMessage.warning('您未填写完整钢板或型材的信息, 或它们的理论重量过小')
    return
  }

  // 计算钢卷总重
  let scWeight = 0
  if (isNotBlank(form.steelCoilList)) {
    form.steelCoilList.forEach((v) => {
      scWeight += v.weighingTotalWeight ? v.weighingTotalWeight : 0
    })
  }

  // 可分配重量 = 整车过磅重量 - 钢卷重量 - 钢板及型材的理论重量
  let assignableWeight = form.loadingWeight - scWeight - spAndSsTheoryTotalWeight

  // 理论重量/总理论重量*可分配重量
  // 为0 则 过磅重量 = 理论重量(无需计算)
  const calc = (row) => {
    row.weighingTotalWeight = assignableWeight
      ? toFixed((row.theoryTotalWeight / spAndSsTheoryTotalWeight) * assignableWeight + row.theoryTotalWeight, 2, { toNum: true })
      : row.theoryTotalWeight
    assignableWeight -= row.weighingTotalWeight - row.theoryTotalWeight
    spAndSsTheoryTotalWeight -= row.theoryTotalWeight
  }
  spList.forEach((v) => calc(v))
  ssList.forEach((v) => calc(v))
  ElMessage.warning('已自动分配车次过磅重量')
}

// 订单变化
async function handleOrderInfoChange(orderInfo) {
  init()
  order.value = orderInfo
  cu.props.order = orderInfo
  boolPartyA.value = orderInfo?.supplyType === orderSupplyTypeEnum.PARTY_A.V
  Object.keys(steelBasicClassKV).forEach((k) => {
    if (steelBasicClassKV[k].V & orderInfo?.basicClass) {
      if (!currentBasicClass.value) currentBasicClass.value = steelBasicClassKV[k].K // 为空则赋值
      disabledBasicClass.value[k] = false
    }
    // if (boolPartyA.value || !(steelBasicClassKV[k].V & orderInfo.basicClass)) {
    form[k] = []
    const trigger = watch(
      matSpecRef,
      () => {
        if (matSpecRef.value) {
          matSpecRef.value.clearByBasicClass(steelBasicClassKV[k].V)
          nextTick(() => {
            trigger()
          })
        }
      },
      { immediate: true }
    )
    // }
  })
  if (orderInfo) {
    // 默认赋值
    nextTick(() => {
      steelRefList[currentBasicClass.value] = steelRef.value
    })
    if (!boolPartyA.value && orderInfo?.details?.length) {
      form.list = orderInfo.details || []
      // 修改的情况下，数据预处理
      await steelInboundFormFormat(form)
      // 设置监听等
      setFormCallback(form)
    }
  } else {
    nextTick(() => {
      steelRefList.steelPlateList = null
      steelRefList.sectionSteelList = null
      steelRefList.steelCoilList = null
    })
  }
  orderLoaded.value = true
}

// 信息初始化
function init() {
  disabledBasicClass.value = {
    // 禁用的分类
    steelPlateList: true,
    sectionSteelList: true,
    steelCoilList: true
  }
  currentBasicClass.value = undefined // 当前分类
  // totalWeight.value = 0 // 总重
  orderLoaded.value = false // 订单加载状态
  boolPartyA.value = false // 是否“甲供”
}

// 导入
cu.props.import = (importList) => {
  const key = currentBasicClass.value
  // 先监听，后加入数组会导致监听失效
  // importList.forEach((v) => steelRefList[key].rowWatch(v))
  // 截取新旧数组长度，对导入数据进行rowWatch监听
  const oldLen = form[key].length
  form[key].push.apply(form[key], importList)
  const newLen = form[key].length
  for (let i = oldLen; i < newLen; i++) {
    if (!fillableAmount.value) {
      form[key][i].amount = undefined
      form[key][i].unitPrice = undefined
    }
    steelRefList[key].rowWatch(form[key][i])
  }
  // 初始化选中数据，执行一次后取消当前监听
  const initSelectedTrigger = watch(
    matSpecRef,
    () => {
      if (matSpecRef.value) {
        matSpecRef.value.initSelected(
          importList.map((v) => {
            return { sn: v.sn, classifyId: v.classifyId }
          })
        )
        nextTick(() => {
          initSelectedTrigger()
        })
      }
    },
    { immediate: true }
  )
}
</script>

<style lang="scss" scoped>
.steel-inbound-application-container {
  position: relative;
  .header {
    padding: 20px 20px 10px 20px;
  }
  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
  height: 100%;
}
</style>
