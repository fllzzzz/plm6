<template>
  <div v-permission="permission" class="aux-mat-inbound-application-container">
    <common-wrapper
      :basic-class="currentBasicClass"
      :validate="validate"
      :edit="props.edit"
      :show-total="false"
      :total-amount="totalAmount"
      :show-total-amount="!boolPartyA && fillableAmount"
      @purchase-order-change="handleOrderInfoChange"
    >
      <div class="filter-container">
        <div class="filter-right-box">
          <el-tooltip :disabled="addable" effect="light" content="请先选择采购合同编号" placement="left-start">
            <span>
              <common-button
                v-if="boolPartyA || noDetail"
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
        <aux-mat-table
          ref="tableRef"
          :max-height="tableMaxHeight"
          :bool-party-a="boolPartyA"
          :bool-apply-purchase="boolApplyPurchase"
          :fillableAmount="fillableAmount"
          :basic-class="currentBasicClass"
          :noDetail="noDetail"
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
          v-model="form.auxMatList"
          :visible="materialSelectVisible"
          :row-init-fn="rowInit"
          :max-height="specSelectMaxHeight"
          :basic-class="currentBasicClass"
          :classify-ids="order.otherMaterialIds"
          :table-width="430"
          auto-selected
          expand-query
        />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { otherInboundApplication } from '@/api/wms/material-inbound/raw-material/application'
import { edit as editInboundApplication } from '@/api/wms/material-inbound/raw-material/record'
import { otherInboundApplicationPM as permission } from '@/page-permission/wms'

import { defineProps, defineEmits, ref, watch, provide, nextTick, reactive, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { orderSupplyTypeEnum } from '@/utils/enum/modules/wms'
import { isNotBlank, toFixed } from '@/utils/data-type'
import { createUniqueString } from '@/utils/data-type/string'
import { DP } from '@/settings/config'
import { isBlank } from '@/utils/data-type'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
// import useWmsConfig from '@/composables/store/use-wms-config'
import CommonWrapper from '@/views/wms/material-inbound/raw-material/application/components/common-wrapper.vue'
import MaterialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import AuxMatTable from '../auxiliary-material/module/aux-mat-table.vue'

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

const defaultForm = {
  purchaseId: null, // 申购单id
  loadingWeight: null, // 装载重量
  licensePlate: null, // 车牌号
  shipmentNumber: null, // 物流单号
  logistics: {}, // 物流信息
  list: [] // 入库清单
}

const tableRef = ref() // 表格ref
const matSpecRef = ref() // 规格列表ref
const formRef = ref() // form表单ref
const drawerRef = ref()
const order = ref() // 订单信息
const orderLoaded = ref(false) // 订单加载状态
const boolPartyA = ref(false) // 是否“甲供”
const noDetail = ref(false) // 采购合同是否有明细

const materialSelectVisible = ref(false) // 显示物料选择
const currentBasicClass = matClsEnum.OTHER.V // 当前基础分类

// const { inboundFillWayCfg } = useWmsConfig()
// 显示金额相关信息（由采购填写的信息）
// const fillableAmount = computed(() => inboundFillWayCfg.value ? inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.APPLICATION.V : false)
const fillableAmount = computed(() => false)
// 是否绑定申购
const boolApplyPurchase = computed(() => Boolean(order.value?.applyPurchase?.length)) // 是否绑定申购

const addable = computed(() => !!(currentBasicClass && order.value)) // 可添加的状态（选择了采购合同编号）
const totalAmount = computed(() => {
  let amount = 0
  if (!boolPartyA.value) {
    if (isNotBlank(form.auxMatList)) {
      form.auxMatList.forEach((v) => {
        if (isNotBlank(v.amount)) {
          amount += +v.amount
        }
      })
    }
  }
  return toFixed(amount, DP.YUAN)
})

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

// 使用草稿/修改时，为数据设置监听
const setFormCallback = (form) => {
  form.auxMatList = form.auxMatList?.map((v) => reactive(v))
  const trigger = watch(
    tableRef,
    (ref) => {
      if (ref) {
        nextTick(() => {
          tableRef.value?.setSelect()
        })
        if (!boolPartyA.value) {
          form.auxMatList.forEach((v) => {
            tableRef.value.rowWatch(v)
            if (!boolPartyA.value && !noDetail.value && form.selectObj?.[v.mergeId]?.isSelected) {
              tableRef.value.toggleRowSelection(v, true)
            }
          })
        }
        // 初始化选中数据，执行一次后取消当前监听
        const initSelectedTrigger = watch(
          matSpecRef,
          () => {
            if (matSpecRef.value) {
              matSpecRef.value.initSelected(
                form.auxMatList.map((v) => {
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
        nextTick(() => {
          trigger()
        })
      }
    },
    { immediate: true, deep: true }
  )
  fixMaxHeight()
}

const { cu, form, FORM } = useForm(
  {
    title: '其它入库',
    formStore: !props.edit,
    formStoreKey: 'WMS_INBOUND_APPLICATION_OTHER_MAT',
    permission: permission,
    defaultForm: defaultForm,
    clearDraftCallback: init,
    api: props.edit ? editInboundApplication : otherInboundApplication
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
    mainBox: '.aux-mat-inbound-application-container',
    extraBox: ['.filter-container', '.inbound-application-header', '.inbound-application-footer'],
    navbar: true,
    minHeight: 300,
    extraHeight: 20
  }
}
const { fixMaxHeight, maxHeight: tableMaxHeight } = useMaxHeight(tableHeightConfig)

// 初始化
init()

FORM.HOOK.beforeToEdit = async (crud, form) => {
  if (!props.edit) return
  // 采购合同id
  form.purchaseId = form.purchaseOrder?.id
  if (!form.logistics) form.logistics = {}
  // 设置监听等
  if (boolPartyA.value) {
    form.auxMatList = form.list
    setFormCallback(form)
  }
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
  // 进入仓库级价格填写页面
  const tableValidateRes = tableRef.value ? tableRef.value.validate() : true
  if (tableValidateRes) {
    const _list = []
    form.auxMatList.forEach((v) => {
      if (v.applyPurchase?.length) {
        v.applyPurchase.forEach((a) => {
          if (a.quantity || a.mete) {
            _list.push({
              ...v,
              quantity: a.quantity,
              projectId: a.project?.id,
              uid: createUniqueString(),
              mete: a.mete,
              applyPurchaseId: a.applyPurchaseId,
              purchaseDetailId: a.purchaseDetailId
            })
          }
        })
      } else if (boolPartyA.value || noDetail.value || form.selectObj[v.mergeId]?.isSelected) {
        _list.push(v)
      }
    })
    form.list = _list
  }
  return tableValidateRes
}

// 行数据添加时初始化
function rowInit(row) {
  return tableRef.value.rowInit(row)
}

// 订单变化
async function handleOrderInfoChange(orderInfo) {
  init()
  order.value = orderInfo
  cu.props.order = orderInfo
  boolPartyA.value = orderInfo?.supplyType === orderSupplyTypeEnum.PARTY_A.V
  noDetail.value = isBlank(orderInfo?.details)
  form.auxMatList = []
  const trigger = watch(
    matSpecRef,
    () => {
      if (matSpecRef.value) {
        matSpecRef.value.clearByBasicClass(currentBasicClass)
        nextTick(() => {
          trigger()
        })
      }
    },
    { immediate: true }
  )
  // 筛除当前订单未指定的其它科目
  // if (orderInfo && isNotBlank(orderInfo.otherMaterialIds)) {
  //   const filterList = form.auxMatList.filter((v) => {
  //     for (const cid of orderInfo.otherMaterialIds) {
  //       if (v.classifyFullPathId.includes(cid)) {
  //         return true
  //       }
  //     }
  //     return false
  //   })
  //   form.auxMatList = [...filterList]
  //   const trigger = watch(
  //     matSpecRef,
  //     () => {
  //       if (matSpecRef.value) {
  //         matSpecRef.value.clear()
  //         matSpecRef.value.initSelected(
  //           filterList.map((v) => {
  //             return { sn: v.sn, classifyId: v.classifyId }
  //           })
  //         )
  //         nextTick(() => {
  //           trigger()
  //         })
  //       }
  //     },
  //     { immediate: true }
  //   )
  // }
  if (orderInfo?.details?.length) {
    form.auxMatList = orderInfo.details.map((v) => {
      v.uid = createUniqueString()
      return v
    })
    // 设置监听等
    setFormCallback(form)
  }
  orderLoaded.value = true
}

// 信息初始化
function init() {
  orderLoaded.value = false
  boolPartyA.value = false // 是否“甲供”
}

// 批量导入
cu.props.import = (importList) => {
  if (!fillableAmount.value) {
    importList.forEach((v) => {
      v.amount = undefined
      v.unitPrice = undefined
    })
  }
  let unexistNameArr = []
  // 0代表所有其它
  if (!order.value.otherMaterialIds.includes(0)) {
    importList.forEach((v) => {
      let boolExit = false
      for (const cid of order.value.otherMaterialIds) {
        if (v.classifyFullPathId.includes(cid)) {
          boolExit = true
          break
        }
      }
      if (!boolExit) {
        unexistNameArr.push(v.classifyName)
      }
    })
  }
  if (unexistNameArr.length > 0) {
    unexistNameArr = Array.from(new Set(unexistNameArr))
    throw new Error(`当前订单其它明细中不存在${unexistNameArr.map((v) => `“${v}”`).join('、')}等科目`)
  }
  // 截取新旧数组长度，对导入数据进行rowWatch监听
  form.auxMatList.push.apply(form.auxMatList, importList)
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
