<template>
  <div v-permission="permission" class="manuf-inbound-application-container">
    <common-wrapper
      :basicClass="manufEnum"
      :current-basic-class="manufBasicClassKV[currentBasicClass]?.V"
      :total-value="totalWeight"
      :total-amount="totalAmount"
      :show-total-amount="!boolPartyA && fillableAmount"
      :validate="validate"
      :edit="props.edit"
      is-manuf
      unit="kg"
      total-name="总量合计"
      @purchase-order-change="handleOrderInfoChange"
    >
      <div class="filter-container">
        <div class="filter-left-box">
          <el-radio-group v-model="currentBasicClass" size="small" class="filter-item">
            <el-radio-button v-for="item in manufBasicClassKV" :key="item.K" :label="item.K" :disabled="disabledBasicClass[item.K]">
              {{ item.L }}{{ getNum(item.K) ? `(${getNum(item.K)})` : '' }}
            </el-radio-button>
          </el-radio-group>
        </div>
        <div class="filter-right-box"></div>
      </div>
      <el-form ref="formRef" :model="form">
        <component
          ref="manufRef"
          :max-height="tableMaxHeight"
          :style="maxHeightStyle"
          :is="comp"
          :bool-party-a="boolPartyA"
          :fillableAmount="fillableAmount"
        />
      </el-form>
    </common-wrapper>
  </div>
</template>

<script setup>
import { steelInboundApplication } from '@/api/wms/material-inbound/raw-material/application'
import { edit as editInboundApplication } from '@/api/wms/material-inbound/raw-material/record'
import { steelInboundApplicationPM as permission } from '@/page-permission/wms'

import { defineProps, defineEmits, ref, computed, watch, provide, nextTick, reactive } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { orderSupplyTypeEnum } from '@/utils/enum/modules/wms'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
// import useWmsConfig from '@/composables/store/use-wms-config'
import commonWrapper from '@/views/wms/material-inbound/raw-material/application/components/common-wrapper.vue'
import strucManufTable from './module/struc-manuf-table.vue'
import enclManufTable from './module/encl-manuf-table.vue'
import { ElMessage, ElRadioGroup } from 'element-plus'
import { isBlank, isNotBlank, toFixed } from '@/utils/data-type'

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

const manufEnum = matClsEnum.STRUC_MANUFACTURED.V | matClsEnum.ENCL_MANUFACTURED.V

// 基础分类
const manufBasicClassKV = {
  strucManufList: { K: 'strucManufList', L: matClsEnum.STRUC_MANUFACTURED.L, V: matClsEnum.STRUC_MANUFACTURED.V, TABLE: strucManufTable },
  enclManufList: { K: 'enclManufList', L: matClsEnum.ENCL_MANUFACTURED.L, V: matClsEnum.ENCL_MANUFACTURED.V, TABLE: enclManufTable }
}

const defaultForm = {
  purchaseId: null, // 采购合同id
  licensePlate: null, // 车牌号
  shipmentNumber: null, // 物流单号
  logistics: {}, // 物流信息
  list: [], // 钢材列表，提交时合并
  strucManufList: [], // 钢板列表
  enclManufList: [] // 型材列表
}

const manufRef = ref() // 表格ref
const matSpecRef = ref() // 规格列表ref
const formRef = ref() // form表单ref
const order = ref() // 订单信息
const orderLoaded = ref(false) // 订单加载状态
const disabledBasicClass = ref({}) // 禁用的基础分类
const currentBasicClass = ref() // 当前基础分类
const list = ref([]) // 当前操作的表格list
const boolPartyA = ref(false) // 是否“甲供”

// 钢材三个组件的ref列表
const manufRefList = reactive({
  strucManufList: null,
  enclManufList: null
})

// const { inboundFillWayCfg } = useWmsConfig()
// 显示金额相关信息（由采购填写的信息）
const fillableAmount = computed(
  () => false
  // inboundFillWayCfg.value ? inboundFillWayCfg.value.amountFillWay === inboundFillWayEnum.APPLICATION.V : false
)

function getNum(key) {
  return form[key]?.filter((v) => form.selectObj?.[v.id])?.length
}

// 列表汇总数据
const formList = computed(() => {
  const list = []
  if (isNotBlank(form.strucManufList)) {
    form.strucManufList.forEach((v) => {
      if (boolPartyA.value || form.selectObj[v.id]) {
        list.push(v)
      }
    })
  }
  if (isNotBlank(form.enclManufList)) {
    form.enclManufList.forEach((v) => {
      if (boolPartyA.value || form.selectObj[v.id]) {
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
    if (isNotBlank(v.mete)) {
      weight += +v.mete
    }
  })
  return toFixed(weight, 2)
})

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

// 使用草稿时，为数据设置监听
const setFormCallback = (form) => {
  const trigger = {
    strucManufList: null,
    enclManufList: null
  }
  const list = ['strucManufList', 'enclManufList']
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
        manufRefList,
        (ref) => {
          if (ref[key]) {
            // 初始化数据监听，执行一次后取消当前监听
            form[key].forEach((v) => ref[key].rowWatch(v))
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
    title: '制成品入库',
    formStore: !props.edit,
    formStoreKey: 'WMS_INBOUND_APPLICATION_MANUF',
    permission: permission,
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? editInboundApplication : steelInboundApplication
  },
  formRef,
  props.detail
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
    mainBox: '.manuf-inbound-application-container',
    extraBox: ['.filter-container', '.inbound-application-header', '.inbound-application-footer'],
    navbar: true,
    minHeight: 300,
    extraHeight: 20
  }
}
const { fixMaxHeight, maxHeight: tableMaxHeight, maxHeightStyle } = useMaxHeight(tableHeightConfig)

// 当前使用的组件
const comp = computed(() => {
  return currentBasicClass.value ? manufBasicClassKV[currentBasicClass.value].TABLE : manufBasicClassKV.strucManufList.TABLE
})

// 监听切换钢材类型，为list赋值
watch(
  currentBasicClass,
  (k) => {
    list.value = form[k]
    if (k) {
      nextTick(() => {
        // nextTick 后 manufRef.value 才会发生变化
        if (!manufRefList[k]) manufRefList[k] = manufRef.value
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
  if (boolPartyA.value && isBlank(form.strucManufList) && isBlank(form.enclManufList)) {
    ElMessage.warning('请填写数据')
    return false
  }
  if (!boolPartyA.value && isBlank(formList.value)) {
    ElMessage.warning('请选择数据')
    return false
  }
  const tableValidateRes = validateTable()
  if (tableValidateRes) {
    form.list = formList.value
  }
  // 进入仓库级价格填写页面
  return tableValidateRes
}

// 表格校验
function validateTable() {
  return Object.keys(manufRefList).every((k) => (manufRefList[k] ? manufRefList[k].validate() : true))
}

// 订单变化
async function handleOrderInfoChange(orderInfo) {
  init()
  order.value = orderInfo
  cu.props.order = orderInfo
  boolPartyA.value = orderInfo?.supplyType === orderSupplyTypeEnum.PARTY_A.V
  if (orderInfo) {
    Object.keys(manufBasicClassKV).forEach((k) => {
      if (manufBasicClassKV[k].V & orderInfo.basicClass) {
        if (!currentBasicClass.value) currentBasicClass.value = manufBasicClassKV[k].K // 为空则赋值
        disabledBasicClass.value[k] = false
      }
      if (boolPartyA.value || !(manufBasicClassKV[k].V & orderInfo.basicClass)) {
        form[k] = []
      }
    })
    // 默认赋值
    nextTick(() => {
      manufRefList[currentBasicClass.value] = manufRef.value
    })
    if (orderInfo?.details?.length) {
      form.list = orderInfo.details
      form.strucManufList = []
      form.enclManufList = []
      for (const row of form.list) {
        switch (row.basicClass) {
          case matClsEnum.STRUC_MANUFACTURED.V:
            form.strucManufList.push(row)
            break
          case matClsEnum.ENCL_MANUFACTURED.V:
            form.enclManufList.push(row)
            break
        }
      }
      // 设置监听等
      setFormCallback(form)
    }
  } else {
    nextTick(() => {
      manufRefList.strucManufList = null
      manufRefList.enclManufList = null
    })
  }
  orderLoaded.value = true
}

// 信息初始化
function init() {
  disabledBasicClass.value = {
    // 禁用的分类
    strucManufList: true,
    enclManufList: true
  }
  currentBasicClass.value = undefined // 当前分类
  totalWeight.value = 0 // 总重
  orderLoaded.value = false // 订单加载状态
  boolPartyA.value = false // 是否“甲供”
}
</script>

<style lang="scss" scoped>
.manuf-inbound-application-container {
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
