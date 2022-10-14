<template>
  <div v-permission="permission" class="gas-inbound-application-container">
    <common-wrapper
      :basic-class="currentBasicClass"
      :validate="validate"
      :edit="props.edit"
      :show-total="false"
      @purchase-order-change="handleOrderInfoChange"
    >
      <div class="filter-container">
        <div class="filter-right-box">
          <el-tooltip :disabled="addable" effect="light" content="请先选择采购合同编号" placement="left-start">
            <span>
              <common-button class="filter-item" type="success" @click="materialSelectVisible = true" :disabled="!addable">
                添加物料
              </common-button>
            </span>
          </el-tooltip>
        </div>
      </div>
      <el-form ref="formRef" :model="form">
        <gas-table ref="tableRef" :max-height="tableMaxHeight" />
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
          v-model="form.list"
          :visible="materialSelectVisible"
          :row-init-fn="rowInit"
          :max-height="specSelectMaxHeight"
          :basic-class="currentBasicClass"
          :table-width="350"
          auto-selected
          expand-query
        />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import { gasInboundApplication } from '@/api/wms/material-inbound/raw-material/application'
import { edit as editInboundApplication } from '@/api/wms/material-inbound/raw-material/record'
import { gasInboundApplicationPM as permission } from '@/page-permission/wms'

import { defineProps, defineEmits, ref, watch, provide, nextTick, reactive, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import commonWrapper from '@/views/wms/material-inbound/raw-material/application/components/common-wrapper.vue'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import gasTable from './module/gas-table.vue'

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
  purchaseId: null, // 采购合同id
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

const materialSelectVisible = ref(false) // 显示物料选择
const currentBasicClass = matClsEnum.GAS.V // 当前基础分类

const addable = computed(() => !!(currentBasicClass && order.value)) // 可添加的状态（选择了采购合同编号）

provide('matSpecRef', matSpecRef) // 供兄弟组件调用 删除

// 使用草稿/修改时，为数据设置监听
const setFormCallback = (form) => {
  form.list = form.list.map((v) => reactive(v))
  const trigger = watch(
    tableRef,
    (ref) => {
      if (ref) {
        // 初始化选中数据，执行一次后取消当前监听
        const initSelectedTrigger = watch(
          matSpecRef,
          () => {
            if (matSpecRef.value) {
              matSpecRef.value.initSelected(
                form.list.map((v) => {
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
    title: '气体入库',
    formStore: !props.edit,
    formStoreKey: 'WMS_INBOUND_APPLICATION_GAS',
    permission: permission,
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? editInboundApplication : gasInboundApplication
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
    mainBox: '.gas-inbound-application-container',
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
  form.purchaseId = form.purchaseOrder.id
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
  // 进入仓库级价格填写页面
  return tableRef.value ? tableRef.value.validate() : true
}

// 行数据添加时初始化
function rowInit(row) {
  return tableRef.value.rowInit(row)
}

// 订单变化
function handleOrderInfoChange(orderInfo) {
  init()
  order.value = orderInfo
  cu.props.order = orderInfo
  orderLoaded.value = true
}

// 信息初始化
function init() {
  orderLoaded.value = false
}

// 批量导入
cu.props.import = (importList) => {
  // 截取新旧数组长度，对导入数据进行rowWatch监听
  form.list.push.apply(form.list, importList)
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
