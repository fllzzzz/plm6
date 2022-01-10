<template>
  <div v-permission="permission" class="aux-mat-inbound-application-container">
    <common-wrapper
      :basic-class="currentBasicClass"
      :validate="validate"
      :edit="props.edit"
      :show-total="false"
      @purchase-order-change="handleOrderInfoChange"
    >
      <div class="filter-container">
        <div class="filter-right-box">
          <el-tooltip :disabled="addable" effect="light" content="请先选择采购订单" placement="left-start">
            <span>
              <common-button class="filter-item" type="success" @click="materialSelectVisible = true" :disabled="!addable">
                添加物料
              </common-button>
            </span>
          </el-tooltip>
        </div>
      </div>
      <el-form ref="formRef" :model="form">
        <aux-mat-table ref="tableRef" :max-height="tableMaxHeight" />
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
          :classify-ids="order.auxMaterialIds"
          :table-width="430"
          auto-selected
          expand-query
        />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
// TODO: 编辑，反向赋值
import { auxMatInboundApplication } from '@/api/wms/material-inbound/raw-material/application'
import { edit as editInboundApplication } from '@/api/wms/material-inbound/raw-material/record'
import { defineProps, defineEmits, ref, watch, provide, nextTick, reactive, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'
import CommonWrapper from '@/views/wms/material-inbound/raw-material/application/components/common-wrapper.vue'
import MaterialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import AuxMatTable from './module/aux-mat-table.vue'
import { isNotBlank } from '@/utils/data-type'

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

// 权限
const permission = ['wms_auxMatInboundApplication:submit']

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

const materialSelectVisible = ref(false) // 显示物料选择
const currentBasicClass = matClsEnum.MATERIAL.V // 当前基础分类

const addable = computed(() => !!(currentBasicClass && order.value)) // 可添加的状态（选择了采购订单）

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
    title: '辅材入库',
    formStore: !props.edit,
    formStoreKey: 'WMS_INBOUND_APPLICATION_AUX_MAT',
    permission: permission,
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? editInboundApplication : auxMatInboundApplication
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
  // 采购单id
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
  // 筛除当前订单未指定的辅材科目
  if (orderInfo && isNotBlank(orderInfo.auxMaterialIds)) {
    const filterList = form.list.filter((v) => {
      for (const cid of orderInfo.auxMaterialIds) {
        if (v.classifyFullPathId.includes(cid)) {
          return true
        }
      }
      return false
    })
    const trigger = watch(
      matSpecRef,
      () => {
        if (matSpecRef.value) {
          matSpecRef.value.clear()
          form.list = filterList
          matSpecRef.value.initSelected(
            form.list.map((v) => {
              return { sn: v.sn, classifyId: v.classifyId }
            })
          )
          nextTick(() => {
            trigger()
          })
        }
      },
      { immediate: true }
    )
  }
}

// 信息初始化
function init() {}
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
