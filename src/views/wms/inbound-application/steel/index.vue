<template>
  <div class="steel-inbound-application-container">
    <common-wrapper :basicClass="STEEL_ENUM" @purchase-order-change="handleOrderInfoChange">
      <div class="filter-container">
        <div class="filter-left-box">
          <el-radio-group v-model="currentBasicClass" size="small" class="filter-item">
            <el-radio-button v-for="item in steelBasicClassKV" :key="item.K" :label="item.K" :disabled="disabledBasicClass[item.K]">
              {{ item.L }}{{ form[item.K] && form[item.K].length ? `(${form[item.K].length})` : '' }}
            </el-radio-button>
          </el-radio-group>
        </div>
        <div class="filter-right-box">
          <common-button class="filter-item" type="success" @click="materialSelectVisible = true" :disabled="!currentBasicClass">
            添加物料
          </common-button>
        </div>
      </div>
      <el-form ref="formRef" :model="form" size="small" label-position="right" inline label-width="80px">
        <component ref="steelRef" :max-height="tableMaxHeight" :is="comp" :key="Math.random()" />
        <!-- <steel-plate-table ref="steelRef" :max-height="tableMaxHeight" /> -->
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
          v-if="currentBasicClass"
          ref="matSpecRef"
          v-model="list"
          :row-init-fn="rowInit"
          :max-height="specSelectMaxHeight"
          :basic-class="steelBasicClassKV[currentBasicClass].V"
          :table-width="350"
        />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
// TODO: 编辑，反向赋值
import { ref, computed, watch, provide, nextTick } from 'vue'
import { STEEL_ENUM } from '@/settings/config'
import { matClsEnum } from '@/utils/enum/modules/classification'

import useForm from '@/composables/form/use-form'
import useMaxHeight from '@compos/use-max-height'

import commonWrapper from '../components/common-wrapper.vue'
import materialTableSpecSelect from '@/components-system/classification/material-table-spec-select.vue'
import steelPlateTable from './module/steel-plate-table.vue'
import sectionSteelTable from './module/section-steel-table.vue'
import steelCoilTable from './module/steel-coil-table.vue'
import { ElRadioGroup } from 'element-plus'

// 权限
const permission = ['wms_steelInboundApplication:get']

// 基础分类
const steelBasicClassKV = {
  steelPlateList: { K: 'steelPlateList', L: '钢板', V: matClsEnum.STEEL_PLATE.V, TABLE: steelPlateTable },
  sectionSteelList: { K: 'sectionSteelList', L: '型钢', V: matClsEnum.SECTION_STEEL.V, TABLE: sectionSteelTable },
  steelCoilList: { K: 'steelCoilList', L: '钢卷', V: matClsEnum.STEEL_COIL.V, TABLE: steelCoilTable }
}

const defaultForm = {
  purchaseId: null, // 申购单id
  loadingWeight: null, // 装载重量
  licensePlate: null, // 车牌号
  steelPlateList: [], // 钢板列表
  sectionSteelList: [], // 型钢列表
  steelCoilList: [] // 钢卷列表
}

const steelRef = ref() // 表格ref
const matSpecRef = ref() // 规格列表ref
const formRef = ref() // form表单ref
const drawerRef = ref()
const order = ref() // 订单信息
const disabledBasicClass = ref() // 禁用的基础分类
const materialSelectVisible = ref(false) // 显示物料选择
const currentBasicClass = ref() // 当前基础分类
const list = ref([]) // 当前操作的表格list
const steelRefList = { // 钢材三个组件的ref列表
  steelPlateList: undefined,
  sectionSteelList: undefined,
  steelCoilList: undefined
}
provide('matSpecRef', matSpecRef)

setTimeout(() => {
  materialSelectVisible.value = true
}, 500)

const { form } = useForm(
  {
    title: '钢材入库',
    formStore: true,
    formStoreKey: 'WMS_INBOUND_APPLICATION_STEEL',
    permission: permission,
    defaultForm: defaultForm,
    crudApi: ''
  },
  formRef
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

// 表格高度
const { maxHeight: tableMaxHeight } = useMaxHeight({
  mainBox: '.steel-inbound-application-container',
  extraBox: ['.filter-container', '.inbound-application-header', '.inbound-application-footer'],
  navbar: true,
  minHeight: 300,
  extraHeight: 20
})

// 当前使用的组件
const comp = computed(() => {
  return currentBasicClass.value ? steelBasicClassKV[currentBasicClass.value].TABLE : steelBasicClassKV.steelPlateList.TABLE
})

// 监听切换钢材类型，为list赋值
watch(
  currentBasicClass,
  (k) => {
    list.value = form[k]
    nextTick(() => {
      // nextTick 后 steelRef.value 才会发生变化
      if (!steelRefList[k]) steelRefList[k] = steelRef.value
    })
  },
  { immediate: true }
)

// 监听list变更,为对应的钢材清单赋值，监听地址即可
watch(list, (val) => {
  form[currentBasicClass] = val
})

// 初始化
init()

// 行初始化
function rowInit(row) {
  return steelRef.value.rowInit(row)
}
function print() {
  Object.keys(steelRefList).forEach((k) => {
    console.log(k, steelRefList[k] && steelRefList[k].co)
  })
}

// 订单变化
function handleOrderInfoChange(orderInfo) {
  init()
  order.value = orderInfo
  if (orderInfo) {
    Object.keys(steelBasicClassKV).forEach((k) => {
      if (steelBasicClassKV[k].V & orderInfo.basicClass) {
        if (!currentBasicClass.value) currentBasicClass.value = steelBasicClassKV[k].K
        disabledBasicClass.value[k] = false
      }
    })
  }
  // 默认赋值
  steelRefList[currentBasicClass.value] = steelRef.value
}

// 信息初始化
function init() {
  disabledBasicClass.value = {
    // 禁用的分类
    steelPlateList: true,
    sectionSteelList: true,
    steelCoilList: true
  }
  currentBasicClass.value = undefined
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
